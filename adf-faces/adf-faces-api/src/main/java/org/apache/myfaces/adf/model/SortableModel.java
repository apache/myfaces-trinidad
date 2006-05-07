/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adf.model;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.faces.context.FacesContext;
import javax.faces.el.PropertyResolver;
import javax.faces.model.DataModel;

import org.apache.myfaces.adf.logging.ADFLogger;


/**
 * Creates a CollectionModel that is sortable.
 * All properties that implement java.lang.Comparable are deemed sortable.
 * @author The Oracle ADF Faces Team
 */
public final class SortableModel extends CollectionModel
{
  /**
   * Create a new SortableModel from the given instance.
   * @param model This will be converted into a {@link DataModel}
   * @see #setWrappedData
   */
  public SortableModel(Object model)
  {
    setWrappedData(model);
  }

  /**
   * No arg constructor for use as a managed-bean.
   * Must call setWrappedData before using this instance.
   */
  public SortableModel()
  {
  }

  public Object getRowData()
  {
    return _model.getRowData();
  }

  public Object getWrappedData()
  {
    return _wrappedData;
  }

  public boolean isRowAvailable()
  {
    return _model.isRowAvailable();
  }

  /**
   * Sets the underlying data being managed by this instance.
   * @param data This Object will be converted into a
   * {@link DataModel}.
   * @see ModelUtils#toDataModel
   */
  public void setWrappedData(Object data)
  {
    _baseIndicesList = null;
    _model = ModelUtils.toDataModel(data);
    _sortCriterion = null;
    _sortedIndicesList = null;
    _wrappedData = data;
  }

  public int getRowCount()
  {
    return _model.getRowCount();
  }

  public void setRowIndex(int rowIndex)
  {
    int baseIndex = _toBaseIndex(rowIndex);
    _model.setRowIndex(baseIndex);
  }

  public int getRowIndex()
  {
    int baseIndex = _model.getRowIndex();
    return _toSortedIndex(baseIndex);
  }

  /**
   * Gets the row key of the current row
   * @inheritDoc
   */
  public Object getRowKey()
  {
    return isRowAvailable()
      ? String.valueOf(getRowIndex())
      : null;
  }

  /**
   * Finds the row with the matching key and makes it current
   * @inheritDoc
   */
  public void setRowKey(Object key)
  {
    setRowIndex(_toRowIndex((String) key));
  }

  /**
   * Checks to see if the underlying collection is sortable by the given property.
   * @param property The name of the property to sort the underlying collection by.
   * @return true, if the property implements java.lang.Comparable
   */
  public boolean isSortable(String property)
  {
    final int oldIndex = _model.getRowIndex();
    try
    {
      _model.setRowIndex(0);
      if (!_model.isRowAvailable())
        return false; // if there is no data in the table then nothing is sortable

      Object data = _model.getRowData();
      try
      {
        FacesContext context = FacesContext.getCurrentInstance();
        PropertyResolver resolver = context.getApplication().getPropertyResolver();
        Object propertyValue = resolver.getValue(data, property);

        // when the value is null, we don't know if we can sort it.
        // by default let's support sorting of null values, and let the user
        // turn off sorting if necessary:
        return (propertyValue instanceof Comparable) ||
          (propertyValue == null);
      }
      catch (RuntimeException e)
      {
        // don't propagate this exception out. This is because it might break
        // the VE.
        _LOG.warning(e);
        return false;
      }
    }
    finally
    {
      _model.setRowIndex(oldIndex);
    }
  }

  public List getSortCriteria()
  {
    return (_sortCriterion == null)
      ? Collections.EMPTY_LIST
      : Collections.singletonList(_sortCriterion);
  }

  public void setSortCriteria(List criteria)
  {
    if ((criteria == null) || (criteria.isEmpty()))
    {
      _sortCriterion = null;
      // restore unsorted order:
      _baseIndicesList = _sortedIndicesList = null;
    }
    else
    {
      SortCriterion sc = (SortCriterion) criteria.get(0);
      if ((_sortCriterion == null) || (!_sortCriterion.equals(sc)))
      {
        _sortCriterion = sc;
        _sort(_sortCriterion.getProperty(), _sortCriterion.isAscending());
      }
    }
  }

  public String toString()
  {
    return "SortableModel[" + _model + "]";
  }

  /**
   * Sorts the underlying collection by the given property, in the
   * given direction.
   * @param property The name of the property to sort by. The value of this
   * property must implement java.lang.Comparable.
   * @param isAscending true if the collection is to be sorted in
   * ascending order.
   * @todo support -1 for rowCount
   */
  private void _sort(String property, boolean isAscending)
  {
//    if (property.equals(_sortBy) && (isAscending == _sortOrder))
//    {
//      return;
//    }
//
//    _sortBy = property;
//    _sortOrder = isAscending;

      //TODO: support -1 for rowCount:
    int sz = getRowCount();
    if ((_baseIndicesList == null) || (_baseIndicesList.size() != sz))
    {
      // we do not want to mutate the original data.
      // however, instead of copying the data and sorting the copy,
      // we will create a list of indices into the original data, and
      // sort the indices. This way, when certain rows are made current
      // in this Collection, we can make them current in the underlying
      // DataModel as well.

      _baseIndicesList = new IntList(sz);
    }

    final int rowIndex = _model.getRowIndex();
    _model.setRowIndex(0);
    // Make sure the model has that row 0! (It could be empty.)
    if (_model.isRowAvailable())
    {
      FacesContext context = FacesContext.getCurrentInstance();
      Comparator comp =
        new Comp(context. getApplication().getPropertyResolver(), property);
      if (!isAscending)
        comp = new Inverter(comp);

      Collections.sort(_baseIndicesList, comp);
      _sortedIndicesList = null;
    }

    _model.setRowIndex(rowIndex);
  }

  private int _toSortedIndex(int baseIndex)
  {
    if ((_sortedIndicesList == null) && (_baseIndicesList != null))
    {
      _sortedIndicesList = (IntList) _baseIndicesList.clone();
      for(int i=0; i<_baseIndicesList.size(); i++)
      {
        Integer base = (Integer) _baseIndicesList.get(i);
        _sortedIndicesList.set(base.intValue(), new Integer(i));
      }
    }

    return _convertIndex(baseIndex, _sortedIndicesList);
  }

  private int _toBaseIndex(int sortedIndex)
  {
    return _convertIndex(sortedIndex, _baseIndicesList);
  }

  private int _convertIndex(int index, List indices)
  {
    if (index < 0) // -1 is special
      return index;

    if ((indices != null) && (indices.size() > index))
    {
      index = ((Integer) indices.get(index)).intValue();
    }
    return index;
  }

  private int _toRowIndex(String rowKey)
  {
    if (rowKey == null)
      return -1;

    try
    {
      return Integer.parseInt(rowKey);
    }
    catch (NumberFormatException nfe)
    {
      _LOG.warning("Illegal rowKey:" + rowKey, nfe);
      return -1;
    }
  }


  private static final class IntList extends ArrayList implements Cloneable
  {
    public IntList(int size)
    {
      super(size);
      _expandToSize(size);
    }

    private void _expandToSize(int desiredSize)
    {
      for(int i=0; i<desiredSize; i++)
      {
        add(new Integer(i));
      }
    }
  }

  private final class Comp implements Comparator
  {
    public Comp(PropertyResolver resolver, String property)
    {
      _resolver = resolver;
      _prop = property;
    }

    public int compare(Object o1, Object o2)
    {
      int index1 = ((Integer) o1).intValue();
      int index2 = ((Integer) o2).intValue();

      _model.setRowIndex(index1);
      Object instance1 = _model.getRowData();
      Object value1 = _resolver.getValue(instance1, _prop);

      _model.setRowIndex(index2);
      Object instance2 = _model.getRowData();
      Object value2 = _resolver.getValue(instance2, _prop);

      if (value1 == null)
        return (value2 == null) ? 0 : -1;

      if (value2 == null)
        return 1;

      // bug 4545164. Sometimes, isSortable returns true
      // even if the underlying object is not a Comparable.
      // This happens if the object at rowIndex zero is null.
      // So test before we cast:
      if (value1 instanceof Comparable)
      {
        return ((Comparable) value1).compareTo(value2);
      }
      else
      {
        // if the object is not a Comparable, then
        // the best we can do is string comparison:
        return value1.toString().compareTo(value2.toString());
      }
    }

    private final PropertyResolver _resolver;
    private final String _prop;
  }

  private static final class Inverter implements Comparator
  {
    public Inverter(Comparator comp)
    {
      _comp = comp;
    }

    public int compare(Object o1, Object o2)
    {
      return _comp.compare(o2, o1);
    }

    private final Comparator _comp;
  }

  private SortCriterion _sortCriterion = null;

  private DataModel _model = null;
  private Object _wrappedData = null;

  private IntList _sortedIndicesList = null, // from baseIndex to sortedIndex
    _baseIndicesList = null; // from sortedIndex to baseIndex

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(SortableModel.class);
}
