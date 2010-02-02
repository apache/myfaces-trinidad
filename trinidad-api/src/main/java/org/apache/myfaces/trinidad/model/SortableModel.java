/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.model;

import java.text.Collator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

import javax.faces.FactoryFinder;
import javax.faces.application.ApplicationFactory;
import javax.faces.context.FacesContext;
import javax.faces.el.PropertyResolver;
import javax.faces.model.DataModel;
import javax.faces.model.DataModelListener;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Creates a CollectionModel that is sortable.
 * All properties that implement java.lang.Comparable are deemed sortable.
 */
public class SortableModel extends CollectionModel
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

  @Override
  public Object getRowData()
  {
    return _model.getRowData();
  }

  @Override
  public Object getWrappedData()
  {
    return _wrappedData;
  }

  @Override
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
  @Override
  public void setWrappedData(Object data)
  {
    _baseIndicesList = null;
    _model = ModelUtils.toDataModel(data);
    _sortCriterion = null;
    _sortedIndicesList = null;
    _wrappedData = data;
  }

  @Override
  public int getRowCount()
  {
    return _model.getRowCount();
  }

  @Override
  public void setRowIndex(int rowIndex)
  {
    int baseIndex = _toBaseIndex(rowIndex);
    _model.setRowIndex(baseIndex);
  }

  @Override
  public int getRowIndex()
  {
    int baseIndex = _model.getRowIndex();
    return _toSortedIndex(baseIndex);
  }

  /**
   * Gets the row key of the current row
   * @inheritDoc
   */
  @Override
  public Object getRowKey()
  {
    return isRowAvailable()
      ? _model.getRowIndex()
      : null;
  }

  /**
   * Finds the row with the matching key and makes it current
   * @inheritDoc
   */
  @Override
  public void setRowKey(Object key)
  {
    _model.setRowIndex(_toRowIndex(key));
  }

  public void addDataModelListener(DataModelListener listener)
  {
    _model.addDataModelListener(listener);
  }

  public DataModelListener[] getDataModelListeners()
  {
    return _model.getDataModelListeners();
  }

  public void removeDataModelListener(DataModelListener listener)
  {
    _model.removeDataModelListener(listener);
  }

  /**
   * Checks to see if the underlying collection is sortable by the given property.
   * @param property The name of the property to sort the underlying collection by.
   * @return true, if the property implements java.lang.Comparable
   */
  @Override
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
        PropertyResolver resolver = __getPropertyResolver();
        Object propertyValue = evaluateProperty(resolver, data, property);

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

  private Object evaluateProperty(PropertyResolver resolver, Object base, String property)
  {
    //simple property -> resolve value directly
    if (!property.contains( "." ))
      return resolver.getValue( base, property );
    
    int index = property.indexOf( '.' );
    Object newBase = resolver.getValue( base, property.substring( 0, index ) );

    return evaluateProperty( resolver, newBase, property.substring( index + 1 ) );
  }

  @Override
  public List<SortCriterion> getSortCriteria()
  {
    if (_sortCriterion == null)
    {
      return Collections.emptyList();
    }
    else
    {
      return Collections.singletonList(_sortCriterion);
    }
  }

  @Override
  public void setSortCriteria(List<SortCriterion> criteria)
  {
    if ((criteria == null) || (criteria.isEmpty()))
    {
      _sortCriterion = null;
      // restore unsorted order:
      _baseIndicesList = _sortedIndicesList = null;
    }
    else
    {
      SortCriterion sc = criteria.get(0);
      if ((_sortCriterion == null) || (!_sortCriterion.equals(sc)))
      {
        _sortCriterion = sc;
        _sort(_sortCriterion.getProperty(), _sortCriterion.isAscending());
      }
    }
  }

  @Override
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
      Comparator<Integer> comp =
        new Comp(__getPropertyResolver(), __getLocale(), property);
      if (!isAscending)
        comp = new Inverter<Integer>(comp);

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
        Integer base = _baseIndicesList.get(i);
        _sortedIndicesList.set(base.intValue(), i);
      }
    }

    return _convertIndex(baseIndex, _sortedIndicesList);
  }

  private int _toBaseIndex(int sortedIndex)
  {
    return _convertIndex(sortedIndex, _baseIndicesList);
  }

  private int _convertIndex(int index, List<Integer> indices)
  {
    if (index < 0) // -1 is special
      return index;

    if ((indices != null) && (indices.size() > index))
    {
      index = indices.get(index).intValue();
    }
    return index;
  }

  private int _toRowIndex(Object rowKey)
  {
    if (rowKey == null)
      return -1;

    try
    {
      return ((Integer)rowKey).intValue();
    }
    catch (ClassCastException e)
    {
      _LOG.warning("INVALID_ROWKEY", new Object[]{rowKey , rowKey.getClass()});
      _LOG.warning(e);
      return -1;
    }
  }


  
  private static final class IntList extends ArrayList<Integer>
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
        add(i);
      }
    }

    private static final long serialVersionUID = 1L;
  }

  private final class Comp implements Comparator<Integer>
  {
    public Comp(PropertyResolver resolver, Locale locale, String property)
    {
      _resolver = resolver;
      _collator = Collator.getInstance(locale);
      _prop = property;
    }

    @SuppressWarnings("unchecked")
    public int compare(Integer o1, Integer o2)
    {
      int index1 = o1.intValue();
      int index2 = o2.intValue();

      _model.setRowIndex(index1);
      Object instance1 = _model.getRowData();
      Object value1 = evaluateProperty( _resolver, instance1, _prop );

      _model.setRowIndex(index2);
      Object instance2 = _model.getRowData();
      Object value2 = evaluateProperty( _resolver, instance2, _prop );

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
        if ((value1 instanceof String) && (value2 instanceof String)) 
        {
          return compare((String) value1, (String) value2);
        }
        else 
        {
          return ((Comparable<Object>) value1).compareTo(value2);
        }
      }
      else
      {
        // if the object is not a Comparable, then
        // the best we can do is string comparison:
        return compare(value1.toString(), value2.toString());
      }
    }
    
    private int compare(String s1, String s2) 
    {
      return _collator.compare(s1, s2);
    }

    private final PropertyResolver _resolver;
    private final String _prop;
    private final Collator _collator;
  }

  private static final class Inverter<T> implements Comparator<T>
  {
    public Inverter(Comparator<T> comp)
    {
      _comp = comp;
    }

    public int compare(T o1, T o2)
    {
      return _comp.compare(o2, o1);
    }

    private final Comparator<T> _comp;
  }
  
  static Locale __getLocale() 
  {
    Locale locale = null;
    RequestContext requestContext = RequestContext.getCurrentInstance();  
    
    if (requestContext != null)
    {
      locale = requestContext.getFormattingLocale();
    }
    
    if (locale == null)
    {
      FacesContext facesContext = FacesContext.getCurrentInstance();
      locale = facesContext.getViewRoot().getLocale();
    }
    
    return locale;
  }

  static PropertyResolver __getPropertyResolver()
  {
    // First try the FacesContext, which is a faster way to
    // get the PropertyResolver (and the 99.9% scenario)
    FacesContext context = FacesContext.getCurrentInstance();
    if (context != null)
      return context.getApplication().getPropertyResolver();
    
    // If that fails, then we're likely outside of the JSF lifecycle.
    // Look to the ApplicationFactory.
    ApplicationFactory factory = (ApplicationFactory)
      FactoryFinder.getFactory(FactoryFinder.APPLICATION_FACTORY);
    return factory.getApplication().getPropertyResolver();
    
  }

  private SortCriterion _sortCriterion = null;

  private DataModel _model = null;
  private Object _wrappedData = null;

  private IntList _sortedIndicesList = null, // from baseIndex to sortedIndex
    _baseIndicesList = null; // from sortedIndex to baseIndex

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SortableModel.class);
}
