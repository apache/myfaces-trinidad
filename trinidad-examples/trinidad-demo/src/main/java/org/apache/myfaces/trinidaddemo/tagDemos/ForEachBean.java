/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidaddemo.tagDemos;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;

import org.apache.commons.lang.StringUtils;
import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ReorderChildrenComponentChange;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.model.RowKeyPropertyModel;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.model.SortableModel;


public class ForEachBean
  implements Serializable
{
  public static class Person
    implements Serializable
  {
    public Person(
      String key,
      String firstName,
      String lastName)
    {
      _firstName = firstName;
      _lastName = lastName;
      _key = key;
    }

    public final String getKey()
    {
      return _key;
    }

    public final String getFirstName()
    {
      return _firstName;
    }

    public final String getLastName()
    {
      return _lastName;
    }

    @Override
    public String toString()
    {
      return ("Person[(" + _key + ") " + _lastName + ", " + _firstName + "]");
    }

    private final String _key;
    private final String _firstName;
    private final String _lastName;

    @SuppressWarnings("compatibility:-1476456984616362784")
    private static final long serialVersionUID = 1L;
  }

  public static class UpdatableItem
    implements Serializable
  {
    public UpdatableItem(
      String key,
      String value)
    {
      _key = key;
      _value = value;
    }

    public final String getKey()
    {
      return _key;
    }

    public final void setValue(String value)
    {
      _value = value;
    }

    public final String getValue()
    {
      return _value;
    }

    private final String _key;
    private String _value;

    @SuppressWarnings("compatibility:-1687851812834776335")
    private static final long serialVersionUID = 1L;
  }

  public ForEachBean()
  {
    _list = new ArrayList<Person>(
      Arrays.asList(
        new Person("a", "John", "Doe"),
        new Person("b", "Jane", "Doe"),
        new Person("c", "Bob", "Smith"),
        new Person("d", "Alice", "Jones")));

    _model = new RowKeyPropertyModel(new ArrayList<Person>(_list), "key");
    _simpleList = new ArrayList<Person>(_list);
    _map = new LinkedHashMap<String, Person>();
    _applySortToNonCollectionModelObjects();
    _arrangeMap = new LinkedHashMap<String, Person>(_map);

    _updatableItemMap = new LinkedHashMap<String, UpdatableItem>();
    for (int i = 1; i <= 5; ++i)
    {
      String key = Integer.toString(i);
      String value = String.format("Item %d", i);
      _updatableItemMap.put(key, new UpdatableItem(key, value));
    }
  }

  public final void setCurrentExample(String currentExample)
  {
    _currentExample = currentExample;
  }

  public final String getCurrentExample()
  {
    return _currentExample;
  }

  public final void setSortProperty(String sortProperty)
  {
    if ("".equals(sortProperty))
    {
      sortProperty = null;
    }
    _sortProperty = sortProperty;
  }

  public final String getSortProperty()
  {
    return _sortProperty;
  }

  public final void setSortAscending(boolean sortAscending)
  {
    _sortAscending = sortAscending;
  }

  public final boolean isSortAscending()
  {
    return _sortAscending;
  }

  public List<Person> getList()
  {
    return _list;
  }

  public List<Person> getSimpleList()
  {
    return _simpleList;
  }

  public void updateSortOrder(ActionEvent evt)
  {
    RowKeyPropertyModel model = _getCollectionModel();

    if (_sortProperty == null)
    {
      model.setSortCriteria(null);
    }
    else
    {
      model.setSortCriteria(Collections.singletonList(new SortCriterion(_sortProperty,
              _sortAscending)));
    }

    // Now fire a component event to re-order the components
    final FacesContext facesContext = FacesContext.getCurrentInstance();
    List<String> orderedKeys = _applySortToNonCollectionModelObjects();

    VisitContext visitContext = VisitContext.createVisitContext(facesContext,
      Arrays.asList("rm:personForEachParent", "rcm:personForEachParent"),
      null);
    facesContext.getViewRoot().visitTree(visitContext,
      new ReorderChildrenVisitCallback(orderedKeys));
  }

  public Map<String, ForEachBean.UpdatableItem> getUpdatableItemMap()
  {
    return _updatableItemMap;
  }

  public Map<String, Person> getMap()
  {
    return _map;
  }

  public Map<String, Person> getArrangeMap()
  {
    return _arrangeMap;
  }

  public SortableModel getModel()
  {
    return _getCollectionModel();
  }

  public void inputValueValueChange(ValueChangeEvent valueChangeEvent)
  {
    UIComponent target = valueChangeEvent.getComponent();
    String id = target.getId();
    String keySuffix = id.substring(id.lastIndexOf('_'));
    UIComponent toUpdate = target.findComponent("outputText" + keySuffix);
    RequestContext.getCurrentInstance().addPartialTarget(toUpdate);
  }

  public void inputValueWithNCValueChange(ValueChangeEvent valueChangeEvent)
  {
    UIComponent target = valueChangeEvent.getComponent();
    String id = target.getId();
    String keySuffix = id.substring(id.lastIndexOf('_'));
    UIComponent toUpdate = target.findComponent("testNC" + keySuffix + ":outputTextNC");
    RequestContext.getCurrentInstance().addPartialTarget(toUpdate);
  }

  public void handleArrangeNewItem(ActionEvent evt)
  {

  }

  public void handleArrangeRemoveItem(ActionEvent evt)
  {

  }

  public void handleArrangeMoveItemUp(ActionEvent evt)
  {

  }

  public void handleArrangeMoveItemDown(ActionEvent evt)
  {

  }

  /**
   * Applies the sort to the person list and the person map based on the sort order of the
   * collection model.
   * @return the sorted keys of the people to use for component reordering
   */
  private List<String> _applySortToNonCollectionModelObjects()
  {
    RowKeyPropertyModel model = _getCollectionModel();

    Object origRowKey = model.getRowKey();
    List<String> orderedKeys = new ArrayList<String>(model.getRowCount());
    _list.clear();
    _map.clear();
    try
    {
      for (model.setRowIndex(0);
        model.isRowAvailable();
        model.setRowIndex(_model.getRowIndex() + 1))
      {
        Person person = (Person)model.getRowData();
        orderedKeys.add(person.getKey());
        //System.out.println("Person: " + person);
        _list.add(person);
        _map.put(person.getKey(), person);
      }
    }
    finally
    {
      try
      {
        model.setRowKey(origRowKey);
      }
      catch (Throwable t)
      {
        ;
      }
    }

    return orderedKeys;
  }

  private RowKeyPropertyModel _getCollectionModel()
  {
    if (_model == null)
    {
      // Model will be null as it is not serializable and the view state is not preserved in
      // the server memory
      _model = new RowKeyPropertyModel(new ArrayList<Person>(_list), "key");

      if (_sortProperty == null)
      {
        _model.setSortCriteria(null);
      }
      else
      {
        _model.setSortCriteria(Collections.singletonList(new SortCriterion(_sortProperty,
                _sortAscending)));
      }
    }

    return _model;
  }

  private static class ReorderChildrenVisitCallback
    implements VisitCallback, Comparator<String>
  {
    private ReorderChildrenVisitCallback(List<String> orderedKeys)
    {
      _orderedKeys = orderedKeys;
    }

    @Override
    public VisitResult visit(
      VisitContext visitContext,
      UIComponent  target)
    {
      //System.out.println("**************VISIT " + target.getClientId());
      FacesContext facesContext = visitContext.getFacesContext();
      List<String> childrenIds = new ArrayList<String>();
      for (UIComponent child: target.getChildren())
      {
        // Safe to call get ID out of context
        childrenIds.add(child.getId());
      }

      Collections.sort(childrenIds, this);

      ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
      apm.addComponentChange(facesContext, target,
          new ReorderChildrenComponentChange(childrenIds));

      return VisitResult.ACCEPT;
    }

    @Override
    public int compare(
      String clientId1,
      String clientId2)
    {
      String key1 = StringUtils.substringAfterLast(clientId1, "_");
      String key2 = StringUtils.substringAfterLast(clientId2, "_");

      int index1 = _orderedKeys.indexOf(key1);
      int index2 = _orderedKeys.indexOf(key2);

      return index1 - index2;
    }

    private final List<String> _orderedKeys;
  }

  private final List<Person> _simpleList;
  private final List<Person> _list;
  private transient RowKeyPropertyModel _model;
  private final Map<String, Person> _map;
  private final Map<String, UpdatableItem> _updatableItemMap;
  private final Map<String, Person> _arrangeMap;
  private String _currentExample = "updates";
  private String _sortProperty;
  private boolean _sortAscending;

  @SuppressWarnings("compatibility:-8911883817863052298")
  private static final long serialVersionUID = 1L;
}
