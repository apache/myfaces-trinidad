package org.apache.myfaces.trinidaddemo.tagDemos;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.commons.lang.StringUtils;
import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ReorderChildrenComponentChange;
import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.visit.VisitTreeUtils;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.model.RowKeyPropertyModel;
import org.apache.myfaces.trinidad.model.SortCriterion;
import org.apache.myfaces.trinidad.model.SortableModel;


public class ForEachBean
{
  public static class Person
  {
    public Person(
      String key,
      String firstName,
      String lastName)
    {
      this._firstName = firstName;
      this._lastName = lastName;
      this._key = key;
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
  }

  public ForEachBean()
  {
    _simpleList = Arrays.asList(
                    "One", "Two", "Three", "Four");
    _model = new RowKeyPropertyModel(
               Arrays.asList(
                 new Person("a", "John", "Doe"),
                 new Person("b", "Jane", "Doe"),
                 new Person("c", "Bob", "Smith"),
                 new Person("d", "Alice", "Jones")),
               "key");
  }

  public final void setSortProperty(String sortProperty)
  {
    this._sortProperty = sortProperty;
  }

  public final String getSortProperty()
  {
    return _sortProperty;
  }

  public final void setSortAscending(boolean sortAscending)
  {
    this._sortAscending = sortAscending;
  }

  public final boolean isSortAscending()
  {
    return _sortAscending;
  }

  public List<String> getSimpleList()
  {
    return _simpleList;
  }

  public void updateSortOrder(
    ActionEvent evt)
  {
    UIXCommand sourceComponent = (UIXCommand) evt.getComponent();

    if (_sortProperty == null)
    {
      _model.setSortCriteria(null);
    }
    else
    {
      _model.setSortCriteria(
        Collections.singletonList(new SortCriterion(_sortProperty, _sortAscending)));
    }

    // Now fire a component event to re-order the components
    Object origRowKey = _model.getRowKey();
    final FacesContext facesContext = FacesContext.getCurrentInstance();

    final List<String> orderedKeys = new ArrayList<String>(_model.getRowCount());
    for (_model.setRowIndex(0);
         _model.isRowAvailable();
         _model.setRowIndex(_model.getRowIndex() + 1))
    {
      Person person = (Person)_model.getRowData();
      orderedKeys.add(person.getKey());
      System.out.println("Person: " + person);
    }
    _model.setRowKey(origRowKey);

    for (UIComponent targetComponent :
         RequestContext.getCurrentInstance().getPartialTargets(sourceComponent))
    {
      String clientId = targetComponent.getClientId();
      System.out.println("CLIENT ID: " + clientId);
      VisitTreeUtils.visitSingleComponent(facesContext, clientId,
        new ReorderChildrenVisitCallback(orderedKeys));
    }
  }

  public SortableModel getModel()
  {
    return _model;
  }

  private static class ReorderChildrenVisitCallback
    implements VisitCallback, Comparator<String>
  {
    private ReorderChildrenVisitCallback(
      List<String> orderedKeys)
    {
      this._orderedKeys = orderedKeys;
    }

    @Override
    public VisitResult visit(
      VisitContext visitContext,
      UIComponent  target)
    {
      System.out.println("**************VISIT " + target.getClientId());
      FacesContext facesContext = visitContext.getFacesContext();
      List<String> childrenIds = new ArrayList<String>();
      for (UIComponent child : target.getChildren())
      {
        // Safe to call get ID out of context
        childrenIds.add(child.getId());
      }

      Collections.sort(childrenIds, this);
      System.out.println("Sorted list:");
      for (String clientId : childrenIds)
      {
        System.out.println("  " + clientId);
      }

      ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
      apm.addComponentChange(facesContext, target,
        new ReorderChildrenComponentChange(childrenIds));

      return VisitResult.COMPLETE;
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

  private final List<String> _simpleList;
  private final RowKeyPropertyModel _model;
  private String _sortProperty;
  private boolean _sortAscending;
}
