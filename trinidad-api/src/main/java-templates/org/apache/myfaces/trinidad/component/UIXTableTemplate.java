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
package org.apache.myfaces.trinidad.component;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.event.SortEvent;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;
import org.apache.myfaces.trinidad.model.SortCriterion;

/**
 * Base class for the Table component. The behaviour supported by this base class
 * include record navigation, sorting, selection and detail-disclosure.
 * <p>
 * @version $Name:  $ ($Revision$) $Date$
 */
abstract public class UIXTableTemplate extends UIXIteratorTemplate
  implements CollectionComponent
{

  @Override
  public void setSortCriteria(List<SortCriterion> criteria)
  {
    _sortCriteria = criteria;
    super.setSortCriteria(criteria);
  }

  /**
   * Sets the phaseID of UI events depending on the "immediate" property.
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    TableUtils.__handleQueueEvent(this, event);
    super.queueEvent(event);
  }

  /**
   * Delivers an event to the appropriate listeners.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    // the order of processing is
    // 1. do any default action handling
    // 2. invoke any actionListener method binding
    // 3. call all the registered ActionListener instances.

    // Deliver to the default RangeChangeListener
    if (event instanceof RangeChangeEvent)
    {
      RangeChangeEvent rEvent = (RangeChangeEvent) event;
      int first = rEvent.getNewStart();
      setFirst(first);
      //pu: Implicitly record a Change for 'first' attribute
      addAttributeChange("first", Integer.valueOf(first));
      
      if ((first == 0) && (rEvent.getNewEnd() == getRowCount()))
      {
        setShowAll(true);
        //pu: Implicitly record a Change for 'showAll' attribute
        addAttributeChange("showAll", Boolean.TRUE);
      }
      else if (isShowAll())
      {
        setShowAll(false);
        //pu: Implicitly record a Change for 'showAll' attribute
        addAttributeChange("showAll", Boolean.FALSE);
      }
      // since the range is now different we can clear the currency cache:
      clearCurrencyStringCache();
      
      broadcastToMethodBinding(event, getRangeChangeListener());
    }
    else if (event instanceof RowDisclosureEvent)
    {
      RowDisclosureEvent eEvent = (RowDisclosureEvent) event;
      RowKeySet set = getDisclosedRowKeys();
      set.addAll(eEvent.getAddedSet());
      set.removeAll(eEvent.getRemovedSet());
      broadcastToMethodBinding(event, getRowDisclosureListener());
    }
    else if (event instanceof SortEvent)
    {
      SortEvent sEvent = (SortEvent) event;
      setSortCriteria(sEvent.getSortCriteria());
      broadcastToMethodBinding(event, getSortListener());
    }
    else if (event instanceof SelectionEvent)
    {
      //pu: Implicitly record a Change for 'selectionState' attribute
      addAttributeChange("selectedRowKeys",
                         getSelectedRowKeys());
      broadcastToMethodBinding(event, getSelectionListener());
    }

    super.broadcast(event);
  }


/**/  abstract public void setDisclosedRowKeys(RowKeySet state);
/**/  abstract public RowKeySet getDisclosedRowKeys();
/**/  public abstract RowKeySet getSelectedRowKeys();
/**/  public abstract void setSelectedRowKeys(RowKeySet model);
/**/  abstract public void setFirst(int first);
/**/  abstract public void setShowAll(boolean showAll);
/**/  abstract public boolean isShowAll();
/**/  abstract public UIComponent getDetailStamp();
/**/  public abstract MethodBinding getRangeChangeListener();
/**/  public abstract MethodBinding getSortListener();
/**/  public abstract MethodBinding getRowDisclosureListener();
/**/  public abstract MethodBinding getSelectionListener();
/**/  public abstract boolean isImmediate();

  @Override
  @SuppressWarnings("unchecked")
  public Object saveState(FacesContext context)
  {
    Object o = super.saveState(context);
    if ((o == null) &&
        ((_sortCriteria == null) || _sortCriteria.isEmpty()))
      return null;

    return new Object[]{o, _sortCriteria};
  }

  @Override
  @SuppressWarnings("unchecked")
  public void restoreState(FacesContext context, Object state)
  {
    Object[] array = (Object[]) state;
    super.restoreState(context, array[0]);


    // Get the sort criteria - but *don't* call setSortCriteria()
    // here;  doing so would require getting the collection model,
    // and that may invoke client code that isn't quite in a state
    // to be invoked, in part because component "binding"s have not been
    // evaluated yet.
    List<SortCriterion> criteria = (List<SortCriterion>) array[1];
    _sortCriteria = criteria;
  }

  
  /**
   * Gets the data for the first selected row.
   * This is useful when using EL to get at column data for the selected
   * row when using a table with single selection.
   * @return null if there is nothing selected in the table.
   */
  public Object getSelectedRowData() 
  {
    RowKeySet state = getSelectedRowKeys();
    Iterator<Object> keys = state.iterator();
    if (keys.hasNext()) 
    {
      Object key = keys.next();
      CollectionModel model = getCollectionModel();
      Object old = model.getRowKey();
      try
      {
        model.setRowKey(key);
        if (isRowAvailable())
          return model.getRowData();
      }
      finally
      {
        model.setRowKey(old);
      }
    }
    return null;
  }

  @Override
  protected final void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    // process all the facets of this table just once
    // (except for the "detailStamp" facet which must be processed once
    // per row):
    TableUtils.__processFacets(context, this, this, phaseId,
      UIXTable.DETAIL_STAMP_FACET);

    // process all the facets of this table's column children:
    TableUtils.__processColumnFacets(context, this, this, phaseId);

    // process all the children and the detailStamp as many times as necessary
    _processStamps(context, phaseId);
  }

  /**
   * Gets the stamps. This returns the children of this component plus
   * the detail stamp (if any).
   */
  // TODO cache the result
  @Override
  protected final List<UIComponent> getStamps()
  {
    List<UIComponent> children = super.getStamps();
    UIComponent detail = getDetailStamp();
    if (detail != null)
    {
      List<UIComponent> stamps = new ArrayList<UIComponent>(children.size() + 1);
      stamps.addAll(children);
      stamps.add(detail);
      return stamps;
    }
    return children;
  }

  /**
   * Saves the state for the given stamp.
   * This method avoids changing the state of facets on columns.
   */
  @Override
  protected final Object saveStampState(FacesContext context, UIComponent stamp)
  {
    if (stamp instanceof UIXColumn)
    {
      // if it is a column, we don't want the facets processed.
      // Only the children:
      return StampState.saveChildStampState(context, stamp, this);
    }
    else
      return super.saveStampState(context, stamp);
  }

  /**
   * Restores the state for the given stamp.
   * This method avoids changing the state of facets on columns.
   */
  @Override
  protected final void restoreStampState(FacesContext context, UIComponent stamp,
                                         Object stampState)
  {
    if (stamp instanceof UIXColumn)
    {
      // if it is a column, we don't want the facets processed.
      // Only the children:
      StampState.restoreChildStampState(context, stamp, this, stampState);
    }
    else
      super.restoreStampState(context, stamp, stampState);
  }

  @Override
  protected final CollectionModel createCollectionModel(
    CollectionModel current,
    Object value)
  {
    CollectionModel model = super.createCollectionModel(current, value); 

    RowKeySet selectedRowKeys = getSelectedRowKeys();

    if (selectedRowKeys == null)
    {
      selectedRowKeys = new RowKeySetImpl();
      setSelectedRowKeys(selectedRowKeys);
    }

    RowKeySet disclosedRowKeys = getDisclosedRowKeys();

    if (disclosedRowKeys == null)
    {
      disclosedRowKeys = new RowKeySetImpl();
      setDisclosedRowKeys(disclosedRowKeys);
    }

    selectedRowKeys.setCollectionModel(model);
    disclosedRowKeys.setCollectionModel(model);

    // If we were perviously sorted, restore the sort order:
    if (_sortCriteria != null)
    {
      model.setSortCriteria(_sortCriteria);
    }

    return model;
  }

  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[6];
    state[0] = _sortCriteria;
    state[1] = super.__getMyStampState();
    state[2] = Integer.valueOf(getFirst());
    state[3] = Boolean.valueOf(isShowAll());
    state[4] = getSelectedRowKeys();
    state[5] = getDisclosedRowKeys();
    return state;
  }
  
  /**
   * Sets the internal state of this component.
   * @param stampState the internal state is obtained from this object.
   */
  @Override
  @SuppressWarnings("unchecked")
  void __setMyStampState(Object stampState)
  {
    Object[] state = (Object[]) stampState;
    _sortCriteria = (List<SortCriterion>) state[0];
    super.__setMyStampState(state[1]);
    setFirst(((Integer) state[2]).intValue());
    setShowAll(Boolean.TRUE == state[3]);
    setSelectedRowKeys((RowKeySet) state[4]);
    setDisclosedRowKeys((RowKeySet) state[5]);
  }

  private void _processStamps(
    FacesContext context,
    PhaseId phaseId)
  {
    // Process all the children
    CollectionModel tableData = getCollectionModel();
    if (tableData.getRowCount() != 0)
    {
      int startIndex = getFirst();
      int endIndex = isShowAll() ? getRowCount()-1 : TableUtils.getLast(this);

      UIComponent detail = getDetailStamp();
      RowKeySet disclosureState =
        (detail == null) ? null : getDisclosedRowKeys();

      for (int i = startIndex; i <= endIndex; i++)
      {
        setRowIndex(i);
        TableUtils.__processStampedChildren(context, this, phaseId);

        if ((disclosureState != null) && disclosureState.isContained())
        {
          assert getRowIndex() == i;
          processComponent(context, detail, phaseId);
        }
      }

      setRowIndex(-1);
    }
  }

  @Override
  void __init()
  {
    super.__init();
    if (getSelectedRowKeys() == null)
      setSelectedRowKeys(new RowKeySetImpl());
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetImpl());
    // if "first" is valueBound, we can't deal with it changing 
    // during the lifecycle. So stash it as a local value.
    // see bug 4537121:
    setFirst(getFirst());
  }

  transient private List<SortCriterion> _sortCriteria = null;
}
