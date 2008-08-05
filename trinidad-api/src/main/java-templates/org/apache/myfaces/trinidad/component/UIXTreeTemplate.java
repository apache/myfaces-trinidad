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

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;

/**
 * Base class for Tree component.
 * @version $Name:  $ ($Revision$) $Date$
 */
abstract public class UIXTreeTemplate extends UIXHierarchy
{
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet keys);
/**/  public abstract RowKeySet getSelectedRowKeys();
/**/  public abstract void setSelectedRowKeys(RowKeySet keys);
/**/  public abstract MethodBinding getRowDisclosureListener();
/**/  public abstract UIComponent getNodeStamp();
/**/  public abstract boolean isInitiallyExpanded();
  
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
   * Delivers an event.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  { 
    if (event instanceof SelectionEvent)
    {
      //pu: Implicitly record a Change for 'selectionState' attribute
      //=-=pu: This ain't getting restored. Check with Arj or file a bug.
      addAttributeChange("selectedRowKeys",
                         getSelectedRowKeys());
      broadcastToMethodBinding(event, getSelectionListener());
    }

    HierarchyUtils.__handleBroadcast(this, 
                                      event, 
                                      getDisclosedRowKeys(), 
                                      getRowDisclosureListener());
    super.broadcast(event);
  }
 
  @Override
  public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    
    TreeModel model = ModelUtils.toTreeModel(value);
    model.setRowKey(null);

    RowKeySet selectedRowKeys = getSelectedRowKeys();

    if (selectedRowKeys == null)
    {
      selectedRowKeys = new RowKeySetTreeImpl();
      setSelectedRowKeys(selectedRowKeys);
    }

    RowKeySet disclosedRowKeys = getDisclosedRowKeys();

    if (disclosedRowKeys == null)
    {
      disclosedRowKeys = new RowKeySetTreeImpl();
      setDisclosedRowKeys(disclosedRowKeys);
    }

    selectedRowKeys.setCollectionModel(model);
    disclosedRowKeys.setCollectionModel(model);

    return model;
  }

  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    // this component has no facets that need to be processed once.
    // instead process the "nodeStamp" facet as many times as necessary:
    Object oldPath = getRowKey();
    setRowKey(null);
    HierarchyUtils.__iterateOverTree(context, 
                                     phaseId, 
                                     this, 
                                     getDisclosedRowKeys(),
                                     true);
    setRowKey(oldPath);
  }

  @Override
  void __init()
  {
    super.__init();
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetTreeImpl());
    if (getSelectedRowKeys() == null)
      setSelectedRowKeys(new RowKeySetTreeImpl());
  }
  
  
  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[4];
    state[0] = super.__getMyStampState();
    state[1] = getFocusRowKey();    
    state[2] = getSelectedRowKeys();
    state[3] = getDisclosedRowKeys();
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
    super.__setMyStampState(state[0]);
    setFocusRowKey(state[1]);
    setSelectedRowKeys((RowKeySet) state[2]);
    setDisclosedRowKeys((RowKeySet) state[3]);
  }
  
 
  private final static String EXPAND_ONCE_KEY = "initialExpandCompleted";

  /**
   * @see org.apache.myfaces.trinidad.component.UIXCollection#__encodeBegin(javax.faces.context.FacesContext)
   */
  @Override @SuppressWarnings("unchecked")
  protected void __encodeBegin(FacesContext context) throws IOException
  {
    if (isInitiallyExpanded() && !Boolean.TRUE.equals(getAttributes().get(EXPAND_ONCE_KEY)))
    {
      Object oldRowKey = getRowKey();
      try
      {
        Object rowKey = getFocusRowKey();
        if (rowKey == null)
        {
          setRowIndex(0);
          rowKey = getRowKey();
        }

        setRowKey(rowKey);
        RowKeySet old = getDisclosedRowKeys();
        RowKeySet newset = old.clone();
        newset.addAll();

        // use an event to ensure the row disclosure listener is called
        broadcast(new RowDisclosureEvent(old, newset, this));
      }
      finally
      {
        setRowKey(oldRowKey);
      }

      // use attributes to store that we have processed the initial expansion
      // as there is no support for private properties in the plug-in at the
      // moment
      getAttributes().put(EXPAND_ONCE_KEY, Boolean.TRUE);
    }
    super.__encodeBegin(context);    
  }
}
