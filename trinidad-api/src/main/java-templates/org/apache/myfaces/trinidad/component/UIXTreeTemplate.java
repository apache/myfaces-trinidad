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

import java.util.Set;

import javax.el.MethodExpression;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.event.SelectionEvent;

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
/**/  public abstract MethodExpression getRowDisclosureListener();
/**/  public abstract UIComponent getNodeStamp();

  @Deprecated
  public void setRowDisclosureListener(MethodBinding binding)
  {
    setRowDisclosureListener(adaptMethodBinding(binding));
  }
  
  @Deprecated
  public void setSelectionListener(MethodBinding binding)
  {
    setSelectionListener(adaptMethodBinding(binding));
  }
  
  /**
   * Sets the phaseID of UI events depending on the "immediate" property.
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    TableUtils.__handleQueueEvent(this, event);
    
    // If the currency is not already established,
    // associate selected row key with the event
    // UIXCollection.queueEvent() will wrap the event into
    // a TableRowEvent that will include currency information
    
    Object newKey = null;
    Object oldKey = getRowKey();
    if (oldKey == null)
    {
      Set keys = getSelectedRowKeys();
      if (!keys.isEmpty())
        newKey = keys.iterator().next();
    }
    
    if (newKey != null)
    {
      setRowKey(newKey);
      super.queueEvent(event);
      setRowKey(oldKey);
    }
    else
    {
      super.queueEvent(event);
    }
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
      broadcastToMethodExpression(event, getSelectionListener());
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
    getDisclosedRowKeys().setCollectionModel(model);
    getSelectedRowKeys().setCollectionModel(model);
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
}
