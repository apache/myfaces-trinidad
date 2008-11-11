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

import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;


/**
 * Base class for the Page component.
 *
 * @version $Name:  $ ($Revision$) $Date$
 */
abstract public class UIXPageTemplate extends UIXMenuHierarchy
{
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet state);
/**/  public abstract MethodBinding getRowDisclosureListener();
/**/  static public final PropertyKey DISCLOSED_ROW_KEYS_KEY = null;

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
    HierarchyUtils.__handleBroadcast(this,
                                      event,
                                      getDisclosedRowKeys(),
                                      getRowDisclosureListener());
    super.broadcast(event);
  }

  @Override
 public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    TreeModel model = (TreeModel)super.createCollectionModel(current, value);
    RowKeySet treeState = getDisclosedRowKeys();
    treeState.setCollectionModel(model);
    return model;
  }

  @Override
  @SuppressWarnings("unchecked")
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    Object oldPath = getRowKey();
    setRowKey(null);

    HierarchyUtils.__iterateOverTree(context,
                                      phaseId,
                                      this,
                                      getDisclosedRowKeys(),
                                      false);

    setRowKey(oldPath);

    // process the children
    TableUtils.__processChildren(context, this, phaseId);

    Map<String, UIComponent> facets = getFacets();
    Iterator<String> facetKeys = facets.keySet().iterator();

    while(facetKeys.hasNext())
    {
      String facetKey = facetKeys.next();
      if (!"nodeStamp".equals(facetKey))
      {
        processComponent(context, facets.get(facetKey), phaseId);
      }
    }

  }

  @Override
  void __encodeBegin(FacesContext context) throws IOException
  {
    HierarchyUtils.__handleEncodeBegin(this, getDisclosedRowKeys());
    super.__encodeBegin(context);
  }

  @Override
  void __init()
  {
    super.__init();
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetTreeImpl());
  }

  @Override
  protected FacesBean createFacesBean(String rendererType)
  {
    return new RowKeyFacesBeanWrapper(super.createFacesBean(rendererType));
  }

  private class RowKeyFacesBeanWrapper
    extends FacesBeanWrapper
  {
    private boolean _retrievingDisclosedRows = false;

    RowKeyFacesBeanWrapper(FacesBean bean)
    {
      super(bean);
    }

    @Override
    public Object getProperty(PropertyKey key)
    {
      Object value = super.getProperty(key);

      if (key == DISCLOSED_ROW_KEYS_KEY && !_retrievingDisclosedRows && value instanceof RowKeySet)
      {
        // Ensure that when we are retrieving and setting the collection model, this property
        // is not asked for which would create an infinite loop
        _retrievingDisclosedRows = true;

        try
        {
          RowKeySet rowKeys = (RowKeySet) value;
          // row key sets need the most recent collection model, but there is no one common entry
          // point to set this on the set besides when code asks for the value from the bean
          rowKeys.setCollectionModel(getCollectionModel());
        }
        finally
        {
          _retrievingDisclosedRows = false;
        }
      }

      return value;
    }

    @Override
    public Object saveState(FacesContext context)
    {
      RowKeySet rowKeys = (RowKeySet)super.getProperty(DISCLOSED_ROW_KEYS_KEY);
      if (rowKeys != null)
      {
        // make sure the set does not pin the model in memory
        rowKeys.setCollectionModel(null);
      }
      return super.saveState(context);
    }
  }
}
