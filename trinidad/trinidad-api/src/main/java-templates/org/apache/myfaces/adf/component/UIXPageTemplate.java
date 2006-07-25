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
package org.apache.myfaces.adf.component;
import java.io.IOException;

import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.adf.model.CollectionModel;
import org.apache.myfaces.adf.model.RowKeySet;
import org.apache.myfaces.adf.model.RowKeySetTreeImpl;
import org.apache.myfaces.adf.model.TreeModel;


/**
 * Base class for the Page component.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java-templates/oracle/adf/view/faces/component/UIXPageTemplate.java#0 $) $Date: 10-nov-2005.19:07:40 $
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXPageTemplate extends UIXMenuHierarchy
{
/**/  public abstract RowKeySet getDisclosedRowKeys();
/**/  public abstract void setDisclosedRowKeys(RowKeySet state);
/**/  public abstract MethodBinding getRowDisclosureListener();

 /**
   * Sets the phaseID of UI events depending on the "immediate" property.
   */
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
  public void broadcast(FacesEvent event) throws AbortProcessingException
  { 
    HierarchyUtils.__handleBroadcast(this, 
                                      event, 
                                      getDisclosedRowKeys(), 
                                      getRowDisclosureListener());
    super.broadcast(event);
  }
 
   
 public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    TreeModel model = (TreeModel)super.createCollectionModel(current, value);    
    RowKeySet treeState = getDisclosedRowKeys();
    treeState.setCollectionModel(model);    
    return model;
  }
 
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

    Map facets = getFacets();
    Iterator facetKeys = facets.keySet().iterator();

    while(facetKeys.hasNext())
    {
      Object facetKey = facetKeys.next();
      if (!"nodeStamp".equals(facetKey))
      {
        processComponent(context, (UIComponent)facets.get(facetKey), phaseId);
      }
    }
        
  }


  void __encodeBegin(FacesContext context) throws IOException
  {
    HierarchyUtils.__handleEncodeBegin(this, getDisclosedRowKeys());
    super.__encodeBegin(context);
  }
  
  void __init()
  {
    super.__init();
    if (getDisclosedRowKeys() == null)
      setDisclosedRowKeys(new RowKeySetTreeImpl());
  }  

 
}
