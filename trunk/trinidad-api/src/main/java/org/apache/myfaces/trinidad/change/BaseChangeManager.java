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
package org.apache.myfaces.trinidad.change;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXIterator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.w3c.dom.Document;


/**
 * Base ChangeManager implementation that manages the bookkeeping for
 * supporting both ComponentChanges and DocumentChanges.
 * subclasses must implement addComponentChangeImpl() to implement
 * the ComponentChange support.  To support DocumentChanges,
 * <code>getDocument</code> must be implemented.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/BaseChangeManager.java#1 $) $Date: 11-nov-2005.14:59:41 $
 */
abstract class BaseChangeManager extends ChangeManager
{
  /**
   * {@inheritDoc}
   */
  @Override
  public void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    // if our component is a stamped component by UIXIterator, we 
    // don't want to persist the changes 
    UIComponent parent = uiComponent.getParent();
    UIComponent root = facesContext.getViewRoot();
    while (parent != null && parent != root)
    {
      if (parent.getClass() == UIXIterator.class) 
      {
        _LOG.info("DONT_PERSIST_STAMPED_COMPONENT_INSIDE_ITERATOR");      
        return;
      }
      parent = parent.getParent();      
    }
        
    if (facesContext == null || uiComponent == null || change == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_ADD_CHANGE_WITH_FACECONTEXT_OR_UICOMPONENT_OR_NULL"));

    // add the change to the component
    addComponentChangeImpl(facesContext, uiComponent, change);

    // add a corresponding DocumentChange if possible
    _addEquivalentDocumentChange(facesContext, uiComponent, change);
  }
  
  /**
   * A no-op implementation of adding a ComponentChange. Sub-classers should
   * override and provide an implementation if they support component changes.
   * @param facesContext The FacesContext for this request.
   * @param targetComponent The target component against which this change needs 
   * to be registered and applied later on.
   * @param componentChange The ComponentChange to add
   */
   protected void addComponentChangeImpl(
    FacesContext facesContext,
    UIComponent targetComponent,
    ComponentChange componentChange)
  {
    //no-op
  }

  // =-= bts Testing hack hook
  protected void persistDocumentChanges(
    FacesContext facesContext)
  {
    // noop
  }

  /**
   * Override to return the Document to modify as part of document-based
   * persistence.
   * Subclassers adding Document-based Persistence
   * must override this method and should override
   * <code>supportsDocumentPersistence</code>
   * in order to enable  Document-based Persistence
   */
  protected abstract Document getDocument(FacesContext context);
  
  /**
   *  Returns true if we can support Document-based Persistence
   *  in this ChangeManager.  Subclassers adding Document-based Persistence
   *  should override this method and must override <code>getDocument</code>
   *  in order to enable  Document-based Persistence.
   * @param context
   * @return true if we can support Document-based Persistence
   */
  protected boolean supportsDocumentPersistence(FacesContext context)
  {
    // correct, but potentially slow implementation
    return getDocument(context) != null;
  }

  /*
   * Create and add a DocumentChange corresponding to the supplied ComponentChange
   */
  private void _addEquivalentDocumentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange compChange)
  {
    if (supportsDocumentPersistence(facesContext))
    {
      // try to get equivalent DocumentChange from ComponentChange
      DocumentChange docChange = createDocumentChange(compChange);

      if (docChange != null)
      {
        //  We ("this" object) could be just a delegate of the top level change manager (i.e. the 
        //  changemanager registered with the webapp). Automatically adding a document change (or 
        //  deciding whether to add one) should be done by the top level changemanager 
        ChangeManager topChangeManager = RequestContext.getCurrentInstance().getChangeManager();
        
        if ( ChangeOutcome.CHANGE_APPLIED ==
              topChangeManager.addDocumentChangeWithOutcome(facesContext, uiComponent, docChange) )
        {
          // notify the registered change manager that we applied the document change 
          topChangeManager.documentChangeApplied(facesContext, uiComponent, compChange);
        }
      }
    }    
  }
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    BaseChangeManager.class);
}
