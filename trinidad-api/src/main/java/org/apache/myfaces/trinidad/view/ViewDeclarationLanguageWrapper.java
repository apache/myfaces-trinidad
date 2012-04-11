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

/**
 * This class will be deprecated when we move to JSF 2.2
 */
package org.apache.myfaces.trinidad.view;

import java.io.IOException;

import java.util.List;

import javax.faces.FacesWrapper;
import javax.faces.application.Resource;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.view.AttachedObjectHandler;
import javax.faces.view.StateManagementStrategy;
import javax.faces.view.ViewDeclarationLanguage;
import javax.faces.view.ViewMetadata;

import java.beans.BeanInfo;

public abstract class ViewDeclarationLanguageWrapper extends ViewDeclarationLanguage
  implements FacesWrapper<ViewDeclarationLanguage>
{
  @Override
  public abstract ViewDeclarationLanguage getWrapped();
  
  @Override
  public BeanInfo getComponentMetadata(FacesContext context, Resource componentResource)
  {
    return getWrapped().getComponentMetadata(context, componentResource);
  }
  
  @Override
  public ViewMetadata getViewMetadata(FacesContext context, String viewId)
  {
    return getWrapped().getViewMetadata(context, viewId);
  }
  
  @Override
  public Resource getScriptComponentResource(FacesContext context, Resource componentResource)
  {
    return getWrapped().getScriptComponentResource(context, componentResource);
  }
  
  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    return getWrapped().createView(context, viewId);
  }
  
  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    return getWrapped().restoreView(context, viewId);
  }
  
  @Override
  public void retargetAttachedObjects(FacesContext context, UIComponent topLevelComponent,
                                      List<AttachedObjectHandler> handlers)
  {
    getWrapped().retargetAttachedObjects(context, topLevelComponent, handlers);
  }

  @Override
  public void retargetMethodExpressions(FacesContext context, UIComponent topLevelComponent)
  {
    getWrapped().retargetMethodExpressions(context, topLevelComponent);
  }
  
  @Override
  public void buildView(FacesContext context, UIViewRoot root)
    throws IOException
  {
    getWrapped().buildView(context, root);
  }
  
  @Override
  public void renderView(FacesContext context, UIViewRoot view)
    throws IOException
  {
    getWrapped().renderView(context, view);
  }
  
  @Override
  public StateManagementStrategy getStateManagementStrategy(FacesContext context, String viewId)
  {
    return getWrapped().getStateManagementStrategy(context, viewId);
  }
  
  @Override
  public boolean viewExists(FacesContext context, String viewId)
  {
    return getWrapped().viewExists(context, viewId);
  }

  @Override
  public String getId()
  {
    return getWrapped().getId();
  }
}
