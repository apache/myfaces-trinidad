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
package org.apache.myfaces.trinidadinternal.application;

import javax.faces.application.NavigationHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;

public class NavigationHandlerImpl extends NavigationHandler
{
  public NavigationHandlerImpl(NavigationHandler delegate)
  {
    _delegate = delegate;
  }

  @Override
  public void handleNavigation(
    FacesContext context,
    String       fromAction,
    String       outcome)
  {
    UIViewRoot oldRoot = context.getViewRoot();
        
    _delegate.handleNavigation(context, fromAction, outcome);

    UIViewRoot newRoot = context.getViewRoot();
    if ((outcome != null) && (newRoot != oldRoot))
    {
      RequestContext afc = RequestContext.getCurrentInstance();

      // Handle "dialog:" URLs
      if (outcome.startsWith(afc.getDialogService().getDialogNavigationPrefix()))
      {
        // Navigate back to the original root
        context.setViewRoot(oldRoot);

        // Give ourselves a new page flow scope
        afc.getPageFlowScopeProvider().pushPageFlowScope(context, true);
        // And ask the component to launch a dialog
        afc.getDialogService().queueLaunchEvent(newRoot);
      }
    }
  }
  
  
  private NavigationHandler _delegate;
}
