/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.application;

import java.io.IOException;

import javax.faces.FacesException;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.render.InternalView;


public class TestInternalView extends InternalView
{
  public TestInternalView()
  {
  }

  public UIViewRoot createView(FacesContext context, String viewId)
  {
    ViewHandlerImplTest.__internalViewCalled = "create";
    return null;
  }

  /**
   * Restores the UIViewRoot;  return null if no view should be returned.
   */
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    ViewHandlerImplTest.__internalViewCalled = "restore";
    return null;
  }
  
  /**
   * Renders the view.
   */
  public void renderView(
    FacesContext context, 
    UIViewRoot   viewToRender) throws IOException, FacesException
  {
    ViewHandlerImplTest.__internalViewCalled = "render";
  }
}
