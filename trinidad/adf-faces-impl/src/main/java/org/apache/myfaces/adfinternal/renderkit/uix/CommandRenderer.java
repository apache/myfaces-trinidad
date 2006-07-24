/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.uix;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.event.ReturnEvent;

import org.apache.myfaces.adfinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.adfinternal.uinode.UINodeRendererBase;

/**
 * Renderer for command components like commandButton and commandLink
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/CommandRenderer.java#0 $) $Date: 10-nov-2005.19:00:27 $
 * @author The Oracle ADF Faces Team
 */
public class CommandRenderer extends UINodeRendererBase
{
  public void decode(FacesContext context, UIComponent component)
  {
    AdfFacesContext afContext = AdfFacesContext.getCurrentInstance();
    ReturnEvent returnEvent =
      afContext.getDialogService().getReturnEvent(component);
    if (returnEvent != null)
    {
      returnEvent.queue();
    }
    else
    {
      Map parameterMap = context.getExternalContext().getRequestParameterMap();
      Object source = parameterMap.get("source");
      String clientId = component.getClientId(context);

      if ((source != null) && source.equals(clientId))
      {
        (new ActionEvent(component)).queue();
        Map attrs = component.getAttributes();
        if (Boolean.TRUE.equals(attrs.get("partialSubmit")))
        {
          PartialPageUtils.forcePartialRendering(context);
        }
      }
    }
  }
}
