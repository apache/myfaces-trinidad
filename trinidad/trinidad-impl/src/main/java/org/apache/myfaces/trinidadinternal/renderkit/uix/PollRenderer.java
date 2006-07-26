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
package org.apache.myfaces.adfinternal.renderkit.uix;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.component.UIXPoll;
import org.apache.myfaces.adf.event.PollEvent;

import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.uinode.UINodeRendererBase;

/**
 * Renderer for poll component.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/PollRenderer.java#0 $) $Date: 10-nov-2005.19:00:32 $
 * @author The Oracle ADF Faces Team
 */
public class PollRenderer extends UINodeRendererBase
{
  public void decode(FacesContext context, UIComponent component)
  {
    Map parameters =  context.getExternalContext().getRequestParameterMap();
    Object event = parameters.get(UIConstants.EVENT_PARAM);
    if (UIConstants.POLL_EVENT.equals(event))
    {
      Object source = parameters.get(UIConstants.SOURCE_PARAM);
      String id = component.getClientId(context);
      
      if (id.equals(source))
      {
        UIXPoll poll = (UIXPoll) component;
        (new PollEvent(component)).queue();
        if (poll.isImmediate())
          context.renderResponse();
      }
    }
  }
}
