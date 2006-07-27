/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;

public class CommandLinkRenderer extends GoLinkRenderer
{
  public CommandLinkRenderer()
  {
    super(CoreCommandLink.TYPE);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey = type.findKey("immediate");
    _partialSubmitKey = type.findKey("partialSubmit");
  }

  public void decode(FacesContext context, UIComponent component)
  {
    RequestContext afContext = RequestContext.getCurrentInstance();
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
        if (getPartialSubmit(getFacesBean(component)))
        {
          PartialPageUtils.forcePartialRendering(context);
        }
      }
    }
  }
  
  protected void encodeBegin(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    if (getPartialSubmit(bean))
    {
      AutoSubmitUtils.writeDependencies(context, arc);
    }
 
    String clientId = comp.getClientId(context);
    // Make sure we don't have anything to save
    assert(arc.getCurrentClientId() == null);
    arc.setCurrentClientId(clientId);
    
    // Find the params up front, and save them off - 
    // getOnClick() doesn't have access to the UIComponent
    String extraParams = AutoSubmitUtils.getParameters(comp);
    Object old = arc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY,
                                         extraParams);
    super.encodeBegin(context, arc, comp, bean);
    // Restore any old params, though really, how could that happen??
    arc.getProperties().put(_EXTRA_SUBMIT_PARAMS_KEY, old);
    
    arc.setCurrentClientId(null);
  }

  public void encodeEnd(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    super.encodeEnd(context, arc, comp, bean);
    FormData fd = arc.getFormData();
    if (fd != null)
      fd.addNeededValue(XhtmlConstants.SOURCE_PARAM);
  }

  protected String getClientId(
    FacesContext context,
    UIComponent  component)
  {
    // Use the cached version
    return RenderingContext.getCurrentInstance().getCurrentClientId();
  }

  protected String getDestination(FacesBean bean)
  {
    return null;
  }

  protected String getTargetFrame(FacesBean bean)
  {
    return null;
  }


  protected boolean hasOnclick(FacesBean bean)
  {
    // More efficient
    return true;
  }

  protected String getOnclick(FacesBean bean)
  {
    String onclick = super.getOnclick(bean);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    String id = arc.getCurrentClientId();
    boolean immediate = getImmediate(bean);
    
    String extraParams = (String)
      arc.getProperties().get(_EXTRA_SUBMIT_PARAMS_KEY);

    String script;
    if (getPartialSubmit(bean))
    {
      script = AutoSubmitUtils.getSubmitScript(
                arc, id, immediate, false,
                null/* no event*/,
                extraParams,
                false);
    }
    else
    {
      script = AutoSubmitUtils.getFullPageSubmitScript(
                arc, id, immediate,
                null/*no event*/,
                extraParams,
                false/* return false*/);
    }

    return XhtmlUtils.getChainedJS(onclick, script, true);
  }

  protected boolean getImmediate(FacesBean bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }


  protected boolean getPartialSubmit(FacesBean bean)
  {
    Object o = bean.getProperty(_partialSubmitKey);
    if (o == null)
      o = _partialSubmitKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  
  private PropertyKey _immediateKey;
  private PropertyKey _partialSubmitKey;
  
  static private final Object _EXTRA_SUBMIT_PARAMS_KEY = new Object();
}
