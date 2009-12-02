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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RenderingContext;

abstract public class FormElementRenderer extends EditableValueRenderer
{
  protected FormElementRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _autoSubmitKey = type.findKey("autoSubmit");
    _onblurKey  = type.findKey("onblur");
    _onfocusKey = type.findKey("onfocus");
    _onchangeKey = type.findKey("onchange");
    _labelKey = type.findKey("label");
    _contentStyleKey = type.findKey("contentStyle");
  }
  

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    if (_autoSubmitKey != null)
      detectAutoSubmit(context, component, clientId);

    return super.getSubmittedValue(context,
                                   component,
                                   clientId);
  }

  @SuppressWarnings("unchecked")
  protected final void detectAutoSubmit(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
      Map<String, String> parameterMap = 
        context.getExternalContext().getRequestParameterMap();
      
      String source = parameterMap.get("source");
      if (clientId.equals(source))
      {
        String event = parameterMap.get("event");
        if (TrinidadRenderingConstants.AUTOSUBMIT_EVENT.equals(event) &&
            isAutoSubmit(getFacesBean(component)))
        {
          (new AutoSubmitEvent(component)).queue();
        }
      }
  }

  @Override
  protected final void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (!renderAsElement(context, arc, bean))
    {
      encodeAllAsNonElement(context, arc, component, bean);
    }
    else
    {
      encodeAllAsElement(context, arc, component, bean);
    }
  }


  protected boolean isHiddenLabelRequired(RenderingContext arc)
  {
    return true;
  }

  /**
   */
  protected void renderShortDescAsHiddenLabel(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
    throws IOException
  {
    if (HiddenLabelUtils.supportsHiddenLabels(arc) && 
        isHiddenLabelRequired(arc))
    {
      String clientId = getClientId(context, component);
      if (HiddenLabelUtils.wantsHiddenLabel(arc, clientId))
      {
        String hiddenLabel = getHiddenLabel(bean);
        if (hiddenLabel != null)
        {
          HiddenLabelUtils.outputHiddenLabel(context,
                                             arc,
                                             clientId,
                                             hiddenLabel,
                                             component);
        }
      }
    }
  }
  

  protected void encodeAllAsElement(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
  }

  /**
   * @todo Make abstract if always overriden
   */
  protected void encodeAllAsNonElement(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", component);

    renderId(context, component);
    rw.writeAttribute("title", getShortDesc(bean), "shortDesc");
    renderStyleAttributes(context, arc, bean);

    renderNonElementContent(context, arc, component, bean);
    rw.endElement("span");
  }


  /**
   * @todo Remove if never necessary
   */
  protected void renderNonElementContent(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
  }

  @Override
  protected void renderEventHandlers(
    FacesContext context,
    FacesBean    bean) throws IOException
  {
    super.renderEventHandlers(context, bean);
    renderFormEventHandlers(context, bean);
  }

  protected void renderFormEventHandlers(
    FacesContext context,
    FacesBean    bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.writeAttribute("onblur", getOnblur(bean),  "onblur");
    rw.writeAttribute("onfocus", getOnfocus(bean),  "onfocus");
    rw.writeAttribute("onchange", getOnchange(bean),  "onchange");
  }


  /**
   * Should this component render as a form element, or just
   * as some non-form content?
   */
  protected final boolean renderAsElement(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean)
  {
    if (getReadOnly(context, bean) || !supportsEditing(arc))
    {
      if (!renderReadOnlyAsElement(arc, bean))
        return false;
    }

    if (!supportsDisabledFormElements(arc) &&
        getDisabled(bean))
    {
      return false;
    }

    return true;
  }

  protected boolean renderReadOnlyAsElement(
    RenderingContext arc,
    FacesBean           bean)
  {
    return false;
  }

  protected String getLabel(FacesBean bean)
  {
    // Not all FormElements necessarily have a label
    if (_labelKey == null)
      return null;

    return toString(bean.getProperty(_labelKey));
  }

  protected String getContentStyle(FacesBean bean)
  {
    if (_contentStyleKey == null)
      return null;
    
    return toString(bean.getProperty(_contentStyleKey));
  }
  

  protected String getOnblur(FacesBean bean)
  {
    if (_onblurKey == null)
      return null;

    return toString(bean.getProperty(_onblurKey));
  }

  protected String getOnfocus(FacesBean bean)
  {
    if (_onfocusKey == null)
      return null;

    return toString(bean.getProperty(_onfocusKey));
  }

  protected String getOnchange(FacesBean bean)
  {
    if (_onchangeKey == null)
      return null;

    return toString(bean.getProperty(_onchangeKey));
  }

  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_FIELD_TEXT_STYLE_CLASS;
  }

  /* FIXME: this method is never called
  protected String getDefaultDisabledStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_FIELD_TEXT_DISABLED_STYLE_CLASS;
  }*/

  /**
   * Tells whether or not the autoSubmit attribute is set on the bean
   *
   * @param bean the bean
   */
  protected boolean isAutoSubmit(FacesBean bean)
  {
    if (_autoSubmitKey == null)
      return false;

    return Boolean.TRUE.equals(bean.getProperty(_autoSubmitKey));
  }

  /**
   * Return the text for a hidden label, using "shortDesc" if set,
   * "label" otherwise.
   */
  protected String getHiddenLabel(FacesBean bean)
  {
    String hiddenLabel = getShortDesc(bean);
    if (hiddenLabel == null)
      hiddenLabel = getLabel(bean);

    return hiddenLabel;
  }


  /**
   * Dummy class purely to get subforms to recognize that
   * an event has occurred
   */
  static private final class AutoSubmitEvent extends FacesEvent
  {
    public AutoSubmitEvent(UIComponent source)
    {
      super(source);
      setPhaseId(PhaseId.INVOKE_APPLICATION);
    }

    @Override
    public void processListener(FacesListener listener)
    {
    }

    @Override
    public boolean isAppropriateListener(FacesListener listener)
    {
      return false;
    }

    private static final long serialVersionUID = 1L;
  }

  private PropertyKey _autoSubmitKey;
  private PropertyKey _labelKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _onblurKey;
  private PropertyKey _onfocusKey;
  private PropertyKey _onchangeKey;
}
