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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.XhtmlConstants;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRendererUtils;

/**
 */
public abstract class SimpleSelectBooleanRenderer extends FormInputRenderer
{
  public SimpleSelectBooleanRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _accessKeyKey = type.findKey("accessKey");
  }

  
  //**********************
  //encode
  //**********************


  abstract protected Object getValueAttr(RenderingContext arc);

  abstract protected Object getType();

  abstract protected String getIconName(
    boolean selected,
    boolean disabled
  );
  
  abstract protected String getIconAltTextName(
    boolean selected
  );


  protected void renderNameAttribute(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean
    )throws IOException
  {
    // no-op
  }
  
  /**
   * @TODO use new renderAllAttributes that doesn't render styleclass once
   * Jeanne adds it.
   */
  @Override
  protected void encodeAllAsElement(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    String clientId = LabelAndMessageRenderer.__getCachedClientId(arc); 
    
    boolean getRenderSimpleSpan = getRenderSimpleSpan(bean);
    if (getRenderSimpleSpan)
    {
      writer.startElement("span", component);
      // put the outer style class here, like af_selectBooleanRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, arc, component, bean);
      // render transformed id so that it can be PPR'd.
      _renderPartialId(context, arc, clientId);
    }
    // Added span around the entire element
    // for the Visual Editor and PPR. See bug # 2222541.
    writer.startElement("span", component);
    
    // render the events only if the browser supports JavaScript
    if (supportsScripting(arc))
    {
      renderSpanEventHandlers(context, bean);
    }
    
    renderStyleClass(context, arc, getContentStyleClass(bean)); 
    renderInlineStyleAttribute(context, arc, getContentStyle(bean));

    if (!getRenderSimpleSpan)
    {
      // render transformed id so that it can be PPR'd.
      _renderPartialId(context, arc, clientId);
    }

    char accessKey;
    if (supportsAccessKeys(arc))
      accessKey = getAccessKey(bean);
    else
      accessKey = CHAR_UNDEFINED;

    if (isAutoSubmit(bean))
      AutoSubmitUtils.writeDependencies(context, arc);
    
    writer.startElement("input", null);
    renderId(context, component);
    // Not calling super.renderAllAttributes or style classes are written out
    renderShortDescAttribute(context, arc, bean);
    
    // render the events only if the browser supports JavaScript
    if (supportsScripting(arc))
    {
      renderInputEventHandlers(context, bean);
    }
    renderDisabledAttribute(context, arc, bean);
    if (!shouldRenderName(context, component))
      renderNameAttribute(context, arc, bean);    
    if (accessKey != CHAR_UNDEFINED)
      writer.writeAttribute("accesskey", Character.toString(accessKey), null);
    writer.writeAttribute("type", getType(), null);
    writer.writeAttribute("value", getValueAttr(arc), null);
    
    Object value = getSubmittedValue(bean);
    if (value == null)
      value = getValue(bean);
    
    if (!(value instanceof Boolean)) 
      value = Boolean.valueOf(getConvertedString(context, component, bean)); 

    if ( Boolean.TRUE.equals(value))  
      writer.writeAttribute("checked", Boolean.TRUE, "value");

    writer.endElement("input");
    
    String text = getText(bean);

    boolean renderLabelTags = _isLabelTagRendered(arc, text, accessKey);
    
    if (renderLabelTags)
    {
      writer.startElement("label", null);

      writer.writeAttribute("for", clientId, null);
      HiddenLabelUtils.rememberLabel(arc, clientId);
    }

    renderText(context, arc, component, bean, true, false, text, accessKey);

    if (renderLabelTags)
    {
      writer.endElement("label");
    }

    // see bug 2880407 we dont need to render hidden label when wrapped with
    // fieldset and legend
    if (isHiddenLabelRequired(arc))
      renderShortDescAsHiddenLabel(context, arc, component, bean);
        
    writer.endElement("span"); 
    if (getRenderSimpleSpan)
      writer.endElement("span");      
  }

  @Override
  protected void encodeAllAsNonElement(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    boolean getRenderSimpleSpan = getRenderSimpleSpan(bean);
    String clientId = LabelAndMessageRenderer.__getCachedClientId(arc);    

    if (getRenderSimpleSpan)
    {
      writer.startElement("span", component);
      // put the outer style class here, like af_selectBooleanRadio, styleClass,
      // inlineStyle, 'state' styles like p_AFDisabled, etc.
      renderRootDomElementStyles(context, arc, component, bean);
      // render transformed id so that it can be PPR'd.
      _renderPartialId(context, arc, clientId);
    }
    
    // Added span around the entire element
    // for the Visual Editor and PPR. See bug # 2222541.
    writer.startElement("div", component);
    if (!getRenderSimpleSpan)
    {
      // render transformed id on the span so that it can be PPR'd.
      // this needs to be composite id even when readOnly is true for PPR
      _renderPartialId(context, arc, clientId);
    }
    renderStyleClass(context, arc, getContentStyleClass(bean));
    renderInlineStyleAttribute(context, arc, getContentStyle(bean));
    renderShortDescAttribute(context, arc, bean);
    
    Object value = getSubmittedValue(bean);
    if ( value == null)
      value = getValue(bean);

    if (!(value instanceof Boolean)) 
      value = Boolean.valueOf(getConvertedString(context, component, bean)); 

    boolean selected = Boolean.TRUE.equals(value);
    boolean disabled = getDisabled(bean);

    String iconName = getIconName(selected, disabled);     
    Icon icon = arc.getIcon(iconName);
    
    String altTextName = getIconAltTextName(selected);

    String shortDesc = arc.getTranslatedString(altTextName);   
    OutputUtils.renderIcon(context, arc, icon, shortDesc, null );                      

    String text = getText(bean);    
    if (text != null)
    {
      writer.writeText(XhtmlConstants.NBSP_STRING, null);
      
      // output the text directly since we have no access key
      writer.writeText(text, "text");        
    }

    writer.endElement("div");   
    if (getRenderSimpleSpan)
      writer.endElement("span");
  }

  protected void renderSpanEventHandlers(
    FacesContext context, 
    FacesBean    bean) throws IOException
  {
    //no op
  }
  
  protected void renderInputEventHandlers(
    FacesContext context, 
    FacesBean    bean) throws IOException
  {
    renderEventHandlers(context, bean); 
  }

  protected void renderAccessKeyAttribute(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
  }  
  
  /**
   * Renders the node text
   */
  protected void renderText(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    boolean             renderAccessKeys,
    boolean             renderID,
    String              text,
    char                accessKey
    ) throws IOException
  {

    if (text != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement("span", component);
      if (renderID)
        renderId( context, component);

      if (renderAccessKeys && supportsAccessKeys(arc))
      {
        // hightlight any access keys with an underline
        AccessKeyUtils.renderAccessKeyText(context, text, accessKey,
                                           SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
      }
      else
      {
        // output the text directly since we have no access key
        writer.writeText(text, null);
      }

      writer.endElement("span");
    }
  }
  
  protected String getAutoSubmitScript(
    FacesBean bean
  )
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    String source = LabelAndMessageRenderer.__getCachedClientId(arc);
    boolean immediate = isImmediate(bean);
    boolean isRadio = isRadio();
    return AutoSubmitUtils.getSubmitScript(arc, source, 
                                           immediate, isRadio,
                                           TrinidadRenderingConstants.AUTOSUBMIT_EVENT, null, true);
  }                     

  protected boolean isRadio()
  {
    return false;
  }


  protected String getCompositeId(String clientId)
  {
    return clientId + TrinidadRenderingConstants.COMPOSITE_ID_EXTENSION + "c";   
  }
    
  protected String getText(FacesBean bean)
  {
    return toString(bean.getProperty(_textKey));
  }

  protected char getAccessKey(FacesBean bean)
  {
    return toChar(bean.getProperty(_accessKeyKey));
  }

  /**
   * return true if you want to render the span that goes around the simple
   * element, where we put the root dom element styles.
   */
  protected boolean getRenderSimpleSpan(FacesBean bean)
  {
    return getSimple(bean);
  }
 
 
    
  /**
   * checks to see if we need label tags around this element and its text.
   * we don't need label tags if this element is readOnly or disabled, or if
   * it does not have any text.
   */
  private boolean _isLabelTagRendered(
    RenderingContext arc,
    String              text,
    char                accessKey
    )
  {
    if (text == null)
      return false;

    if (isInaccessibleMode(arc) &&
        accessKey == CHAR_UNDEFINED)
      return false;

    return true;
  }

  /**
   * this id is rendered so that the node can be ppr replaced
   */
  private void _renderPartialId(
    FacesContext        context,
    RenderingContext arc,
    String              clientId
  )throws IOException
  {
    if (clientId != null && CoreRendererUtils.supportsPartialRendering(arc))
    {
      
      String compositeId = getCompositeId(clientId);

      context.getResponseWriter().writeAttribute("id", compositeId, null );
    }
  }


  private PropertyKey _textKey;
  private PropertyKey _accessKeyKey;
}
