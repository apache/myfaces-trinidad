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
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetail;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * This needs to be massively cleaned up...
 * @todo TEST NON-PPR!  I removed the non-PPR case from the
 *   JS script
 */
public class ShowDetailRenderer extends ShowDetailItemRenderer
{
  public ShowDetailRenderer()
  {
    this(CoreShowDetail.TYPE);
  }

  protected ShowDetailRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey = type.findKey("immediate");
    _disclosedTextKey = type.findKey("disclosedText");
    _undisclosedTextKey = type.findKey("undisclosedText");
  }

  /**
   */
  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    
    boolean javaScriptSupport = supportsScripting(arc);
    boolean disclosed = getDisclosed(bean);

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", component);
    if (!isTableAllDisclosure() && !isTableDetailDisclosure())
      renderId(context, component);

    renderPromptStart(context, arc, component, bean);
    String sourceValue = getClientId(context, component);
    String linkId = getLinkId(sourceValue, disclosed);
    
    String parameterString;
    if (javaScriptSupport)
    {
      _renderScripts(context, arc, component);
      parameterString = _generateOnClickString(context, 
                                               arc, 
                                               component, 
                                               bean, 
                                               disclosed);
    }
    else
    { 
      parameterString = _encodedParam(context, 
                                      arc, 
                                      component, 
                                      bean, 
                                      disclosed);

    }
    
    _renderLinkStart(context, arc, bean, parameterString);

    if (linkId != null)
      rw.writeAttribute("id", linkId, null);

    if (!isTableAllDisclosure())
    {
      renderStyleClasses(context, arc, getDisclosureIconLinkStyleClasses());
      renderDisclosureIcon(context, arc, disclosed);
      _renderLinkEnd(context, arc);
    }

    UIComponent prompt = getFacet(component,
                                  CoreShowDetail.PROMPT_FACET);
    if (prompt == null)
    {
      String text = getDisclosureText(arc, bean, disclosed);
      if (text != null)
      {
        if (!isTableAllDisclosure())
          _renderLinkStart(context, arc, bean, parameterString);
        renderStyleClasses(context, arc, getLinkStyleClasses());
        // if navigation is not supported (i.e. printMode) a span-tag is rendered. See _renderLinkStart() and _renderLinkEnd()
        if (javaScriptSupport || !supportsNavigation(arc))
        {
          rw.writeText(text,
                     disclosed ? "disclosedText" : "undisclosedText");
        }
        else
        {
          // Since for Non-JavaScript browsers we render an input tag, set the 
          // value attribute to text
          rw.writeAttribute("value", text, 
                           disclosed ? "disclosedText" : "undisclosedText");
        }
        _renderLinkEnd(context, arc);
      }
    }

    if (isTableAllDisclosure() && prompt != null)
      _renderLinkEnd(context, arc);

    if (prompt != null)
    {
      encodeChild(context, prompt);
    }

    renderPromptEnd(context);

    if (disclosed &&
        !isTableAllDisclosure() &&
        !isTableDetailDisclosure() &&
        !renderAsInline())
    {
      rw.startElement("div", null);
      encodeAllChildren(context, component);
      rw.endElement("div");
    }

    rw.endElement("span");

  }

  protected void renderPromptStart(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start a different span;  this is where all attributes
    // except the ID will go.
    if ( renderAsInline())
      writer.startElement("span", component);
    else
      writer.startElement("div", component);

    renderAllAttributes(context, arc, bean);
  }

  protected void renderPromptEnd(
    FacesContext        context) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if ( renderAsInline())
      writer.endElement("span");
    else
      writer.endElement("div");
  }


  static public void renderDisclosureIcon(
    FacesContext        context,
    RenderingContext arc,
    boolean             disclosed,
    String              disclosedAltTextKey,
    String              undisclosedAltTextKey) throws IOException
  { 
    String key = disclosed ? disclosedAltTextKey : undisclosedAltTextKey;
    // Get the alt text
    String altText = arc.getTranslatedString(key);
    // Since we render input element for Non-JavaScript browsers, we cannot
    // render image element as its child. So set the value attribute of 
    // input element to the icon symbol 
    if (!supportsScripting(arc))
    {
      ResponseWriter rw = context.getResponseWriter();
      String icon = disclosed ? TrinidadRenderingConstants.NON_JS_DETAIL_DISCLOSED_ICON :
                                TrinidadRenderingConstants.NON_JS_DETAIL_UNDISCLOSED_ICON;  
      rw.writeAttribute("title", altText, null);
      rw.writeAttribute("value", icon, null);
      String linkConverter = "border: none; background: inherit;";
      rw.writeAttribute("style", linkConverter,null);
    }
    else
    {
      Icon icon = _getDisclosureIcon(arc, disclosed);
      if (icon != null)
      {
        // Get the align
        String align = OutputUtils.getMiddleIconAlignment(arc);
        // Render the icon with the specified attrs
        OutputUtils.renderIcon(context, arc, icon, altText, align);
      }
    }
  }


  protected void renderDisclosureIcon(
    FacesContext        context,
    RenderingContext arc,
    boolean             disclosed) throws IOException
  {
    renderDisclosureIcon(context, arc, disclosed,
                         _DISCLOSED_TIP_KEY, _UNDISCLOSED_TIP_KEY);
  }

  /**
   * Always render an ID, needed for proper PPR.
   */
  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }


  // Returns the disclosure Icon
  private static Icon _getDisclosureIcon(
    RenderingContext arc,
    boolean             disclosed
    )
  {
    String iconName;
    
    // Requests from Nokia's WebKit-browsers are handled by desktop renderer.
    // But unlike desktop browsers, Nokia's WebKit-browsers don't support icons
    // that are encoded in Unicode.
    // Ideally, we should be able to use different disclosed/undisclosed icons 
    // based on different platforms using the same disclosed/undisclosed 
    // skinning-key, but we don't have such mechanism in Trinidad right now.  
    // Hence, I have created new skinning-keys for Nokia to render text-icons.
    if (isNokiaS60(arc))
    {
      iconName = 
           disclosed 
           ? SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME_FOR_NOKIA_S60
           : SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME_FOR_NOKIA_S60;
    }
    else
    {
      iconName = (disclosed
                  ? SkinSelectors.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME
                  : SkinSelectors.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME);
    }
    
    return arc.getIcon(iconName);
  }

  protected String getDisclosureText(
    RenderingContext arc,
    FacesBean           bean,
    boolean             disclosed)
  {
    String text;
    if (disclosed)
    {
      text = getDisclosedText(bean);
      if (text == null)
      {
        //smo: This functionality was added durring API Merge.  If getDisclosedText is not present
        //we'll render undislosedText text if it's present before rendering the defaults
        text = getUndisclosedText(bean);
        if(text == null)
        {
          text = arc.getTranslatedString(_DISCLOSED_KEY);
        }
      }
    }
    else
    {
      text = getUndisclosedText(bean);
      if (text == null)
      {
        text = getDisclosedText(bean);
        if(text == null)
        {
          text = arc.getTranslatedString(_UNDISCLOSED_KEY);
        }
      }
    }

    return text;
  }

  private void _renderScripts(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component) throws IOException
  {
    if (!supportsNavigation(arc))
      return;

    FormData fData = arc.getFormData();
    if (fData == null)
    {
      _LOG.warning("SHOWDETAIL_NOT_IN_FORM_WILLNOT_FUNCTION_PROPERLY");
      return;
    }

    String valueValue = getValueParameter(component);


    ResponseWriter rw = context.getResponseWriter();
    boolean partial = PartialPageUtils.isPPRActive(context);

    // the first time, render the necessary Javascript
    if (arc.getProperties().get(_SHOW_DETAIL_SUBMIT_JS_RENDERED) == null)
    {
      arc.getProperties().put(_SHOW_DETAIL_SUBMIT_JS_RENDERED,
                              Boolean.TRUE);
      // write the submit function

      String js;
      // Javascript function, optimized to reduce size.  Parameters are:
      // a: form name
      // v: validate?
      // b: event name
      // c: source parameter (id)
      // l: ID of link (for focusing)
      // d: value parameter (for use in table)
      // =-=AEW Why bother including "document" and "window" in
      // _setRequestedFocusNode() call?  It could derive them itself.
      if (partial)
      {
        js =
          "function _submitHideShow(a,v,b,c,l,d) {" +
            "var o = {"+
                  TrinidadRenderingConstants.EVENT_PARAM + ":b," +
                  TrinidadRenderingConstants.SOURCE_PARAM + ":c};" +
            "if (d!=(void 0)) o." +
                  TrinidadRenderingConstants.VALUE_PARAM + "=d;" +
            "_setRequestedFocusNode(document,l,false,window);" +
            "_submitPartialChange(a,v,o);" +
            "return false;}";
      }
      else
      {
        js =
          "function _submitHideShow(a,v,b,c,l,d) {" +
            "var o={"+
                  TrinidadRenderingConstants.EVENT_PARAM + ":b," +
                  TrinidadRenderingConstants.SOURCE_PARAM + ":c};" +
            "if (d!=(void 0)) o." +
                  TrinidadRenderingConstants.VALUE_PARAM + "=d;" +
            "submitForm(a,v,o);" +
            "return false;}";
      }

      // write the submit function
      rw.startElement("script", null);
      renderScriptDeferAttribute(context, arc);
      renderScriptTypeAttribute(context, arc);

      rw.writeText(js, null);
      rw.endElement("script");

      // Add these needed values at most once per page
      fData.addNeededValue(TrinidadRenderingConstants.EVENT_PARAM);
      fData.addNeededValue(TrinidadRenderingConstants.SOURCE_PARAM);
      if (partial)
        fData.addNeededValue(TrinidadRenderingConstants.PARTIAL_PARAM);
    }

    // And add this needed value if it ever comes up
    if (valueValue != null)
      fData.addNeededValue(TrinidadRenderingConstants.VALUE_PARAM);

  }

  private void _renderLinkStart(
    FacesContext        context,
    RenderingContext arc,
    FacesBean bean,
    String           parameterString ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    if (!supportsNavigation(arc)) {
      rw.startElement("span", null);
    }
    else if (supportsScripting(arc))
    {
      String onclick = getOnclick(bean);
      rw.startElement("a", null);
      onclick = XhtmlUtils.getChainedJS(onclick, parameterString, true);
      rw.writeAttribute("onclick", onclick, null);
      rw.writeURIAttribute("href", "#", null);
    }
    // For Non-JavaScript browsers, render an input element(type=submit) to 
    // submit the page. Encode the name attribute with the parameter name 
    // and value thus it would enable the browsers to include the name of 
    // this element in its payLoad if it submits the page.
    else
    {
      rw.startElement("input", null);
      rw.writeAttribute("type", "submit", null);
      rw.writeURIAttribute("name", parameterString, null);
    }
  }

  private String _generateOnClickString(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    boolean             disclosed)
  {
    FormData fData = arc.getFormData();
    if (fData == null)
       return null;

    String sourceValue = getClientId(context, component);
    String formName = fData.getName();
    String eventValue = (disclosed
                         ? TrinidadRenderingConstants.HIDE_EVENT
                         : TrinidadRenderingConstants.SHOW_EVENT);
    String valueValue = getValueParameter(component);
    String linkId = getLinkId(sourceValue, disclosed);

    int linkBufferLen = 41 +
                        formName.length() +
                        eventValue.length() +
                        sourceValue.length();

    if (valueValue != null)
      linkBufferLen += valueValue .length();
    else
      linkBufferLen += 4;


    StringBuffer linkBuffer = new StringBuffer(linkBufferLen);
    linkBuffer.append("return _submitHideShow('");
    linkBuffer.append(formName);
    linkBuffer.append("',");
    linkBuffer.append(getImmediate(bean) ? '0' : '1');
    linkBuffer.append(",'");
    linkBuffer.append(eventValue);
    linkBuffer.append("','");
    linkBuffer.append(sourceValue);
    linkBuffer.append("',");

    if (linkId != null)
      linkBuffer.append("'").append(linkId).append("'");
    else
      linkBuffer.append("null");

    if (valueValue != null)
      linkBuffer.append(",'").append(valueValue).append("'");

    linkBuffer.append(")");
    return linkBuffer.toString();
  }


  private void _renderLinkEnd(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    if (!supportsNavigation(arc))
      rw.endElement("span");
    else if (supportsScripting(arc))
      rw.endElement("a");
    else
      rw.endElement("input");
  }
  
  /**
   * @return encoded parameter name and value pairs for Non-JavaScript
   * browsers 
   */
  private String _encodedParam(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    boolean             disclosed)
  {
    FormData fData = arc.getFormData();
    if (fData == null)
       return null;
       
    String sourceValue = getClientId(context, component);
    String eventValue = (disclosed
                         ? TrinidadRenderingConstants.HIDE_EVENT
                         : TrinidadRenderingConstants.SHOW_EVENT);
                         
    String valueValue = getValueParameter(component);
    String linkId = getLinkId(sourceValue, disclosed);
  
    String nameAttri = XhtmlUtils.getEncodedParameter
                                  (TrinidadRenderingConstants.SOURCE_PARAM)
                       + XhtmlUtils.getEncodedParameter(sourceValue)
                       + XhtmlUtils.getEncodedParameter
                                  (TrinidadRenderingConstants.EVENT_PARAM)
                       + XhtmlUtils.getEncodedParameter(eventValue)
                       + XhtmlUtils.getEncodedParameter
                                  (TrinidadRenderingConstants.VALUE_PARAM)
                       + valueValue;  
    return nameAttri;
  }

  /**
   * Hook for table;  it'd be cleaner to simply expose
   * more protected hooks in general, and eliminate this
   * Table-ShowDetail dependency.
   */
  protected boolean isTableDetailDisclosure()
  {
    return false;
  }

  /**
   * Hook for table;  it'd be cleaner to simply expose
   * more protected hooks in general, and eliminate this
   * Table-ShowDetail dependency.
   */
  protected boolean isTableAllDisclosure()
  {
    return false;
  }

  protected boolean renderAsInline()
  {
    return false;
  }

  protected String getValueParameter(UIComponent component)
  {
    return null;
  }

  /**
   * Returns the style classes to use for links rendered by the
   * ShowDetailRenderer
   */
  protected String[] getLinkStyleClasses()
  {
    return PROMPT_LINK_STYLE_CLASSES;
  }

  /**
   */
  protected String getLinkId(String rootId, boolean disclosed)
  {
    return XhtmlUtils.getCompositeId(rootId, null);
  }

  protected boolean getImmediate(FacesBean bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }


  protected String getDisclosedText(FacesBean bean)
  {
    // It can be null in the table...
    if (_disclosedTextKey == null)
      return null;

    return toString(bean.getProperty(_disclosedTextKey));
  }

  protected String getUndisclosedText(FacesBean bean)
  {
    // It can be null in the table...
    if (_undisclosedTextKey == null)
      return null;

    return toString(bean.getProperty(_undisclosedTextKey));
  }

  protected String[] getDisclosureIconLinkStyleClasses() {
    return DISCLOSURE_ICON_LINK_STYLE_CLASSES;
  }

  protected String getPromptStyleClass(boolean disclosed) {
    return disclosed
        ? SkinSelectors.AF_SHOW_DETAIL_PROMPT_DISCLOSED_STYLE_CLASS
        : SkinSelectors.AF_SHOW_DETAIL_PROMPT_UNDISCLOSED_STYLE_CLASS;
  }

  protected String getDefaultStyleClass(FacesBean bean) {
    return getPromptStyleClass(getDisclosed(bean));
  }

  private PropertyKey _immediateKey;
  private PropertyKey _disclosedTextKey;
  private PropertyKey _undisclosedTextKey;

  private static final Object _SHOW_DETAIL_SUBMIT_JS_RENDERED =
                             new Object();

  private static final String _DISCLOSED_KEY =
    "af_showDetail.DISCLOSED";
  private static final String _UNDISCLOSED_KEY =
    "af_showDetail.UNDISCLOSED";

  private static final String _DISCLOSED_TIP_KEY =
    "af_showDetail.DISCLOSED_TIP";
  private static final String _UNDISCLOSED_TIP_KEY =
    "af_showDetail.UNDISCLOSED_TIP";

  private static final String[] PROMPT_LINK_STYLE_CLASSES =
    {SkinSelectors.LINK_STYLE_CLASS,
     SkinSelectors.AF_SHOW_DETAIL_PROMPT_LINK_STYLE_CLASS};

  private static final String[]DISCLOSURE_ICON_LINK_STYLE_CLASSES =
    {SkinSelectors.LINK_STYLE_CLASS,
     SkinSelectors.AF_SHOW_DETAIL_DISCLOSURE_ICON_LINK_STYLE_CLASS};

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ShowDetailRenderer.class);
}
