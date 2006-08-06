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
import javax.faces.context.ResponseWriter;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.render.TypedRenderer;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContext;
import org.apache.myfaces.trinidadinternal.util.FormattedTextParser;

/**
 *  @todo Move "supportsStyleAttributes()", etc., architecture
 */
public class XhtmlRenderer extends CoreRenderer
                           implements TypedRenderer, Cloneable
{
  public static final String TRANSPARENT_GIF = "t.gif";

  protected XhtmlRenderer(FacesBean.Type type)
  {
    findTypeConstants(type);
  }

  /**
   * Clone a Renderer instance with a new Type.
   */
  public Renderer cloneWithType(FacesBean.Type type)
  {
    try
    {
      XhtmlRenderer that = (XhtmlRenderer) clone();
      that.findTypeConstants(type);
      return that;
    }
    catch (CloneNotSupportedException cnse)
    {
      _LOG.severe(cnse);
      return null;
    }
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    _shortDescKey = type.findKey("shortDesc");
    _styleClassKey = type.findKey("styleClass");
    _inlineStyleKey = type.findKey("inlineStyle");
    _onclickKey = type.findKey("onclick");
    _ondblclickKey = type.findKey("ondblclick");
    _onkeydownKey = type.findKey("onkeydown");
    _onkeyupKey = type.findKey("onkeyup");
    _onkeypressKey = type.findKey("onkeypress");
    _onmousedownKey = type.findKey("onmousedown");
    _onmousemoveKey = type.findKey("onmousemove");
    _onmouseoutKey = type.findKey("onmouseout");
    _onmouseoverKey = type.findKey("onmouseover");
    _onmouseupKey = type.findKey("onmouseup");

    _partialTriggersKey = type.findKey("partialTriggers");
  }

  //
  // AGENT CAPABILITY CONVENIENCE METHODS
  //

  static public boolean isDesktop(RenderingContext arc)
  {
    return (arc.getAgent().getAgentType() == TrinidadAgent.TYPE_DESKTOP);
  }

  static public boolean isIE(RenderingContext arc)
  {
    return (arc.getAgent().getAgentApplication() ==
              TrinidadAgent.APPLICATION_IEXPLORER);
  }

  static public boolean isGecko(RenderingContext arc)
  {
    return (arc.getAgent().getAgentApplication() ==
              TrinidadAgent.APPLICATION_GECKO);
  }

  static public boolean isInaccessibleMode(RenderingContext arc)
  {
    return (arc.getAccessibilityMode() ==
              RenderingContext.INACCESSIBLE_MODE);
  }

  static public boolean isScreenReaderMode(RenderingContext arc)
  {
    return (arc.getAccessibilityMode() ==
              RenderingContext.SCREEN_READER_MODE);
  }

  static public boolean supportsScripting(RenderingContext arc)
  {
    Object scriptingSpeed = arc.getAgent().getCapability(
            TrinidadAgent.CAP_SCRIPTING_SPEED);

    return ((scriptingSpeed != null) &&
            (TrinidadAgent.SCRIPTING_SPEED_CAP_NONE != scriptingSpeed));
  }

  static public boolean supportsEditing(RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_EDITING);
    return !Boolean.FALSE.equals(cap);
  }

  /**
   * Returns true if the style attributes should be rendered for this node.
   * <p>
   * Clients should override this method if the the user agent
   * doesn't support style attributes.
   * <p>
   * See section 5.18 of xhtml modularization
   */
  public static boolean supportsStyleAttributes(
          RenderingContext arc
          )
  {
    return (arc.getAgent().getCapability(TrinidadAgent.CAP_STYLE_ATTRIBUTES) !=
            TrinidadAgent.STYLES_NONE);
  }

  static public boolean supportsNavigation(RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_NAVIGATION);
    return !Boolean.FALSE.equals(cap);
  }

  /**
   * Returns true if the agent supports the text presentation module.
   * <p>
   * See section 5.4.1 of xhtml modularization.
   */
  public static boolean supportsTextPresentation(
          RenderingContext arc
          )
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_TEXT_PRESENTATION);
    return !Boolean.FALSE.equals(cap);
  }
  static public boolean supportsAccessKeys(RenderingContext arc)
  {
    // In screen reader mode, disable access keys.  Despite
    // the name, they are currently considered an accessibility
    // liability
    if (isScreenReaderMode(arc))
      return false;

    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_ACCESS_KEYS);
    return !Boolean.FALSE.equals(cap);
  }

  static public final boolean supportsDisabledFormElements(RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_DISABLED_FORM_ELEMENTS);
    return !Boolean.FALSE.equals(cap);

  }

  static public final boolean supportsReadonlyFormElements(RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_READONLY_FORM_ELEMENTS);
    return !Boolean.FALSE.equals(cap);

  }

  static public final boolean supportsAutoCompleteFormElements(
     RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_AUTO_COMPLETE_FORM_ELEMENTS);
    return !Boolean.FALSE.equals(cap);
  }

  static public final boolean supportsSeparateWindow(
    RenderingContext arc)
  {
    return XhtmlUtils.supportsSeparateWindow(arc.getAgent());
  }

  /**
   * Returns true if the agent supports setting the target
   * attribute of other elements.
   * <p>
   * See section 5.12 of xhtml modularization.
   */
  static public final boolean supportsTarget(
     RenderingContext arc)

  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_TARGET);
    return !Boolean.FALSE.equals(cap);
  }



  /**
   * Returns true if the agent supports the xmldom
   */
  public static boolean supportsXMLDOM(
    RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapability(
                    TrinidadAgent.CAP_XMLDOM);
    return Boolean.TRUE.equals(cap);
  }

  //
  // END OF AGENT CAPABILITY CONVENIENCE METHODS
  //


  /**
   * Returns true if the component should render an ID.  Components
   * that deliver events should always return "true".
   * @todo Profile and possibly optimize.
   */
  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    // If there's partial triggers, always render an ID if possible
    FacesBean bean = getFacesBean(component);
    if ((_partialTriggersKey != null) &&
        (bean.getProperty(_partialTriggersKey) != null))
    {
      return true;
    }

    return super.shouldRenderId(context, component);
  }

  /**
   * Render the main renderer-specific attributes:  "title", "class", "style",
   * and all the Javascript attributes. This will render style attributes.
   * @todo Since this is non-final, it can become difficult to
   * re-divide its functionality in a subclass.  Make it final???
   */
  protected void renderAllAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    renderAllAttributes(context, arc, bean, true);
  }


  /**
   * Render the main renderer-specific attributes:  "title", "class", "style",
   * and all the Javascript attributes. Takes a boolean to determine if
   * renderStyleAttributes should be called, which renders "class", "style"
   */
  protected void renderAllAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean,
    boolean             renderStyleAttrs) throws IOException
  {
    renderShortDescAttribute(context, arc, bean);
    renderEventHandlers(context, bean);
    if (renderStyleAttrs)
      renderStyleAttributes(context, arc, bean);

  }
  /**
   * Renders the inline style attribute for the specified node
   */
  public static void renderInlineStyleAttribute(
    FacesContext        context,
    RenderingContext arc,
    String              style
    ) throws IOException
  {
    if (style != null)
    {
      if (supportsStyleAttributes(arc))
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.writeAttribute("style", style, null);
      }

    }
  }

  protected void renderInlineStyle(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    String style = getInlineStyle(bean);
    if (style != null)
    {
      context.getResponseWriter().writeAttribute("style",
                                                 style,
                                                 "inlineStyle");
    }
  }

  protected void renderShortDescAttribute(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    String shortDesc = getShortDesc(bean);
    if (shortDesc != null)
      context.getResponseWriter().writeAttribute("title",
                                                 shortDesc,
                                                 "shortDesc");
  }

  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    renderStyleAttributes(context, arc, bean, null);
  }

  /**
   * When there's a default style class pass it in to this method
   */
  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean,
    String              defaultStyleClass) throws IOException
  {
    String styleClass = getStyleClass(bean);
    if (defaultStyleClass != null)
    {
      if(styleClass!= null )
      {
        defaultStyleClass = arc.getStyleClass(defaultStyleClass);
        styleClass = arc.getStyleClass(styleClass);
        renderStyleClasses( context,
                            arc,
                            new String[]{styleClass, defaultStyleClass});
      }
      else
      {
        defaultStyleClass = arc.getStyleClass(defaultStyleClass);
        context.getResponseWriter().writeAttribute("class",
                                                   defaultStyleClass,
                                                   null);
      }
    }
    else if (styleClass != null)
    {
      styleClass = arc.getStyleClass(styleClass);
      context.getResponseWriter().writeAttribute("class",
                                                 styleClass,
                                                 "styleClass");
    }

    String style = getInlineStyle(bean);
    if (style != null)
    {
      context.getResponseWriter().writeAttribute("style",
                                                 style,
                                                 "inlineStyle");
    }
  }

  /**
   * Render a generic styleClass (not one derived from an attribute)
   * @param context  the FacesContext
   * @param styleClass the style class
   */
  static public void renderStyleClass(
    FacesContext        context,
    RenderingContext arc,
    String              styleClass) throws IOException
  {
    if (styleClass != null)
    {
      styleClass = arc.getStyleClass(styleClass);
      context.getResponseWriter().writeAttribute("class", styleClass, null);
    }
  }

  /**
   * Render an array of styleClasses as space-separated values.
   * NOTE: the array is mutated during this method!
   * @param context  the FacesContext
   * @param styleClass the style class
   */
  static public void renderStyleClasses(
    FacesContext        context,
    RenderingContext arc,
    String[]            styleClasses) throws IOException
  {
    int length = 0;
    for (int i = 0; i < styleClasses.length; i++)
    {
      if (styleClasses[i] != null)
      {
        String styleClass = arc.getStyleClass(styleClasses[i]);
        length += styleClass.length() + 1;
        styleClasses[i] = styleClass;
      }
    }

    StringBuffer buffer = new StringBuffer(length);
    for (int i = 0; i < styleClasses.length; i++)
    {
      if (styleClasses[i] != null)
      {
        if (buffer.length() != 0)
          buffer.append(' ');
        buffer.append(styleClasses[i]);
      }
    }

    context.getResponseWriter().writeAttribute("class", buffer.toString(), null);
  }



  /**
   * Render all the Javascript attributes.
   */
  protected void renderEventHandlers(
    FacesContext context,
    FacesBean    bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.writeAttribute("onclick", getOnclick(bean),  "onclick");
    rw.writeAttribute("ondblclick", getOndblclick(bean),  "ondblclick");
    rw.writeAttribute("onkeydown", getOnkeydown(bean),  "onkeydown");
    rw.writeAttribute("onkeyup", getOnkeyup(bean),  "onkeyup");
    rw.writeAttribute("onkeypress", getOnkeypress(bean),  "onkeypress");
    rw.writeAttribute("onmousedown", getOnmousedown(bean),  "onmousedown");
    rw.writeAttribute("onmousemove", getOnmousemove(bean),  "onmousemove");
    rw.writeAttribute("onmouseout", getOnmouseout(bean),  "onmouseout");
    rw.writeAttribute("onmouseover", getOnmouseover(bean),  "onmouseover");
    rw.writeAttribute("onmouseup", getOnmouseup(bean),  "onmouseup");
  }

  protected void renderEncodedActionURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeActionURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }

  protected void renderEncodedResourceURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeResourceURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }


  protected static void renderHAlign(
    FacesContext        context,
    RenderingContext arc,
    Object              hAlign) throws IOException
  {
    if (hAlign != null)
    {
      boolean rtl = arc.isRightToLeft();

      if ("start".equals(hAlign))
      {
        hAlign = (rtl)
                  ? "right"
                  : "left";
      }
      else if ("end".equals(hAlign))
      {
        hAlign = (rtl)
                  ? "left"
                  : "right";
      }

      context.getResponseWriter().writeAttribute("align", hAlign, null);
    }
  }

  //
  // FORMATTED TEXT
  //


  final protected void renderPossiblyFormattedText(
    FacesContext context,
    Object       textValue
    ) throws IOException
  {
    if (textValue != null)
    {
      String textStr = textValue.toString();
      if (_isTextFormatted(textStr))
        _getFormattedTextParser().writeFormattedText(context, textStr);
      else
        context.getResponseWriter().writeText(textStr, null);
    }
  }

  final protected void renderFormattedText(
    FacesContext context,
    Object       textValue) throws IOException
  {
    if (textValue != null)
    {
      String textStr = textValue.toString();
      _getFormattedTextParser().writeFormattedText(context, textStr);
    }
  }

  private boolean _isTextFormatted(String textStr)
  {
    // =-=AEW Should we support "<HTML>" (caps)?
    return textStr.startsWith("<html>");
  }

  /**
   * @todo Move to AdfRenderingContext???
   */
  private FormattedTextParser _getFormattedTextParser()
  {
    return XhtmlFormattedText.getFormattedTextParser();
  }

  //
  // SPACERS AND TRANSPARENT IMAGES
  //

  static public String getAbsoluteImageUri(
     FacesContext        context,
     RenderingContext arc,
     String              imagePath)
  {
    return getBaseImageUri(context, arc) + imagePath;
  }

  /**
   * @todo GET FROM REAL SOURCE?
   * @todo Cache concatentation
   */
  static protected String getBaseImageUri(
    FacesContext        context,
    RenderingContext arc)
  {
    String contextUri = context.getExternalContext().getRequestContextPath();
    return contextUri + "/adf/images/";
  }

  /**
   * Renders an icon.  If the the specified iconUri is a relative URI,
   * it is appended to the value of the base Image URI.
   * This method may only be called for decorative icons - icons
   * that are purely visual.
   */
  protected final void renderDecorativeIcon(
    FacesContext        context,
    RenderingContext arc,
    String              iconUri,
    Object              width,
    Object              height,
    Object              id,
    Object              altText
    ) throws IOException
  {
    renderDecorativeIcon(context, arc, iconUri, width, height,
                         id, altText, null);
  }

  protected final void renderDecorativeIcon(
    FacesContext        context,
    RenderingContext arc,
    String              iconUri,
    Object              width,
    Object              height,
    Object              id,
    Object              altText,
    UIComponent         comp
    ) throws IOException
  {
    // Convert iconUri to an absolute uri
    String absoluteUri = getAbsoluteImageUri(context, arc, iconUri);

    if ((altText == null) && !isInaccessibleMode(arc))
      altText = "";

    OutputUtils.renderImage(context, arc, absoluteUri,
                            width, height, id, altText, comp);
  }


  /**
   * Renders a vertical spacer for a specified non-null height.
   */
  protected final void renderVerticalSpacer(
    FacesContext        context,
    Object              height,
    Object              id,
    UIComponent         comp
    ) throws IOException
  {
    if (height != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", comp);
      writer.writeAttribute("id", id, null);
      String heightString = height.toString();
      if (heightString.length() != 0)
      {
        //pu: CSS mandates specifying units, a crude sanity check if height
        //  does not already contain units, if yes treat it as pixels.
        boolean isUnitsNotSpecified =
          Character.isDigit(heightString.charAt(heightString.length()-1));
        if (isUnitsNotSpecified)
          heightString += "px";
        writer.writeAttribute("style", "margin-top:" + heightString, null);
      }
      writer.endElement("div");
    }
  }

  /**
   * Renders a spacer.
   */
  protected final void renderSpacer(
    FacesContext        context,
    RenderingContext arc,
    String              width,
    String              height
    ) throws IOException
  {
    renderTransparent(context, arc, width, height, false, null);
  }


  /**
   * Renders a transparent gif using a script to save space.
   */
  protected final void renderTransparent(
    FacesContext        context,
    RenderingContext arc,
    String              width,
    String              height,
    boolean             needsQuoting,
    Object              id
    ) throws IOException
  {
    Counter counter =
      (Counter) getRenderingProperty(arc, _SCRIPT_SPACER_COUNT);
    if (counter == null)
    {
      counter = new Counter();
      setRenderingProperty(arc, _SCRIPT_SPACER_COUNT, counter);
    }
    int count = counter.count++;

    // do not use the script spacer, if we have already rendered an enormous
    // number of spacers. bug 3786394:
    boolean useScript =
      ((count < 800)
       && (TrinidadAgent.SCRIPTING_SPEED_CAP_FAST ==
           arc.getAgent().getCapability(TrinidadAgent.CAP_SCRIPTING_SPEED)));
    _renderTransparent(context, arc, width, height, needsQuoting, id, useScript);
  }

  private static final class Counter
  {
    public int count = 0;
  }

  /**
   * @param useScript use javascript to render the spacer. if this is false,
   * then an html IMG tag will be used.
   * @todo fixup call to addLib
   */
  private void _renderTransparent(
    FacesContext        context,
    RenderingContext arc,
    String              width,
    String              height,
    boolean             needsQuoting,
    Object              id,
    boolean             useScript
    ) throws IOException
  {
    PartialPageContext pContext = arc.getPartialPageContext();

    // cannot use t() in MarlinCore.js on a partial rendering pass
    // just render the icon.
    if (!useScript || (pContext != null))
    {
      renderDecorativeIcon(context, arc, TRANSPARENT_GIF,
                           width, height, id, null);
    }
    else
    {
      // IE has fast javascript, so render has a js function call
      ResponseWriter writer = context.getResponseWriter();

      if (getRenderingProperty(arc,
                               _TRANSPARENT_FUNCTION_WRITTEN_KEY) == null)
      {
        // determine the transparent image's URI
        String transparentURI = getAbsoluteImageUri(context, arc, TRANSPARENT_GIF);

        setRenderingProperty(arc,
                             _TRANSPARENT_FUNCTION_WRITTEN_KEY,
                             Boolean.TRUE);

        // make sure the transparent image function is loaded
        XhtmlUtils.addLib(context, arc, "t()");

        writer.startElement("script", null);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        renderScriptTypeAttribute(context, arc);

        writer.writeAttribute("id", id, null);

        // store transparentURI as javascript variable
        // which is used in t()
        writer.write("var _tURL=\"" + transparentURI + "\";");

        // store accessibility mode as javascript variable
        // which is used in t()
        writer.write("var _axm");
        if (!isInaccessibleMode(arc))
          writer.write("=1");
        writer.write(";");
      }
      else
      {
        writer.startElement("script", null);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        renderScriptTypeAttribute(context, arc);
        writer.writeAttribute("id", id, null);
      }

      // write a reference to the transparent gif function
      writer.write("t(");

      if ((width != null) || (height != null))
      {
        String widthParam = "void 0";

        if (width != null)
        {
          widthParam = width;

          if (needsQuoting)
          {
            writer.write("'");
          }
        }

        writer.write(widthParam);

        if (needsQuoting && (width != null))
        {
          writer.write("'");
        }

        if (height != null)
        {
          writer.write(",");

          if (needsQuoting)
          {
            writer.write("'");
          }

          writer.write(height);

          if (needsQuoting)
          {
            writer.write("'");
          }
        }
      }

      writer.write(")");

      writer.endElement("script");
    }
  }

  //
  // JAVASCRIPT RENDERING
  //
  /**
   * Renders the "defer" attribute for a script element.
   * In order to support partial page rendering, scripts must
   * be rendered using the "defer" attribute to ensure that
   * they are executed in the appropriate context.  However,
   * some browsers (eg. IE) do not reliably load scripts
   * which are deferred.  This method detects whether the
   * target agent can handle the defer attribute correctly and
   * only renders this attribute when as appropriate.
   * <p>
   * Note: ResponseWriter.startElement("script", null) must be called
   * before calling this method.
   */
  public static void renderScriptDeferAttribute(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    // At the moment we only render the defer attribute if
    // we are in the partial rendering pass.  This is to
    // avoid the "object expected" JavaScript errors that we
    // see (bug 2251656).  We need to do further browser
    // testing to determine whether we might enable deferred
    // scripts in more cases.

    // Note that we only want to defer scripts once we are actually
    // inside the body content - and writing content to the
    // ScriptBufferingResponseWriter.  Otherwise, we run into bug
    // 2466017.
    if (getRenderingProperty(arc, _DEFER_SCRIPTS_KEY) != null)
      context.getResponseWriter().writeAttribute("defer", Boolean.TRUE, null);
  }

  public static void enableScriptDeferring(RenderingContext arc, boolean isEnabled)
  {
    // This property is used to control whether or not scripts should
    // be deferred.  We only defer scripts if we are rendering a
    // partial page response (or full page response in response to
    // a partial page request).  We could just check for a PartialPageContext,
    // but we actually need to know whether or not we are currently buffering
    // scripts.  We don't want to defer scripts which are rendered before
    // the ScriptBufferingResponseWriter kicks in - such as Core.js -
    // since IE has problems with deferred scripts.  This property is
    // enabled by PanelPartialRootRenderer when rendering the contents
    // of the body.
    Map<Object, Object> props = arc.getProperties();
    if (isEnabled)
    {
      // Turn script deferring on
      props.put(XhtmlRenderer._DEFER_SCRIPTS_KEY, Boolean.TRUE);
    }
    else
    {
      // Turn script deferring off
      props.remove(XhtmlRenderer._DEFER_SCRIPTS_KEY);
    }
  }

  /**
   * Checks whether in screen reader mode, and if so, renders "type" attribute
   * for a script element.
   * <p>
   *
   * Note: ResponseWriter.startElement("script", null) must be called
   * before calling this method.
   *
   * [ =-= mll added 20-Apr-04 to address bug 3426092 ]
   *
   */
  public static void renderScriptTypeAttribute(
    FacesContext        context,
    RenderingContext arc) throws IOException
  {
    if (isScreenReaderMode(arc))
    {
      context.getResponseWriter().writeAttribute("type",
                                                 _ACCESSIBILITY_SCRIPT_TYPE,
                                                 null);
    }
  }



  //
  // ATTRIBUTE HOOKS
  //
  protected String getShortDesc(FacesBean bean)
  {
    return toString(bean.getProperty(_shortDescKey));
  }

  protected String getStyleClass(FacesBean bean)
  {
    return toString(bean.getProperty(_styleClassKey));
  }

  protected String getInlineStyle(FacesBean bean)
  {
    return toString(bean.getProperty(_inlineStyleKey));
  }

  protected String getOnclick(FacesBean bean)
  {
    return toString(bean.getProperty(_onclickKey));
  }

  protected String getOndblclick(FacesBean bean)
  {
    return toString(bean.getProperty(_ondblclickKey));
  }

  protected String getOnkeydown(FacesBean bean)
  {
    return toString(bean.getProperty(_onkeydownKey));
  }

  protected String getOnkeyup(FacesBean bean)
  {
    return toString(bean.getProperty(_onkeyupKey));
  }

  protected String getOnkeypress(FacesBean bean)
  {
    return toString(bean.getProperty(_onkeypressKey));
  }

  protected String getOnmousedown(FacesBean bean)
  {
    return toString(bean.getProperty(_onmousedownKey));
  }

  protected String getOnmousemove(FacesBean bean)
  {
    return toString(bean.getProperty(_onmousemoveKey));
  }

  protected String getOnmouseout(FacesBean bean)
  {
    return toString(bean.getProperty(_onmouseoutKey));
  }

  protected String getOnmouseover(FacesBean bean)
  {
    return toString(bean.getProperty(_onmouseoverKey));
  }

  protected String getOnmouseup(FacesBean bean)
  {
    return toString(bean.getProperty(_onmouseupKey));
  }


  private PropertyKey _shortDescKey;
  private PropertyKey _styleClassKey;
  private PropertyKey _inlineStyleKey;
  private PropertyKey _onclickKey;
  private PropertyKey _ondblclickKey;
  private PropertyKey _onkeydownKey;
  private PropertyKey _onkeyupKey;
  private PropertyKey _onkeypressKey;
  private PropertyKey _onmousedownKey;
  private PropertyKey _onmousemoveKey;
  private PropertyKey _onmouseoutKey;
  private PropertyKey _onmouseoverKey;
  private PropertyKey _onmouseupKey;
  private PropertyKey _partialTriggersKey;

  // The value bound to the type attribute in script tags in accessibilty mode.
  private static final String _ACCESSIBILITY_SCRIPT_TYPE = "text/javascript";

  // Key to look up the number of times we have rendered the script spacer:
  private static final Object _SCRIPT_SPACER_COUNT = new Object();

  // Rendering context key for transparent gif
  // This MUST REMAIN THE SAME VALUE as
  //                 XhtmlLafRenderer.__TRANSPARENT_URL_KEY = "_t.gif";
  private static final String _TRANSPARENT_FUNCTION_WRITTEN_KEY = "_t.gif";

  // =-=AEW THIS MUST BE THE SAME AS THE VALUE IN XhtmlLafRenderer!!!
  private static final String _DEFER_SCRIPTS_KEY = "_defer";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(XhtmlRenderer.class);
}
