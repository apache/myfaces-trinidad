/*
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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelPopup;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 * @author Danny Robinson
 */
public class PanelPopupRenderer extends XhtmlRenderer
{
  public PanelPopupRenderer()
  {
    this(CorePanelPopup.TYPE);
  }

  protected PanelPopupRenderer(FacesBean.Type type)
  {
    super(type);
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _titleKey = type.findKey("title");
    _alignmentKey = type.findKey("alignment");
    _modalKey = type.findKey("modal");
    _contentStyleKey = type.findKey("contentStyle");
    _widthKey = type.findKey("width");
    _heightKey = type.findKey("height");
  }

  protected String getText(FacesBean bean)
  {
    return toString(bean.getProperty(_textKey));
  }

  protected String getTitle(FacesBean bean)
  {
    return toString(bean.getProperty(_titleKey));
  }

  protected String getContentStyle(FacesBean bean)
  {
    return toString(bean.getProperty(_contentStyleKey));
  }

  protected int getWidth(FacesBean bean)
  {
    Object o = bean.getProperty(_widthKey);
    if (o == null)
      o = _widthKey.getDefault();

    return toInt(o);
  }

  protected int getHeight(FacesBean bean)
  {
    Object o = bean.getProperty(_heightKey);
    if (o == null)
      o = _heightKey.getDefault();

    return toInt(o);
  }

  protected boolean isModal(FacesBean bean)
  {
    Object o = bean.getProperty(_modalKey);
    if (o == null)
      o = _modalKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean isCentered(FacesBean bean)
  {
    String centeredString = toString(bean.getProperty(_alignmentKey));
    if (centeredString != null)
      return centeredString.equalsIgnoreCase("center");
    return false;
  }

  @Override
  protected String getOnclick(FacesBean bean)
  {
    String onclick = super.getOnclick(bean);
    
    String clientId = RenderingContext.getCurrentInstance().getCurrentClientId();

    StringBuilder script = new StringBuilder();

    script.append("TrPanelPopup.showPopup('");
    script.append(XhtmlUtils.getJSIdentifier(clientId));
    script.append(_POPUP_CONTAINER_ID_SUFFIX); 
    script.append("', '");
    script.append(XhtmlUtils.getJSIdentifier(clientId));
    script.append(_POPUP_TRIGGER_ID_SUFFIX); 
    script.append("', {");
    
    boolean writtenOne = false;

    if (isModal(bean))
    {
      script.append("modal:true");
      writtenOne = true;
    }
    
    if (isCentered(bean))
    {
      if (writtenOne)
        script.append(',');
      else
        writtenOne = true;
      script.append("center:true");
    }

    int width = getWidth(bean);
    if (width > 0)
    {
      if (writtenOne)
        script.append(',');
      else
        writtenOne = true;
      script.append("width:");
      script.append(width);
    } 
    
    int height = getHeight(bean);
    if (height > 0)
    {
      if (writtenOne)
        script.append(',');
      else
        writtenOne = true;
      script.append("height:");
      script.append(height);
    } 
    
    script.append("}, event); return false;");
    
    return XhtmlUtils.getChainedJS(onclick, script.toString(), true);
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(FacesContext context, RenderingContext arc,
      UIComponent component, FacesBean bean) throws IOException
  {
    // Currently, we require scripting to render anything
    if (!supportsScripting(arc))
    {
      _LOG.severe("Agent requires Script Support - unable to render.");
      return;
    }

    ResponseWriter writer = context.getResponseWriter();
    String clientId = getClientId(context, component);
    
    // Make sure we don't have anything to save
    assert(arc.getCurrentClientId() == null);
    // Set current clientId so we can access this in getOnclick 
    arc.setCurrentClientId(clientId);
    
    _renderTrigger(context, arc, component, bean);

    // Render the outer span that is the actual popup container,
    // this element is rendered so the component can be updated via ppr.
    writer.startElement(XhtmlConstants.SPAN_ELEMENT, component);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, 
        XhtmlUtils.getJSIdentifier(clientId), null);

    // render the outer div that is the actual popup container
    writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, XhtmlUtils.getJSIdentifier(clientId)
        + _POPUP_CONTAINER_ID_SUFFIX, null);
    writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
        _POPUP_CONTAINER_DIV_STYLES, null);

    writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
    renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_CONTAINER_STYLE_CLASS);
    writer.writeAttribute("cellspacing", "0", null);

    writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);

    _renderTitleBar(context, arc, component, bean);

    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);

    // table cell that contains the child components
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_CONTENT_STYLE_CLASS);
    // spans both the title and close-icon columns
    writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE, "2", null);

    // render custom styles for content area if specified
    String style = getContentStyle(bean);
    if(style != null)
    {
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, style, null);
    }

    encodeAllChildren(context, component);

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);

    writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);

    writer.endElement(XhtmlConstants.TABLE_ELEMENT);
    
    // Close outer show/hide div
    writer.endElement(XhtmlConstants.DIV_ELEMENT);
    
    // Close outer span
    writer.endElement(XhtmlConstants.SPAN_ELEMENT);
    
    // Reset current clientId
    arc.setCurrentClientId(null);
  }

  protected void _renderTrigger(FacesContext context, RenderingContext arc,
      UIComponent component, FacesBean bean) throws IOException 
  {
    ResponseWriter writer = context.getResponseWriter();

    UIComponent triggerFacet = getFacet(component,
                                        CorePanelPopup.TRIGGER_FACET);

    String text = getText(bean);

    // start rendering 'control'
    writer.startElement(XhtmlConstants.LINK_ELEMENT, null);

    String id = getClientId(context, component);
    writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, 
        XhtmlUtils.getJSIdentifier(id) + _POPUP_TRIGGER_ID_SUFFIX, null);
    writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
    if (triggerFacet != null)
      renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_TRIGGER_STYLE_CLASS);
    else
      renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_LINK_STYLE_CLASS);
    renderAllAttributes(context, arc, bean);

    // Note: render the trigger facet or the text attribute
    // and if neither is set, leave the popup for display by JS

    // Write out the trigger facet if it exists
    if (triggerFacet != null)
    {
      // render 'control' facet
      encodeChild(context, triggerFacet);
    } 
    else if (text != null)
    {
      // render 'text' attribute
      writer.writeText(text, "text");
    }
    
    writer.endElement(XhtmlConstants.LINK_ELEMENT);
  }

  protected void _renderTitleBar(FacesContext context, RenderingContext arc,
      UIComponent component, FacesBean bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String title = getTitle(bean);
    if (title == null)
      return;

    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_TITLEBAR_STYLE_CLASS);
    
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_TITLE_STYLE_CLASS);
    
    writer.writeText(title, "title");

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

    Icon icon = arc.getIcon(SkinSelectors.AF_PANEL_POPUP_CLOSE_ICON_STYLE_CLASS);

    if (isModal(bean))
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, arc, SkinSelectors.AF_PANEL_POPUP_CLOSE_ICON_STYLE_CLASS);

      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);

      StringBuilder script = new StringBuilder();
      script.append("TrPanelPopup.hidePopup(); return false;");

      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, script,
          null);
      
      if (icon != null && !icon.isNull())
      {
        String closeText = arc.getTranslatedString("af_panelPopup.CLOSE");
        OutputUtils.renderIcon(context, arc, icon, closeText, null);
      }
      else
      {
        writer.writeText("X", "text");
      }
      writer.endElement(XhtmlConstants.LINK_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);

  }

  private static final TrinidadLogger _LOG = TrinidadLogger
      .createTrinidadLogger(PanelPopupRenderer.class);

  private PropertyKey _textKey;

  private PropertyKey _titleKey;

  private PropertyKey _alignmentKey;

  private PropertyKey _modalKey;

  private PropertyKey _contentStyleKey;
  
  private PropertyKey _widthKey;
  
  private PropertyKey _heightKey;
  
  private static final String _POPUP_CONTAINER_ID_SUFFIX = "_popupContainer";

  private static final String _POPUP_TRIGGER_ID_SUFFIX = "_popupTrigger";

  /**
   * styles for container element that is shown/hidden. User can't style this
   * element, but there's an inner element for skin styling (e.g border,
   * padding, etc.).
   */
  private static final String _POPUP_CONTAINER_DIV_STYLES = "position: absolute; "
      + "z-index: 201; "
      + "top: 0px; "
      + "left: 0px;  "
      + "visibility:hidden; " + "padding: 0px;  " + "overflow:hidden;";

}
