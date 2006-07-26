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

package org.apache.myfaces.adfinternal.skin.icon;

import java.io.IOException;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;


import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

import org.apache.myfaces.adfinternal.style.Style;
import org.apache.myfaces.adfinternal.style.util.StyleUtils;

/**
 * An Icon implementation which renders a text string as the icon.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/TextIcon.java#0 $) $Date: 10-nov-2005.18:59:05 $
 * @author The Oracle ADF Faces Team
 */
public class TextIcon extends Icon
{
  /**
   * Creates the TextIcon with the specified text string.
   */
  public TextIcon(String text)
  {
    this(text, null, null, null);
  }

  /**
   * Creates a TextIcon which uses different text depending on the
   * reading direction.
   */
  public TextIcon(
    String text,
    String rtlText
    )
  {
    this(text, rtlText, null, null);
  }

  /**
   * Creates the TextIcon with the specified text string, style class,
   * and inline style.
   */
  public TextIcon(
    String text,
    String rtlText,
    String styleClass,
    Style  inlineStyle
    )
  {
    _text = text;
    _rtlText = rtlText;
    _styleClass = styleClass;
    _inlineStyle = inlineStyle;
  }

  /**
   * Renders the Icon.
   *
   * @param context The RenderingContext for the current request.
   * @param attrs A Map which provides access to
   *             values that might be useful to Icon implementations
   *             TextIcon looks for Icon.ID_KEY, Icon.SHORT_DESC_KEY, and
   *             Icon.EMBEDDED_KEY. It does not render SHORT_DESC_KEY if it
   *             is null or "", because there is no point to this for TextIcons.
   */
  public void renderIcon(
    FacesContext        context,
    AdfRenderingContext arc,
    Map              attrs
    ) throws IOException
  {
    // See if we have an id
    Object id = null;
    Object styleClass = _styleClass;
    Object title = null;
    boolean embedded = false;

    if (attrs != null)
    {
      id = attrs.get(Icon.ID_KEY);

      title = _getTitle(attrs);
      embedded = _isEmbedded(attrs);
    }


    // If we have an id or style information, render the text contents
    // within a span
    ResponseWriter writer = context.getResponseWriter();

    boolean useSpan = _useSpan(styleClass, _inlineStyle, title, embedded);

    if (useSpan)
      writer.startElement("span", null);

    if (id != null)
      writer.writeAttribute("id", id, null);

    // If we have a title that isn't "", render it on the span
    if ((title != null) && (title != ""))
      writer.writeAttribute("title", title, null);

    // Handle style attributes (or elements)

    // =-=jmwIcon @todo: do what XhtmlLafRenderer.renderStyleClassAttribute  
    // =-=jmwIcon need to shorten it!
    // we map the styleClass in case it is used in a composite
    // Then, since we aren't shortening it, which we need to!, we need
    // to at least make sure it is valid.
    if (styleClass != null)
    {
      String convertedStyleClass = 
        StyleUtils.convertToValidSelector(arc.getStyleClass(_styleClass));
   
      writer.writeAttribute("class", convertedStyleClass, null);
    }
  
    if (_inlineStyle != null)
      writer.writeAttribute("style", _inlineStyle.toInlineString(), null);

    

    String text = getText(arc);

  // don't know how to map this back to the source, so using null...
    writer.writeText(text, null);



    if (useSpan)
      writer.endElement("span");
  }

  /**
   * Returns the text to render.
   */
  protected String getText(AdfRenderingContext arc)
  {
    if ((_rtlText != null) && arc.isRightToLeft() )
      return _rtlText;

    return _text;
  }

  // Returns the title text for the icon
  private Object _getTitle(
    Map              attrs
    )
  {
    assert (attrs != null);

    return attrs.get(Icon.SHORT_DESC_KEY);
  }

  private boolean _isEmbedded(
    Map              attrs
    )
  {
    assert (attrs != null);

    return Boolean.TRUE.equals(
             attrs.get(Icon.EMBEDDED_KEY));
  }

  // Tests whether we need to render a span
  private static boolean _useSpan(
    Object styleClass,
    Object inlineStyle,
    Object title,
    boolean embedded
    )
  {
    return (!embedded &&
             ((styleClass != null)  ||
              (title != null && title != "")       ||
              (inlineStyle != null) ||
              (title != null)));
  }

  private String _text;
  private String _rtlText;
  private String _styleClass;
  private Style  _inlineStyle;
}
