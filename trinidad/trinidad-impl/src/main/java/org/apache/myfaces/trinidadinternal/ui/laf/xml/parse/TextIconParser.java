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

package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.Style;

import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for TextIcons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/TextIconParser.java#0 $) $Date: 10-nov-2005.18:50:46 $
 * @author The Oracle ADF Faces Team
 */
public class TextIconParser extends BaseNodeParser implements XMLConstants
{
  /**
   * Override of BaseNodeParser.startElement().
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    // Text attr is required
    _text = getRequiredAttribute(context, attrs, TEXT_ATTR);

    // Other attrs are optional
    _rtlText = attrs.getValue(RTL_TEXT_ATTR);
    _styleClass = attrs.getValue(STYLE_CLASS_ATTR);
  }

  /**
   * Override of BaseNodeParser.endElement();
   */
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException
  {
    if (_text == null)
      return null;

    return new TextIcon(_text, _rtlText, _styleClass, _inlineStyle);
  }

  /**
   * Override of BaseNodeParser.startChildElement() for
   * handling <inlineStyle> child elements.
   */
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    if (INLINE_STYLE_NAME.equals(localName))
      return context.getParser(Style.class, namespaceURI, localName);

    return null;
  }

  /**
   * Override of BaseNodeParser.addCompletedChild() for adding
   * <inlineStyle> children.
   */
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child) throws SAXParseException
  {
    _inlineStyle = (Style)child;
  }

  private String _text;
  private String _rtlText;
  private String _styleClass;
  private Style  _inlineStyle;
}
