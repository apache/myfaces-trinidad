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


import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.Style;

import org.apache.myfaces.trinidadinternal.skin.icon.Icon;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.URIImageIcon;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for ImageIcons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/ImageIconParser.java#0 $) $Date: 10-nov-2005.18:50:39 $
 * @author The Oracle ADF Faces Team
 */
public class ImageIconParser extends BaseNodeParser implements XMLConstants
{
  /**
   * Override of LeafNodeParser.getNodeValue().
   * Returns a ComponentNode.
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    // Get URI attrs
    _uri = getRequiredAttribute(context, attrs, URI_ATTR);
    _rtlURI = attrs.getValue(RTL_URI_ATTR);

    // Get size attrs
    _width = _getSizeAttr(attrs, WIDTH_ATTR);
    _height = _getSizeAttr(attrs, HEIGHT_ATTR);

    // Get style class attr
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
    Icon icon = null;

    if (CONTEXT_IMAGE_NAME.equals(localName))
    {
      icon = new ContextImageIcon(_uri,
                                  _rtlURI,
                                  _width,
                                  _height,
                                  _styleClass,
                                  _inlineStyle);
    }
    else if (URI_IMAGE_NAME.equals(localName))
    {
      icon = new URIImageIcon(_uri,
                              _rtlURI,
                              _width,
                              _height,
                              _styleClass,
                              _inlineStyle);
    }

    assert (icon != null);

    return icon;
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

  // Returns the size attribute (width/height) value
  // as an Integer
  private Integer _getSizeAttr(
    Attributes   attrs,
    String       attrName
    )
  {
    String value = attrs.getValue(attrName);
    if (value == null)
      return null;

    try
    {
      int intValue = Integer.parseInt(value);

      // =-=ags Should we check to make sure that we
      //        don't have a negative value?

      return IntegerUtils.getInteger(intValue);
    }
    catch (NumberFormatException e)
    {
      _LOG.warning(e);
    }

    return null;
  }

  private String  _uri;
  private String  _rtlURI;
  private Integer _width;
  private Integer _height;
  private String  _styleClass;
  private Style   _inlineStyle;
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(ImageIconParser.class);
}
