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

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.skin.icon.Icon;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <icon> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/IconNodeParser.java#0 $) $Date: 10-nov-2005.18:50:37 $
 * @author The Oracle ADF Faces Team
 */
public class IconNodeParser extends BaseNodeParser implements XMLConstants
{
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);
  }

  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    // Make sure we only have one Icon per IconNode
    if (_icon != null)
    {
      _LOG.warning(_MULTIPLE_ICONS_ERROR);
      return null;
    }

    return context.getParser(Icon.class, namespaceURI, localName);
  }

  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    _icon = (Icon)child;
  }

  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    // Make sure we have everything we need
    if (_name == null)
      return null;

    return new IconNode(_name, _icon);
  }

  private String _name;
  private Icon   _icon;

  private static final String _MULTIPLE_ICONS_ERROR =
    "The icon element must only contain one child element.";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(IconNodeParser.class);
}
