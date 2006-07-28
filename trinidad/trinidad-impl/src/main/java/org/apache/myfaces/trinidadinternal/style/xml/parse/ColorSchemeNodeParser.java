/*
 * Copyright  2000-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for ColorSchemeNodes.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/ColorSchemeNodeParser.java#0 $) $Date: 10-nov-2005.18:58:03 $
 * @author The Oracle ADF Faces Team
 */
public class ColorSchemeNodeParser extends BaseNodeParser
  implements XMLConstants, StyleConstants
{
  /**
   * Implementation of NodeParser.startElement()
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);
    _namespace = getRequiredAttribute(context, attrs, NAMESPACE_ATTR);

    _LOG.warning(_DEPRECATED_ERROR);
  }

  /**
   * Implementation of NodeParser.endElement()
   */
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    if ((_name == null) || (_namespace == null))
       return null;

    ColorNode[] colors = null;
    if (_colors != null)
    {
      colors = new ColorNode[_colors.size()];
      _colors.copyInto(colors);
    }

    return new ColorSchemeNode(_namespace, _name, colors);
  }


  /**
   * Implementation of NodeParser.startChildElement()
   */
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    if (localName.equals(COLOR_NAME))
      return context.getParser(ColorNode.class, namespaceURI, localName);

    return null;
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    if (child instanceof ColorNode)
    {
      if (_colors == null)
        _colors = new Vector();

      _colors.addElement(child);
    }
  }

  private String _namespace;
  private String _name;
  private Vector _colors;

  private static final String _DEPRECATED_ERROR =
    "The <colorScheme> element is deprecated.  Colors must be explicitly specified using <property> elements.";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ColorSchemeNodeParser.class);
}
