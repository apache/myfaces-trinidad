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

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for property nodes
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/PropertyNodeParser.java#0 $) $Date: 10-nov-2005.18:58:10 $
 * @author The Oracle ADF Faces Team
 */
public class PropertyNodeParser extends BaseNodeParser
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

    if (XMLConstants.COLOR_PROPERTY_NAME.equals(localName))
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
    if (_name == null)
      return null;

    if (localName.equals(PROPERTY_NAME))
    {
      String error = _validateValue(_name, _value);
      if (error != null)
      {
        _LOG.warning(error);
        return null;
      }

      return new PropertyNode(_name, _value);
    }
    if (localName.equals(COLOR_PROPERTY_NAME))
      return new ColorPropertyNode(_name, _value);

    return null;
  }

  /**
   * Implementation of NodeParser.addText()
   */
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length
    )
  {
    String moreText = new String(text, start, length);

    if (_value == null)
      _value = moreText;
    else
      _value = _value + moreText;
  }

  // Validates the value using a PropertyValidater.  Returns an error
  // message if there are validation errors.  Otherwise, returns null
  // if everything is okay.
  private static String _validateValue(String name, String value)
  {
    // Assume name is not null
    assert (name != null);

    PropertyValidater validater = (PropertyValidater)ArrayMap.get(_VALIDATERS,
                                                           name.toLowerCase());

    if (validater != null)
    {
      return validater.validateValue(name, value);
    }

    return null;
  }

  private String _name;
  private String _value;

  private static final String _DEPRECATED_ERROR =
    "The <colorProperty> element is deprecated.  Colors must be explicitly specified using <property> elements.";

  // PropertyValidater instances, hashed by property name
  private static final Object[] _VALIDATERS =
  {
    "color",             ColorValidater.getInstance(),
    "background-color",  ColorValidater.getInstance(),
    "font-size",         new FontSizeValidater(),
    "font-weight",       new FontWeightValidater(),
    "font-style",        new FontStyleValidater(),
    "text-antialias",    new TextAntialiasValidater(),
  };
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(PropertyNodeParser.class);
}
