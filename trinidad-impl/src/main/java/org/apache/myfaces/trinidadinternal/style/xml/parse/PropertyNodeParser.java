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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for property nodes. This parses the XSS file.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/PropertyNodeParser.java#0 $) $Date: 10-nov-2005.18:58:10 $
 */
public class PropertyNodeParser extends BaseNodeParser
  implements XMLConstants, StyleConstants
{

  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);

  }

  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
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

    return null;
  }

  /**
   * Implementation of NodeParser.addText()
   */
  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length
    )
  {
    String moreText = new String(text, start, length);
    if (_whitespace != null)
    {
      moreText = _whitespace + moreText;
      _whitespace = null;
    }

    if (_value == null)
      _value = moreText;
    else
      _value = _value + moreText;
  }

  @Override
  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
    String whitespace = new String(text, start, length);
    if (_whitespace == null)
      _whitespace = whitespace;
    else
      _whitespace = _whitespace + whitespace;
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
  private String _whitespace;


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
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PropertyNodeParser.class);
}
