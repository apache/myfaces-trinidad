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

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for CSSStyles.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/CSSStyleParser.java#0 $) $Date: 10-nov-2005.18:58:05 $
 */
public class CSSStyleParser extends BaseNodeParser
{
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
    return new CSSStyle(_properties);
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    if ((_propertyName == null) &&
         (XMLConstants.PROPERTY_NAME.equals(localName)))
    {
      _propertyName = getRequiredAttribute(context,
                                           attrs,
                                           XMLConstants.NAME_ATTR);
      return this;
    }

    // If we are already parsing a property element, or if the child
    // is something other than a property element, don't parse it.
    return null;
  }

  @Override
  public void endChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName) throws SAXParseException
  {
    if (_propertyName != null)
    {
      if ((_propertyValue == null) || (_propertyValue.length() == 0))
        _properties.remove(_propertyName);
      else
        _properties.put(_propertyName, _propertyValue);
    }

    _propertyName = null;
    _propertyValue = null;
  }

  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
    if (_propertyName != null)
    {
      String value = new String(text, start, length);

      if (_propertyValue == null)
        _propertyValue = value;
      else
        _propertyValue += value;
    }
  }

  // The properties that have already been parsed
  private ArrayMap<String, String> _properties = 
    new ArrayMap<String, String>(1);

  // The name of the current property
  private String    _propertyName;

  // The value of the current property
  private String    _propertyValue;
}
