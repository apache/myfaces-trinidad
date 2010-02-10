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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for includeProperty nodes. This parses the XSS file.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/IncludePropertyNodeParser.java#0 $) $Date: 10-nov-2005.18:58:08 $
 */
public class IncludePropertyNodeParser extends LeafNodeParser
  implements XMLConstants, StyleConstants
{
  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    String name = attrs.getValue(NAME_ATTR);
    String selector = attrs.getValue(SELECTOR_ATTR);
    String propertyName = getRequiredAttribute(context,
                                               attrs,
                                               PROPERTY_NAME_ATTR);
    String localPropertyName = attrs.getValue(LOCAL_PROPERTY_NAME_ATTR);

    if ((name == null) && (selector == null))
    {
      _LOG.warning(_INCLUDE_PROPERTY_ID_ERROR);
      return null;
    }

    if (propertyName == null)
      return null;

    return new IncludePropertyNode(name,
                                   selector,
                                   propertyName,
                                   localPropertyName);
  }

  private static final String _INCLUDE_PROPERTY_ID_ERROR =
    "<includeProperty> element must specify a selector or name attribute";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(IncludePropertyNodeParser.class);
}
