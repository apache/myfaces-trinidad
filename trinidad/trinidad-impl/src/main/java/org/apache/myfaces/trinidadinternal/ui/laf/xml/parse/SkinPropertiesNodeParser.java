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
package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import java.util.ArrayList;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;



import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <properties> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertiesNodeParser.java#0 $) $Date: 10-nov-2005.18:50:44 $
 */
public class SkinPropertiesNodeParser extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    return context.getParser(SkinPropertyNode.class, namespaceURI, localName);
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    if (!(child == null) && !(child instanceof SkinPropertyNode))
    {
      throw new IllegalArgumentException(
        "Non Null child and child not an instance of SkinPropertyNode");
    }

    if (child instanceof SkinPropertyNode)
      _properties.add((SkinPropertyNode)child);
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    if (_properties.isEmpty())
      return null;

    SkinPropertyNode[] properties = new SkinPropertyNode[_properties.size()];

    return _properties.toArray(properties);
  }

  private ArrayList<SkinPropertyNode> _properties = new ArrayList<SkinPropertyNode>();
}
