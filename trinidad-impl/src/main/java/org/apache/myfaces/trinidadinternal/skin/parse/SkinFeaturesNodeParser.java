/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.myfaces.trinidadinternal.skin.parse;

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.share.xml.StringParser;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

/**
 * This class is used when we parse the trinidad-skins.xml file and there is a 'features' element
 * as a child of the 'skin' element. This is a collection of simple 'feature' nodes, represented as Strings
 * @see SkinFeaturesNode
 * 
 * @version $Name:  $ ($Revision: $) $Date:  $
 */
public class SkinFeaturesNodeParser extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _skinFeatures = new HashMap<String, String>();
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    
    return new SkinFeaturesNode(_skinFeatures);
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    if("feature".equals(localName))
    {
      _currentAttributes = attrs;
      return new StringParser();
    }
    else
    { //Clear it out if we are not processing a feature currently
      _currentAttributes = null;
    }
    return null;
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {

    if ("feature".equals(localName) && _currentAttributes != null)
    {
      String name = _currentAttributes.getValue("name");
      if(name != null && String.class.isInstance(child))
      {
        _skinFeatures.put(name, (String)child);
      }
    }
  }

  private Map<String, String> _skinFeatures;
  private Attributes _currentAttributes;

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SkinFeaturesNodeParser.class);
}
