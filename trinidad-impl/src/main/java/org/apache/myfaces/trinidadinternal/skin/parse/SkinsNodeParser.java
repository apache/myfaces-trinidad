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

import java.util.ArrayList;

import java.util.List;

import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * NodeParser for &lt;skins&gt; element in trinidad-skins.xml
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinsNodeParser.java#0 $) $Date: 10-nov-2005.18:50:46 $
 */
public class SkinsNodeParser extends BaseNodeParser
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
    if ("skin-addition".equals(localName))
      return context.getParser(SkinAddition.class, namespaceURI, localName);
    else
      return context.getParser(SkinMetadata.class, namespaceURI, localName);
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    assert ((child == null) ||
            (child instanceof SkinMetadata) ||
            (child instanceof SkinAddition));

    // we should not add a child which is null
    // so use both instance checks
    if (child instanceof SkinAddition)
      _skinAdditions.add((SkinAddition)child);
    else if (child instanceof SkinMetadata)
      _skins.add((SkinMetadata)child);
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    return new SkinsNode(_skins,
                         _skinAdditions);
  }

  private List<SkinMetadata> _skins = new ArrayList<SkinMetadata>();
  private List<SkinAddition> _skinAdditions = new ArrayList<SkinAddition>();

}
