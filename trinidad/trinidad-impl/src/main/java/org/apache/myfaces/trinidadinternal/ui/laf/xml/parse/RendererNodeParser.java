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

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NamespaceURI;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;


import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <renderer> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RendererNodeParser.java#0 $) $Date: 10-nov-2005.18:50:41 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RendererNodeParser extends BaseNodeParser implements XMLConstants
{
  @SuppressWarnings("deprecation")
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    String qName = getRequiredAttribute(context, attrs, NAME_ATTR);

    if (qName != null)
    {
      NamespaceURI nsURI = NamespaceURI.create(context,
                                               qName,
                                               UIConstants.MARLIN_NAMESPACE);

      _namespace = nsURI.getNamespace();
      _name = nsURI.getName();
    }
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    // Make sure we only have one Renderer per RendererNode
    if (_renderer != null)
    {
      _LOG.warning(_MULTIPLE_RENDERERS_ERROR);
      return null;
    }

    return context.getParser(Renderer.class, namespaceURI, localName);
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    _renderer = (Renderer)child;
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    // Make sure we have everything we need
    if ((_name == null) || (_renderer == null))
      return null;

    return new RendererNode(_namespace, _name, _renderer);
  }

  private String   _namespace;
  private String   _name;
  private Renderer _renderer;

  private static final String _MULTIPLE_RENDERERS_ERROR =
    "The renderer element must only contain one child element.";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(RendererNodeParser.class);
}
