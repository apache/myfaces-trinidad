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

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for class-based Renderers.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/ClassRendererParser.java#0 $) $Date: 10-nov-2005.18:50:36 $
 * @author The Oracle ADF Faces Team
 */
public class ClassRendererParser extends LeafNodeParser implements XMLConstants
{
  /**
   * Override of LeafNodeParser.getNodeValue().
   * Returns a ComponentNode.
   */
  @Override
  protected Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    String name = getRequiredAttribute(context, attrs, NAME_ATTR);
    if (name == null)
      return null;

    try
    {
      // Use ClassLoaderUtils to load the Renderer class
      Class<?> classInstance = ClassLoaderUtils.loadClass(name);

      // Instantiate a new instance
      return classInstance.newInstance();
    }
    catch (Exception e)
    {
      _LOG.warning(e);
    }

    return null;
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ClassRendererParser.class);
}
