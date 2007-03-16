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

import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * ParserFactory for icon parsers.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/IconParserFactory.java#0 $) $Date: 10-nov-2005.18:50:38 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class IconParserFactory implements ParserFactory
{
  /**
   * Returns the parser for the icon with the 
   * specified namespaceURI/localName.
   */
  public NodeParser getParser(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    if (XMLConstants.CONTEXT_IMAGE_NAME.equals(localName) ||
        XMLConstants.URI_IMAGE_NAME.equals(localName)    ||
        XMLConstants.RESOURCE_IMAGE_NAME.equals(localName)    ||
        XMLConstants.CONFIG_IMAGE_NAME.equals(localName))
    {
      return new ImageIconParser();
    }
    else if (XMLConstants.TEXT_NAME.equals(localName))
    {
      return new TextIconParser();
    }
    else if (XMLConstants.INSTANCE_NAME.equals(localName))
    {
      return new InstanceIconParser();
    }
    else if (XMLConstants.NULL_NAME.equals(localName))
    {
      return new NullIconParser();
    }
    else if (XMLConstants.REFERENCE_NAME.equals(localName))
    {
      return new ReferenceIconParser();
    }

    return null;
  }
}
