/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * ParserFactory for Renderer parsers.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RendererParserFactory.java#0 $) $Date: 10-nov-2005.18:50:42 $
 * @author The Oracle ADF Faces Team
 */
public class RendererParserFactory implements ParserFactory
{
  /**
   * Returns the parser for the Renderer with the 
   * specified namespaceURI/localName.
   */
  public NodeParser getParser(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    if (XMLConstants.CLASS_NAME.equals(localName))
      return new ClassRendererParser();

    return null;
  }
}
