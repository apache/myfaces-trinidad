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

import org.xml.sax.Attributes;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

/**
 * NodeParser for instance icons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/NullIconParser.java#0 $) $Date: 10-nov-2005.18:50:40 $
 * @author The Oracle ADF Faces Team
 */
public class NullIconParser extends LeafNodeParser
{
  protected Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    return null;
  }
}
