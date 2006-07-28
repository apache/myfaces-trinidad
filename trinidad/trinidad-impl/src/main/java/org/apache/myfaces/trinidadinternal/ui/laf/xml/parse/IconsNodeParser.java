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

import java.util.ArrayList;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;



import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <icons> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/IconsNodeParser.java#0 $) $Date: 10-nov-2005.18:50:38 $
 * @author The Oracle ADF Faces Team
 */
public class IconsNodeParser extends BaseNodeParser
  implements XMLConstants
{
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    return context.getParser(IconNode.class, namespaceURI, localName);
  }

  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    if ((child != null) && !(child instanceof IconNode)) 
    {
      throw new IllegalArgumentException("child is not null and not an instance of IconNode");
    }

    if (child instanceof IconNode)
      _icons.add(child);
  }

  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    if (_icons.isEmpty())
      return null;

    IconNode[] icons = new IconNode[_icons.size()];

    return _icons.toArray(icons);
  }

  private ArrayList _icons = new ArrayList();
}
