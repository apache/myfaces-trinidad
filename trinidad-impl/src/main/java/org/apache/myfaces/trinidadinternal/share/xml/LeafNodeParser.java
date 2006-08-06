/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.share.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

/**
 * An implementation of NodeParser that works only
 * on leaf XML elements - or XML elements that ignore
 * all of their contents.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/xml/LeafNodeParser.java#0 $) $Date: 10-nov-2005.18:59:09 $
 * @author The Oracle ADF Faces Team
 */
abstract public class LeafNodeParser extends BaseNodeParser
{
  /**
   * Creates a LeafNodeParser
   */
  public LeafNodeParser()
  {
  }

  /**
   * Implementation of NodeParser.startElement()
   */
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _value = getNodeValue(context,
                          namespaceURI,
                          localName,
                          attrs);
  }
  
  /**
   * Implementation of NodeParser.endElement()
   */
  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    return _value;
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    return null;
  }

  /**
   * Method called to get the node's value.  Clients
   * must override this method.
   */
  abstract protected Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)  throws SAXParseException;

  private Object _value;
}
