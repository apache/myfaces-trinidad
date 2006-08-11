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
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <renderers> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RenderersNodeParser.java#0 $) $Date: 10-nov-2005.18:50:43 $
 * @author The Oracle ADF Faces Team
 */
public class RenderersNodeParser extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    // Check for specified facets
    String facets = attrs.getValue(FACETS_ATTR);

    if (facets != null)
      _facets = XMLUtils.parseNameTokens(facets);
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    return context.getParser(RendererNode.class, namespaceURI, localName);
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    if ((child != null) && !(child instanceof RendererNode))
    {
      throw new IllegalArgumentException("Null child or child not an instance of RendererNode");
    }

    if (child instanceof RendererNode)
      _renderers.add((RendererNode)child);
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    if (_renderers.isEmpty())
      return null;

    RendererNode[] renderers = new RendererNode[_renderers.size()];
    renderers = _renderers.toArray(renderers);

    return new RenderersNode(renderers, _facets);
  }

  private ArrayList<RendererNode> _renderers = new ArrayList<RendererNode>();
  private String[]  _facets;
}
