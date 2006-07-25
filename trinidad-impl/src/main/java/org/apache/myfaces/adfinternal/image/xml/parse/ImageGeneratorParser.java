/*
 * Copyright  2001-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.image.xml.parse;

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.adfinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.adfinternal.share.xml.NodeParser;
import org.apache.myfaces.adfinternal.share.xml.ParseContext;

import org.apache.myfaces.adfinternal.image.ImageConstants;
import org.apache.myfaces.adfinternal.image.ImageProviderRequest;

/**
 * NodeParser for ImageGenerator elements
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/xml/parse/ImageGeneratorParser.java#0 $) $Date: 10-nov-2005.19:04:06 $
 * @author The Oracle ADF Faces Team
 */
public class ImageGeneratorParser extends BaseNodeParser
{

  /**
   * Implementations of NodeParser.startElement();
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    super.startElement(context, namespaceURI, localName, attrs);

    // Enable mnemonic stripping.
    context.setProperty(ImageConstants.TECATE_NAMESPACE,
                     BaseImageProviderRequestParser.__STRIP_MNEMONICS_PROPERTY,
                     Boolean.TRUE);
  }
  /**
   * Implementation of NodeParser.endElement()
   */
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    )
  {
    // Disabled mnemonic stripping
    context.setProperty(ImageConstants.TECATE_NAMESPACE,
                     BaseImageProviderRequestParser.__STRIP_MNEMONICS_PROPERTY,
                     Boolean.FALSE);

    int count = _requests.size();
    ImageProviderRequest[] requests = new ImageProviderRequest[count];
    _requests.copyInto(requests);

    return requests;
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    return context.getParser(ImageProviderRequest.class, 
                             namespaceURI, 
                             localName);
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    _requests.addElement((ImageProviderRequest)child);
  }

  private Vector _requests = new Vector();
}

