/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.agent.parse;

import org.xml.sax.SAXException;
import org.xml.sax.InputSource;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.ArrayList;
import java.net.URL;
import java.net.URLConnection;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adfinternal.agent.CapabilityKey;
import org.apache.myfaces.adfinternal.agent.CapabilityValue;
import org.apache.myfaces.adfinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.adfinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.adfinternal.share.xml.TreeBuilder;
import org.apache.myfaces.adfinternal.share.xml.ParseContext;
import org.apache.myfaces.adfinternal.share.xml.NodeParser;

/**
 * CapabilitiesData document parser
 */
public class CapabilityDataDocumentParser  extends BaseNodeParser
        implements XMLConstants
{

  private CapabilityDataDocumentParser()
  {
  }

  static public Object[] parse (URL srcUrl)
  {
    //if URL is null return empty documen
    if (srcUrl == null)
      return new Object[0];

    CapabilityDataDocumentParser parser = new CapabilityDataDocumentParser();
    InputStream stream = null;
    try
    {
      URLConnection connection = srcUrl.openConnection();
      stream = connection.getInputStream();

      InputSource source = new InputSource(stream);
      source.setSystemId(srcUrl.toExternalForm());

      ParseContextImpl pc = new ParseContextImpl ();
      TreeBuilder builder =  new  TreeBuilder();
      return (Object[]) builder.parse(null, source, pc, parser);
    }
    catch (SAXException saxe)
    {
      _LOG.severe("Failed to parse capabilities data document", saxe);
    }
    catch (IOException ioe)
    {
      _LOG.severe("Failed to parse capabilities data document", ioe);
    }
    finally
    {
      try
      {
        if (stream != null)
          stream.close();
      }
      catch (IOException e)
      {
        //do nothing
        ;
      }
    }

    return new Object[0];
  }

  public void startElement (ParseContext context,
                            String       namespaceURI,
                            String       localName,
                            Attributes   attrs )
          throws SAXParseException
  {
    if (!NS_URI.equals(namespaceURI))
    {
      throw new SAXParseException("Invalid Namespace: " +
                                  namespaceURI, context.getLocator());
    }
  }

  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs) throws SAXParseException
  {
    if (ELEMENT_CAPABILITY.equals(localName))
    {
      String name = attrs.getValue(ATTRIBUTE_NAME);
      String value = attrs.getValue(ATTRIBUTE_VALUE);

      if (name != null)
        name = name.trim();
      if (value != null)
        value = value.trim();


      if ((name == null) || (name.length() <= 0) ||
          (value == null) || (value.length() <= 0))
      {
        _LOG.warning("Element " + ELEMENT_CAPABILITIES +
                     " has missing (or empty) attributes");
      }

      CapabilityKey key =
              CapabilityKey.getCapabilityKey(name, true);
      Object valueObject =
              CapabilityValue.getCapabilityValue(key, value);

      _capList.add(key);
      _capList.add(valueObject);

      return this;
    }

    return null;
  }


  public Object endElement (ParseContext context,
                            String       namespaceURI,
                            String       localName)
  {
    if (ELEMENT_CAPABILITY_DATA.equals(localName))
       return _capList.toArray(new Object[_capList.size()]);

    return null;
  }

  private List _capList = new ArrayList();

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(CapabilityDataDocumentParser.class);

}
