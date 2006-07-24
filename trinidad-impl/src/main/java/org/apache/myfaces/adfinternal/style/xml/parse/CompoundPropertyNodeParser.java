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

package org.apache.myfaces.adfinternal.style.xml.parse;

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.adfinternal.share.xml.NodeParser;
import org.apache.myfaces.adfinternal.share.xml.ParseContext;

import org.apache.myfaces.adfinternal.style.StyleConstants;
import org.apache.myfaces.adfinternal.style.xml.XMLConstants;

/**
 * NodeParser for compound property nodes
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/CompoundPropertyNodeParser.java#0 $) $Date: 10-nov-2005.18:58:05 $
 * @author The Oracle ADF Faces Team
 */
public class CompoundPropertyNodeParser extends BaseNodeParser
  implements XMLConstants, StyleConstants
{

  /**
   * Implementation of NodeParser.startElement()
   */
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);
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
    if (_name == null)
      return null;

    Object[] values = null;
    if (_values != null)
    {
      values = new Object[_values.size()];
      _values.copyInto(values);
    }

    return new CompoundPropertyNode(_name, values);
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
    if (localName.equals(XMLConstants.INCLUDE_PROPERTY_NAME))
    {
      _LOG.warning(_INCLUDE_PROPERTY_ERROR);
      return context.getParser(IncludePropertyNode.class,
                               namespaceURI,
                               localName);
    }
    if (localName.equals(XMLConstants.INCLUDE_VALUE_NAME))
    {
      return context.getParser(IncludePropertyNode.class,
                               namespaceURI,
                               localName);
    }
    else if (localName.equals(XMLConstants.VALUE_NAME))
    {
      return context.getParser(String.class, namespaceURI, localName);
    }

    return null;
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
    if (localName.equals(XMLConstants.INCLUDE_PROPERTY_NAME) ||
        localName.equals(XMLConstants.INCLUDE_VALUE_NAME))
    {
      if (!(child instanceof IncludePropertyNode))
      {
        throw new IllegalArgumentException("child not an instanceof IncludePropertyNode");
      }

      if (child instanceof IncludePropertyNode)
      {
        _values.addElement(child);
      }
    }
    else if (localName.equals(XMLConstants.VALUE_NAME))
    {
      if (!(child instanceof String))
      {
        throw new IllegalArgumentException("child not an instance of String");
      }

      if (child instanceof String)
      {
        _values.addElement(child);
      }
    }
    else
    {
      assert false;
    }
  }

  private String  _name;
  private Vector  _values = new Vector();

  private static final String _INCLUDE_PROPERTY_ERROR =
    "The use of the includeProperty element inside of a compoundProperty element is now deprecated.  Please use the includeValue element instead.";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(CompoundPropertyNodeParser.class);
}
