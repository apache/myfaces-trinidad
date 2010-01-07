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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;



import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for style nodes. This parses the XSS file.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleNodeParser.java#0 $) $Date: 10-nov-2005.18:58:12 $
 */
public class StyleNodeParser extends BaseNodeParser
  implements XMLConstants, StyleConstants
{
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
    _name = attrs.getValue(NAME_ATTR);
    _selector = attrs.getValue(SELECTOR_ATTR);
    _resetProperties = "true".equals(attrs.getValue(RESET_PROPERTIES_ATTR));

    // Name or selector should be specified
    if ((_name == null) && (_selector == null) && _LOG.isWarning())
      _LOG.warning("ELEMENT_MUST_HAVE_NAME_ATTRIBUTE");
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
    if ((_name == null) && (_selector == null))
       return null;

    PropertyNode[] properties = null;
    if (_properties != null)
    {
      properties = new PropertyNode[_properties.size()];
      _properties.copyInto(properties);
    }

    IncludeStyleNode[] includedStyles = null;
    if (_includedStyles != null)
    {
      includedStyles = new IncludeStyleNode[_includedStyles.size()];
      _includedStyles.copyInto(includedStyles);
    }

    IncludePropertyNode[] includedProperties = null;
    if (_includedProperties != null)
    {
      includedProperties = new IncludePropertyNode[_includedProperties.size()];
      _includedProperties.copyInto(includedProperties);
    }

    return new StyleNode(_name,
                         _selector,
                         properties,
                         includedStyles,
                         includedProperties,
                         null,
                         _resetProperties);
  }

  /**
   * Implementation of NodeParser.startChildElement()
   */
  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    )
  {
    // Delegate property nodes to the appropriate parser
    // We handle included styles ourself
    if (localName.equals(PROPERTY_NAME))
    {
      return context.getParser(PropertyNode.class, namespaceURI, localName);
    }
    else if (localName.equals(INCLUDE_PROPERTY_NAME))
    {
      return context.getParser(IncludePropertyNode.class,
                               namespaceURI,
                               localName);
    }
    else if (localName.equals(INCLUDE_STYLE_NAME))
    {
      String name = attrs.getValue(NAME_ATTR);
      String selector = attrs.getValue(SELECTOR_ATTR);

      if ((name == null) && (selector == null))
      {
        _LOG.warning(_INCLUDE_STYLE_ID_ERROR);
      }
      else
      {
        if (_includedStyles == null)
          _includedStyles = new Vector<IncludeStyleNode>();

        _includedStyles.addElement(new IncludeStyleNode(name, selector));
      }

      return BaseNodeParser.getIgnoreParser();
    }
    else
    {
      return null;
    }
  }

  /**
   * Implementation of NodeParser.addCompletedChild().
   */
  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    if (localName.equals(PROPERTY_NAME))
    {
      if (child != null)
      {
        if (_properties == null)
          _properties = new Vector<PropertyNode>();

        if (!(child instanceof PropertyNode))
        {
          throw new IllegalArgumentException(_LOG.getMessage(
            "CHILD_NOT_PROPERTYNODE_INSTANCE"));
        }

        if (child instanceof PropertyNode)
          _properties.addElement((PropertyNode)child);
      }
    }
    else if (localName.equals(INCLUDE_PROPERTY_NAME))
    {
      if (!(child instanceof IncludePropertyNode))
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "CHIL_NOT_INCLUDEPROEPRTYNODE_INSTANCE"));
      }

      if (child instanceof IncludePropertyNode)
      {
        if (_includedProperties == null)
          _includedProperties = new Vector<IncludePropertyNode>();

        _includedProperties.addElement((IncludePropertyNode)child);
      }
    }
  }

  private String  _name;
  private String  _selector;
  private boolean _resetProperties;
  // -= Simon Lessard =-
  // TODO: Check if synchronization is truly required
  private Vector<PropertyNode>  _properties;
  private Vector<IncludeStyleNode>  _includedStyles;
  private Vector<IncludePropertyNode>  _includedProperties;

  // Error messages
  private static final String _INCLUDE_STYLE_ID_ERROR =
    "<includeStyle> element must specify a selector or name attribute";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleNodeParser.class);
}
