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
 * NodeParser for style nodes
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleNodeParser.java#0 $) $Date: 10-nov-2005.18:58:12 $
 * @author The Oracle ADF Faces Team
 */
public class StyleNodeParser extends BaseNodeParser
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
    _name = attrs.getValue(NAME_ATTR);
    _selector = attrs.getValue(SELECTOR_ATTR);
    _resetProperties = "true".equals(attrs.getValue(RESET_PROPERTIES_ATTR));

    // Name or selector should be specified
    if ((_name == null) && (_selector == null) && _LOG.isWarning())
      _LOG.warning("<style> elements must have either a name " +
                   "or a selector attribute");
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
    if ((_name == null) && (_selector == null))
       return null;

    PropertyNode[] properties = null;
    if (_properties != null)
    {
      properties = new PropertyNode[_properties.size()];
      _properties.copyInto(properties);
    }

    CompoundPropertyNode[] compoundProperties = null;
    if (_compoundProperties != null)
    {
      compoundProperties =
        new CompoundPropertyNode[_compoundProperties.size()];
      _compoundProperties.copyInto(compoundProperties);
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
                         compoundProperties,
                         includedStyles,
                         includedProperties,
                         _resetProperties);
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
    // Delegate property/color property nodes to the appropriate parser
    // We handle included styles ourself
    if (localName.equals(PROPERTY_NAME) ||
        localName.equals(COLOR_PROPERTY_NAME))
    {
      return context.getParser(PropertyNode.class, namespaceURI, localName);
    }
    else if (localName.equals(COMPOUND_PROPERTY_NAME))
    {
      return context.getParser(CompoundPropertyNode.class,
                               namespaceURI,
                               localName);
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
          _includedStyles = new Vector();

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
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    )
  {
    if (localName.equals(PROPERTY_NAME)       ||
        localName.equals(COLOR_PROPERTY_NAME))
    {
      if (child != null)
      {
        if (_properties == null)
          _properties = new Vector();

        if (!(child instanceof PropertyNode))
        {
          throw new IllegalArgumentException("child not instance of PropertyNode");
        }

        if (child instanceof PropertyNode)
          _properties.addElement(child);
      }
    }
    else if (localName.equals(COMPOUND_PROPERTY_NAME))
    {
      if (!(child instanceof CompoundPropertyNode))
      {
        throw new IllegalArgumentException("child not instance of CompoundPropertyNode");
      }

      if (child instanceof CompoundPropertyNode)
      {
        if (_compoundProperties == null)
          _compoundProperties = new Vector();

        _compoundProperties.addElement(child);
      }

    }
    else if (localName.equals(INCLUDE_PROPERTY_NAME))
    {
      if (!(child instanceof IncludePropertyNode))
      {
        throw new IllegalArgumentException("child not an instance of IncludePropertyNode");
      }

      if (child instanceof IncludePropertyNode)
      {
        if (_includedProperties == null)
          _includedProperties = new Vector();

        _includedProperties.addElement(child);
      }
    }
  }

  private String  _name;
  private String  _selector;
  private boolean _resetProperties;
  private Vector  _properties;
  private Vector  _compoundProperties;
  private Vector  _includedStyles;
  private Vector  _includedProperties;

  // Error messages
  private static final String _INCLUDE_STYLE_ID_ERROR =
    "<includeStyle> element must specify a selector or name attribute";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(StyleNodeParser.class);
}
