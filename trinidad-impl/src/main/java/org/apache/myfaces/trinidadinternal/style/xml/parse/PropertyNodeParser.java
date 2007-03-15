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

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;

/**
 * NodeParser for property nodes
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/PropertyNodeParser.java#0 $) $Date: 10-nov-2005.18:58:10 $
 * @author The Oracle ADF Faces Team
 */
public class PropertyNodeParser extends BaseNodeParser
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
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);

    if (XMLConstants.COLOR_PROPERTY_NAME.equals(localName))
      _LOG.warning(_DEPRECATED_ERROR);
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
    if (_name == null)
      return null;

    if (localName.equals(PROPERTY_NAME))
    {
      String error = _validateValue(_name, _value);
      if (error != null)
      {
        _LOG.warning(error);
        return null;
      }
      // resolve urls in the xss
      if (_value!= null  && _isURLValue(_value))
      {
        _value = _resolveURL(_value);
      }
      return new PropertyNode(_name, _value);
    }
    if (localName.equals(COLOR_PROPERTY_NAME))
      return new ColorPropertyNode(_name, _value);

    return null;
  }

  /**
   * Implementation of NodeParser.addText()
   */
  @Override
  public void addText(
    ParseContext context,
    char[]       text,
    int          start,
    int          length
    )
  {
    String moreText = new String(text, start, length);
    if (_whitespace != null)
    {
      moreText = _whitespace + moreText;
      _whitespace = null;
    }

    if (_value == null)
      _value = moreText;
    else
      _value = _value + moreText;
  }

  @Override
  public void addWhitespace(
    ParseContext context,
    char[]       text,
    int          start,
    int          length) throws SAXParseException
  {
    String whitespace = new String(text, start, length);
    if (_whitespace == null)
      _whitespace = whitespace;
    else
      _whitespace = _whitespace + whitespace;
  }

  private static String _resolveURL(
      String url)
  {
    int endIndex = -1;
    int index = url.indexOf("url(");
    StringBuilder builder = new StringBuilder();
    // this loop takes care of the usecase where there can be more than
    // one url, like this: 
    // background-image: url("/skins/purple/images/btns.gif"), 
    // url("/skins/purple/images/checkdn.gif");
    while(index >= 0)
    {
      // Appends values before url()
      builder.append(url, endIndex + 1, index);
      
      endIndex = url.indexOf(')', index + 3);
      String uri = url.substring(index + 4, endIndex);

      // Trim off 
      int uriLength = uri.length();
      if (uriLength > 0)
      {
        if ((uri.charAt(0) == '\'' && uri.charAt(uriLength - 1) == '\'') ||
            (uri.charAt(0) == '"' && uri.charAt(uriLength - 1) == '"'))
        {
          uri = uri.substring(1, uriLength - 1);
          uriLength = uriLength - 2;
        }
      }

      if(uriLength == 0)
      {
        // url() or url('') found, should not happen.
        _LOG.warning("An empty URL was found in selector in xss file");
      }
      
      builder.append("url(");

      String resolvedURI = _resolveCSSURI(uri);
      builder.append(resolvedURI);
      builder.append(')');      

      
      index = url.indexOf("url(", endIndex);
    }
    
    builder.append(url, endIndex + 1, url.length());

    // Don't change anything
    return builder.toString();
  }
  // this is called to resolve the uri that is used in the generated CSS file
  // do not call this method if the selector is an icon selector, since the icon url
  // resolution happens in the ImageIcon classes.
  private static String _resolveCSSURI (
  String uri)
  {
    // defaults to not converting the uri
    // this handles the case where the uri starts with http:
    String resolvedURI = uri;
    FacesContext facesContext = FacesContext.getCurrentInstance();
    assert(facesContext != null);
    ExternalContext externalContext = facesContext.getExternalContext();
    
    if(uri.charAt(0) == '/')
    {
      int uriLength = uri.length();
      // A transformation is required
      if(uriLength > 1 && uri.charAt(1) == '/')
      {
        // Double slashes, trim one and do not add context root before
        resolvedURI = uri.substring(1, uriLength);
      }
      else
      {
        // Single slash, add context path.
        String contextPath = externalContext.getRequestContextPath();
        assert contextPath.charAt(0) == '/';       
        assert contextPath.charAt(contextPath.length() - 1) != '/';

        StringBuilder builder = new StringBuilder(contextPath.length() + uri.length());
        builder.append(contextPath);

        builder.append(uri);
        resolvedURI = builder.toString();
      }
    }
    // now encode the resolved url and return that
    return externalContext.encodeResourceURL(resolvedURI);
  }


  

  // Tests whether the specified property value is an "url" property.
  private static boolean _isURLValue(String propertyValue)
  {
    // URL property values start with "url("
    return propertyValue.startsWith("url(");
  }




  // Validates the value using a PropertyValidater.  Returns an error
  // message if there are validation errors.  Otherwise, returns null
  // if everything is okay.
  private static String _validateValue(String name, String value)
  {
    // Assume name is not null
    assert (name != null);

    PropertyValidater validater = (PropertyValidater)ArrayMap.get(_VALIDATERS,
                                                           name.toLowerCase());

    if (validater != null)
    {
      return validater.validateValue(name, value);
    }

    return null;
  }

  private String _name;
  private String _value;
  private String _whitespace;

  private static final String _DEPRECATED_ERROR =
    "The <colorProperty> element is deprecated.  Colors must be explicitly specified using <property> elements.";

  // PropertyValidater instances, hashed by property name
  private static final Object[] _VALIDATERS =
  {
    "color",             ColorValidater.getInstance(),
    "background-color",  ColorValidater.getInstance(),
    "font-size",         new FontSizeValidater(),
    "font-weight",       new FontWeightValidater(),
    "font-style",        new FontStyleValidater(),
    "text-antialias",    new TextAntialiasValidater(),
  };
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PropertyNodeParser.class);
}
