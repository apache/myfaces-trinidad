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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.Vector;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;
import org.apache.myfaces.trinidadinternal.style.StyleConstants;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

/**
 * NodeParser for style sheet nodes. This class is thread-safe since it is created each time 
 * with newInstance(). Therefore we do not synchronize.
 * This parses the XSS file and creates a StyleSheetNode object.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetNodeParser.java#0 $) $Date: 10-nov-2005.18:58:47 $
 * @see org.apache.myfaces.trinidadinternal.share.xml.ClassParserFactory#getParser
 */
public class StyleSheetNodeParser extends BaseNodeParser
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
    _locales = _initLocales(attrs.getValue(LOCALES_ATTR));
    _direction = NameUtils.getDirection(attrs.getValue(DIRECTION_ATTR));
    _mode = NameUtils.getMode(attrs.getValue(MODE_ATTR));
    _browsers = _initBrowsers(attrs.getValue(BROWSERS_ATTR));
    _versions = _initVersions(attrs.getValue(VERSIONS_ATTR));
    _platforms = _initPlatforms(attrs.getValue(PLATFORMS_ATTR));
    _accProperties = _initAccessibilityProperties(attrs.getValue(ACC_PROFILE_ATTR));
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
    StyleNode[] styles = null;
    if (_styles != null)
    {
      styles = _styles.toArray(new StyleNode[_styles.size()]);
    }
    // Do not create an agentMatcher if there are no browsers or versions to compare against.
    // This way a styleSheetNode will match any browser if it hasn't specified a specific browser.
    AgentAtRuleMatcher agentMatcher = 
      (_browsers.isEmpty()) ? null : new AgentAtRuleMatcher(_browsers, _versions);
  
    return new StyleSheetNode(
        styles,
        null,      // icons only supported in skin CSS - not XSS
        null,      // properties only supported in skin CSS - not XSS
        _locales,
        _direction,
        agentMatcher,
        _platforms,
        _mode,
        _accProperties
        );
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
    if (localName.equals(STYLE_NAME))
      return context.getParser(StyleNode.class, namespaceURI, localName);

    return null;
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
    if (child instanceof StyleNode)
    {
      if (_styles == null)
        _styles = new ArrayList<StyleNode>();

      _styles.add((StyleNode)child);
    }
  }

  // Initialize locales
  private Set<Locale> _initLocales(String localeAttr)
  {
    Iterator<String> tokens = _getTokens(localeAttr);
    
    if (tokens == null)
      return Collections.emptySet();
    
    Set<Locale> locales = new HashSet<Locale>();
    
    while (tokens.hasNext())
    {
      String localeString = tokens.next();
      // TODO: check if the replace of _ with - is really necessary. Also see RequestContextImpl.getFormattingLocale()
      Locale locale = LocaleUtils.getLocaleForIANAString(localeString.replace('_', '-').trim());

      if (locale != null)
        locales.add(locale);
    }
    
    return locales;
  }

  // Initialize browsers
  private List<TrinidadAgent.Application> _initBrowsers(String browserAttr)
  {
    Iterator<String> browsers = _getTokens(browserAttr);
    if (browsers == null)
      return Collections.emptyList();

    List<TrinidadAgent.Application> applications = new ArrayList<TrinidadAgent.Application>();
    
    while (browsers.hasNext())
    {
      TrinidadAgent.Application browser = NameUtils.getAgentApplication(browsers.next());

      if (browser != TrinidadAgent.Application.UNKNOWN)
        applications.add(browser);
    }
    
    return applications;
  }

  // Initialize version
  private Version[] _initVersions(String versionAttr)
  {
    Iterator<String> versions = _getTokens(versionAttr);
    if (versions == null)
      return null;
  
    List<Version> v = new ArrayList<Version>();
    while (versions.hasNext())
    {
      String version = versions.next();
      if (version != null)
      {
        v.add(new Version(version, "*"));
      }
    }
    
    return v.toArray(new Version[v.size()]);
  }

  // Initialize platforms
  private int[] _initPlatforms(String platformAttr)
  {
    Iterator<String> platforms = _getTokens(platformAttr);
    if (platforms == null)
      return null;

    List<Integer> v = new ArrayList<Integer>();
    while (platforms.hasNext())
    {
      String platformName = platforms.next();
      int platform = NameUtils.getPlatform(platformName);

      // If we didn't find the platform, check for special "unix" platform
      if ((platform == TrinidadAgent.OS_UNKNOWN) && "unix".equals(platformName))
        platform = StyleSheetNode.__OS_UNIX;

      if (platform != TrinidadAgent.OS_UNKNOWN)
        v.add(platform);
    }

    return _getIntegers(v);
  }

  // Initialize accessibility profile properties
  private Set<String> _initAccessibilityProperties(String accProfileAttr)
  {
    Iterator<String> tokens = _getTokens(accProfileAttr);
    if (tokens == null)
      return Collections.emptySet();

    // The number of accessibility properties is always small - typically
    // just 1.  Use a small initial capacity.
    Set<String> props = new HashSet<String>(11);

    while (tokens.hasNext())
    {
      String token = tokens.next();
              
      if (NameUtils.isAccessibilityPropertyName(token))
      {
        props.add(token);
      }
      else
      {
        _LOG.warning("INVALID_ACC_PROFILE", new Object[]{token});
      }
    }
    
    return props;
  }

  // Copies Integers from a List into an int array
  private int[] _getIntegers(List<Integer> v)
  {
    int count = v.size();

    if (count == 0)
      return null;

    int[] array = new int[count];

    for (int i = 0; i < count; i++)
      array[i] = v.get(i).intValue();

    return array;
  }

  // Converts a NMTOKENS attribute into the individual tokens
  private Iterator<String> _getTokens(String attr)
  {
    if (attr == null)
      return null;

    return (Arrays.asList(XMLUtils.parseNameTokens(attr))).iterator();
  }

  private List<StyleNode> _styles;
  private Set<Locale>       _locales;
  private int               _direction;
  private int               _mode;
  private List<TrinidadAgent.Application> _browsers;
  private Version[]         _versions;
  private int[]             _platforms;
  private Set<String>       _accProperties;

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StyleSheetNodeParser.class);
}
