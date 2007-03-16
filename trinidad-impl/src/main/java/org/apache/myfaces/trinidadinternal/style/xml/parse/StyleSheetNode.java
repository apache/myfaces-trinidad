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

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;


import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.style.util.ModeUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;


/**
 * Private implementation of StyleSheetNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetNode.java#0 $) $Date: 10-nov-2005.18:58:46 $
 */
public class StyleSheetNode
{
  // A private constructor that we are temporarily using while
  // resolving the now-deprecated color properties
  StyleSheetNode(StyleSheetNode styleSheet, StyleNode[] styles)
  {
    this (styles,
          styleSheet._locales,
          styleSheet._direction,
          styleSheet._browsers,
          styleSheet._versions,
          styleSheet._platforms,
          styleSheet._mode);
  }

  /**
   * Creates a StyleSheetNode with the specified attributes.
   */
  public StyleSheetNode(
    StyleNode[] styles,
    Locale[] locales,
    int direction,
    int[] browsers,
    int[] versions,
    int[] platforms,
    int mode
    )
  {
    if (styles != null)
    {
      _styles = new StyleNode[styles.length];
      System.arraycopy(styles, 0, _styles, 0, styles.length);
    }

    if (locales != null)
    {
      _locales = new Locale[locales.length];
      System.arraycopy(locales, 0, _locales, 0, locales.length);
    }

    _browsers = _copyIntArray(browsers);
    _direction = direction;
    _versions = _copyIntArray(versions);
    _platforms = _copyIntArray(platforms);
    _mode = mode;
  }

  /**
   * Implementation of StyleSheetNode.getStyles().
   */
  public Iterator<StyleNode> getStyles()
  {
    if(_styles!=null)
    {
      return (Arrays.asList(_styles)).iterator();
    }
    else
    {
      List<StyleNode> list = Collections.emptyList();
      return list.iterator();
    }
  }

  /**
   * Implementation of StyleSheetNode.getReadingDirection();
   */
  public int getReadingDirection()
  {
    return _direction;
  }
  
  public int getMode()
  {
    return _mode;
  }

  /**
   * Implementation of StyleSheetNode.getLocales().
   */
  public Iterator<Locale> getLocales()
  {
    if (_locales == null) 
    {
      List<Locale> list = Collections.emptyList();
      return list.iterator();
    }
    else
    {
      return (Arrays.asList(_locales)).iterator();
    }
  }

  /**
   * Implementation of StyleSheetNode.getBrowsers().
   */
  public Iterator<Integer> getBrowsers()
  {
    return Collections.list(new IntegerArrayEnumeration(_browsers)).iterator();
  }

  /**
   * Implementation of StyleSheetNode.getVersions().
   */
  public Iterator<Integer> getVersions()
  {
    return Collections.list(new IntegerArrayEnumeration(_versions)).iterator();
  }

  /**
   * Implementation of StyleSheetNode.getPlatforms().
   */
  public Iterator<Integer> getPlatforms()
  {
    return Collections.list(new IntegerArrayEnumeration(_platforms)).iterator();
  }


  /**
   * Tests whether this StyleSheet matches the specified variants.
   * Returns a number indicating the specificity of the match.
   * Zero means there is no match.  Larger numbers indicate
   * better matches.  The value returned by compareVariants is used to
   * sort style sheets according to precedence.
   * @param mode 
   */
  public int compareVariants(
    Locale locale, 
    int direction, 
    TrinidadAgent agent, 
    int mode)
  {
    int localeMatch = _compareLocale(locale);
    if (localeMatch == 0)
      return 0;

    int directionMatch = _compareDirection(direction);
    if (directionMatch == 0)
      return 0;

    int browser = agent.getAgentApplication();
    int browserMatch = _compareBrowser(browser);
    if (browserMatch == 0)
      return 0;
    int modeMatch = _compareMode(mode);
    if(modeMatch == 0)
      return 0;
    int versionMatch = 0;

    // We only consider version if browser is known.  This allows
    // clients to generate a "default" style sheet - for cases where
    // the browser/version isn't known.  Since there is no Agent
    // VERSION_UNKNOWN constant, we cue off of APPLICATION_UNKNOWN.
    if (browser != TrinidadAgent.APPLICATION_UNKNOWN)
    {
      versionMatch = _compareVersion(agent.getAgentMajorVersion());
      if (versionMatch == 0)
      return 0;
    }
    
    int osMatch = _compareOS(agent.getAgentOS());
    if (osMatch == 0)
      return 0;

    return (localeMatch | browserMatch | versionMatch | osMatch);
  }

  @Override
  public String toString()
  {
    return getClass().getName() + "[" +
      "locales="   + _getLocalesString()   + ", " +
      "direction=" + _getDirectionString() + ", " +
      "browsers="  + _getBrowsersString()  + ", " +
      "versions="  + _getVersionsString()  + ", " +
      "platforms=" + _getPlatformsString() + "]";
  }

  // Compares the specified locale against the supported variants
  private int _compareLocale(Locale locale)
  {
    // If we don't have any locales specified, anything matches
    if (_locales == null)
      return _LOCALE_UNKNOWN_MATCH;

    // On the other hand, if the client-locale is not specified,
    // but we do have a locale specified, there is no match.
    if (locale == null)
      return 0;

    int match = 0;

    for (int i = 0; i < _locales.length; i++)
    {
      Locale tmpLocale = _locales[i];

      if (tmpLocale.getLanguage().equals(locale.getLanguage()))
      {
        if (tmpLocale.getCountry().equals(locale.getCountry()))
        {
          match = _LOCALE_EXACT_MATCH;
          break;
        }

        // If we've got a partial match, keep looking - we may could find
        // an exact match
        match = _LOCALE_PARTIAL_MATCH;
      }
    }

    return match;
  }

  // Compares the specified direction against the supported direction
  private int _compareDirection(int direction)
  {
    // If we don't have a locale specified, we match anything
    if (_direction == LocaleUtils.DIRECTION_DEFAULT)
      return _DIRECTION_UNKNOWN_MATCH;

    // This comparison will return 0 if the client-direction is
    // not specified (ie. if direction == DIRECTION_DEFAULT).
    if (direction == _direction)
      return _DIRECTION_EXACT_MATCH;

    return 0;
  }
  
  private int _compareMode(int mode)
  {
    if (_mode == ModeUtils.MODE_DEFAULT)
      return _MODE_UNKNOWN_MATCH;
    
    if(mode == _mode)
      return _MODE_EXACT_MATCH;
    
    return 0;
  }

  // Compares the specified browser against the supported variants
  private int _compareBrowser(int browser)
  {
    // If we don't have a browser specified, we match anything
    if (_browsers == null)
      return _BROWSER_UNKNOWN_MATCH;

    // On the other hand, if we do have a browser specified, but
    // the client browser is not known, we don't have a match
    if (browser == TrinidadAgent.APPLICATION_UNKNOWN)
      return 0;

    if (_containsInt(browser, _browsers))
      return _BROWSER_EXACT_MATCH;

    return 0;
  }

  // Compares the specified version against the supported variants
  private int _compareVersion(int version)
  {
    if (_versions == null)
      return _VERSION_UNKNOWN_MATCH;

    if (_containsInt(version, _versions))
      return _VERSION_EXACT_MATCH;

    return 0;
  }

  // Compares the specified OS against the supported variants
  private int _compareOS(int os)
  {
    // If we don't have a platform specified, we match anything
    if (_platforms == null)
      return _OS_UNKNOWN_MATCH;

    // On the other hand, if we do have a platform specified, but
    // the client platform is unknown, we don't have a match.
    if (os == TrinidadAgent.OS_UNKNOWN)
      return 0;

    if (_containsInt(os, _platforms))
      return _OS_EXACT_MATCH;

    if (_isUnixPlatform(os) && (_containsInt(__OS_UNIX, _platforms)))
      return _OS_PARTIAL_MATCH;

    return 0;
  }

  // Get a String representing the locales
  private String _getLocalesString()
  {
    if (_locales == null)
      return _EMPTY_STRING;

    StringBuffer buffer = new StringBuffer();
    buffer.append("\"");

    for (int i = 0; i < _locales.length; i++)
    {
      buffer.append(_locales[i].toString());

      if (i < (_locales.length - 1))
        buffer.append(" ");
    }

    buffer.append("\"");

    return buffer.toString();

  }

  // Get a String representing the direction
  private String _getDirectionString()
  {
    if (_direction == LocaleUtils.DIRECTION_DEFAULT)
      return _EMPTY_STRING;
    return NameUtils.getDirectionName(_direction);
  }

  // Get a String representing the browsers
  private String _getBrowsersString()
  {
    if (_browsers == null)
      return _EMPTY_STRING;

    StringBuffer buffer = new StringBuffer();
    buffer.append("\"");

    for (int i = 0; i < _browsers.length; i++)
    {
      String name = NameUtils.getBrowserName(_browsers[i]);
      buffer.append(name);

      if (i < (_browsers.length - 1))
        buffer.append(" ");
    }

    buffer.append("\"");

    return buffer.toString();
  }

  // Get a String representing the versions
  private String _getVersionsString()
  {
    if (_versions == null)
      return _EMPTY_STRING;

    StringBuffer buffer = new StringBuffer();
    buffer.append("\"");

    for (int i = 0; i < _versions.length; i++)
    {
      String name = Integer.toString(_versions[i]);
      buffer.append(name);

      if (i < (_versions.length - 1))
        buffer.append(" ");
    }

    buffer.append("\"");

    return buffer.toString();
  }

  // Get a String representing the platforms
  private String _getPlatformsString()
  {
    if (_platforms == null)
      return _EMPTY_STRING;

    StringBuffer buffer = new StringBuffer();
    buffer.append("\"");

    for (int i = 0; i < _platforms.length; i++)
    {
      String name = NameUtils.getPlatformName(_platforms[i]);
      buffer.append(name);

      if (i < (_platforms.length - 1))
        buffer.append(" ");
    }

    buffer.append("\"");

    return buffer.toString();
  }

  // Tests whether the int n is contained within the int array
  private static boolean _containsInt(int n, int[] array)
  {
    if (array == null)
      return false;

    for (int i = 0; i < array.length; i++)
    {
      if (array[i] == n)
        return true;
    }

    return false;
  }

  // Returns a copy of the int array
  private static int[] _copyIntArray(int[] array)
  {
    if (array == null)
      return null;

    int[] copy = new int[array.length];
    System.arraycopy(array, 0, copy, 0, array.length);

    return copy;
  }

  // Tests whether the specified Agent.OS value is a Unix platform
  private static boolean _isUnixPlatform(int os)
  {
    return (_containsInt(os, _UNIX_PLATFORMS));
  }

  private StyleNode[] _styles;         // The styles contained within this node
  private Locale[]    _locales;        // The locale variants
  private int         _direction;      // The reading direction
  private int[]       _browsers;       // The browser variants
  private int[]       _versions;       // The version variants
  private int[]       _platforms;      // The platform variants
  private int         _mode;           // The mode  

  // Constants for locale matches - 0x000f0000 bits
  private static final int _LOCALE_EXACT_MATCH      = 0x00040000;
  private static final int _LOCALE_PARTIAL_MATCH    = 0x00020000;
  private static final int _LOCALE_UNKNOWN_MATCH    = 0x00010000;

  // Constants for locale matches - 0x0000f000 bits
  private static final int _DIRECTION_EXACT_MATCH   = 0x00002000;
  private static final int _DIRECTION_UNKNOWN_MATCH = 0x00001000;
  
  private static final int _MODE_EXACT_MATCH        = 0x00200000;
  private static final int _MODE_UNKNOWN_MATCH      = 0x00100000;
  
  

  // Constants for browser matches - 0x00000f00 bits
  private static final int _BROWSER_EXACT_MATCH     = 0x00000200;
  private static final int _BROWSER_UNKNOWN_MATCH   = 0x00000100;

  // Constants for version matches - 0x000000f0 bits
  private static final int _VERSION_EXACT_MATCH     = 0x00000020;
  private static final int _VERSION_UNKNOWN_MATCH   = 0x00000020;

  // Constants for os matches - 0x0000000f bits
  private static final int _OS_EXACT_MATCH          = 0x00000004;
  private static final int _OS_PARTIAL_MATCH        = 0x00000002;
  private static final int _OS_UNKNOWN_MATCH        = 0x00000001;

  private static final String _EMPTY_STRING = "\"\"";

  // List of all known Unix platforms.   This array must be updated any
  // time a new Unix OS constant is added Agent.
  private static final int[] _UNIX_PLATFORMS =
  {
    TrinidadAgent.OS_LINUX,
    TrinidadAgent.OS_SOLARIS
  };

  // This special platform constant is used to indicate that the style sheet
  // is Unix-specific, but not specific to a particular Unix OS.  It is
  // package private, as StyleSheetNodeParser references this when
  // creating the int[] platforms array that gets passed in to StyleSheetNode.
  // Agent.OS constants start from 0.  We use Integer.MAX_VALUE to avoid
  // collisions
          static final int    __OS_UNIX = Integer.MAX_VALUE;
}
