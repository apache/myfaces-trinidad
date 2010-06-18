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
package org.apache.myfaces.trinidadinternal.agent;

import org.apache.myfaces.trinidad.context.Agent;

/**
 * Extension of public Agent interface. Defines constants/method for AdInternal use.
 * Keeping this Interace as is (from before), but extends the public
 * the current (internal) code base uses this heavily.
 *
 */
public interface TrinidadAgent extends Agent
{
  static public final CapabilityKey CAP_DOM =
          CapabilityKey.getCapabilityKey("dom", true);

  static public final CapabilityKey CAP_ONCLICK_IMG_INPUT =
          CapabilityKey.getCapabilityKey("-adfinternal-onclickOnImgInput", true);

  static public final CapabilityKey CAP_XMLDOM =
          CapabilityKey.getCapabilityKey("-adfinternal-xmldom", true);

  static public final CapabilityKey CAP_ID =
          CapabilityKey.getCapabilityKey("-adfinternal-id", true);

  static public final CapabilityKey CAP_ACCESS_KEYS =
          CapabilityKey.getCapabilityKey("accessKeys", true);

  static public final CapabilityKey CAP_PARTIAL_RENDERING =
          CapabilityKey.getCapabilityKey("-adfinternal-partialRendering", true);

  static public final CapabilityKey CAP_DISABLED_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-disabledFormElements", true);

  static public final CapabilityKey CAP_READONLY_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-readonlyFormElements", true);

  static public final CapabilityKey CAP_AUTO_COMPLETE_FORM_ELEMENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-autoCompleteFormElements", true);

  static public final CapabilityKey CAP_ADVANCED_BUTTONS =
          CapabilityKey.getCapabilityKey("-adfinternal-advancedButtons", true);

  static public final CapabilityKey CAP_WIDTH =
          CapabilityKey.getCapabilityKey("width", true);

  static public final CapabilityKey CAP_HEIGHT =
          CapabilityKey.getCapabilityKey("height", true);

  //
  // XHTML Modularization
  //
  static public final CapabilityKey CAP_TEXT_PRESENTATION =
          CapabilityKey.getCapabilityKey("textPresentation", true);

  static public final CapabilityKey CAP_ADVANCED_FORMS =
          CapabilityKey.getCapabilityKey("-adfinternal-advancedForms", true);

  static public final CapabilityKey CAP_TABLES =
          CapabilityKey.getCapabilityKey("tables", true);

  static public final CapabilityKey CAP_FRAMES =
          CapabilityKey.getCapabilityKey("frames", true);

  static public final CapabilityKey CAP_TARGET =
          CapabilityKey.getCapabilityKey("-adfinternal-target", true);

  static public final CapabilityKey CAP_IFRAMES =
          CapabilityKey.getCapabilityKey("iframes", true);

  static public final CapabilityKey CAP_INTRINSIC_EVENTS =
          CapabilityKey.getCapabilityKey("-adfinternal-intrinsicEvents", true);

  static public final CapabilityKey CAP_STYLE_ATTRIBUTES =
          CapabilityKey.getCapabilityKey("-adfinternal-styleAttributes", true);

  static public final CapabilityKey CAP_NAME_IDENTIFICATION =
          CapabilityKey.getCapabilityKey("-adfinternal-nameIdentification", true);

  static public final CapabilityKey CAP_FIELDSET =
          CapabilityKey.getCapabilityKey("-adfinternal-fieldset", true);

  /**
   * capability describing level of support for css selectors
   */
  static public final CapabilityKey CAP_CSS_SELECTORS =
          CapabilityKey.getCapabilityKey("-adfinternal-cssSelectors", true);

  /**
   * true if supports disabling wrapping
   */
  static public final CapabilityKey CAP_NOWRAP =
          CapabilityKey.getCapabilityKey("-adfinternal-nowrap", true);

  /**
   * true if supports vertical alignment
   */
  static public final CapabilityKey CAP_VALIGN =
          CapabilityKey.getCapabilityKey("-adfinternal-valign", true);

  /**
   * true if the alt key renders a tooltip for an image *
   */
  static public final CapabilityKey CAP_ALT_RENDERS_TOOLTIP_ON_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-altRendersTooltipOnImage", true);

  static public final CapabilityKey CAP_SCRIPTING_SPEED =
          CapabilityKey.getCapabilityKey("-adfinternal-scriptingSpeed", true);


  /**
   * true if multiple windows can be opened
   */
  static public final CapabilityKey CAP_MULTIPLE_WINDOWS =
          CapabilityKey.getCapabilityKey("-adfinternal-multipleWindows", true);


  // True if agent supports page navigation
  static public final CapabilityKey CAP_NAVIGATION =
          CapabilityKey.getCapabilityKey("-adfinternal-navigation", true);

  // True if agent supports editing
  static public final CapabilityKey CAP_EDITING =
          CapabilityKey.getCapabilityKey("-adfinternal-editing", true);

  // True if agent supports image stretching, ie. setting the img
  // width/height to a percentage.
  static public final CapabilityKey CAP_IMAGE_STRETCH =
          CapabilityKey.getCapabilityKey("-adfinternal-imageStretch", true);

  static public final CapabilityKey CAP_GIF_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-gifImage", true);

  static public final CapabilityKey CAP_JPEG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-jpegImage", true);

  static public final CapabilityKey CAP_PNG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-pngImage", true);

  static public final CapabilityKey CAP_TRANSPARENT_PNG_TYPE_IMAGE =
          CapabilityKey.getCapabilityKey("-adfinternal-transparentPngImage", true);

  static public final CapabilityKey CAP_BMP_TYPE_IMAGE =
               CapabilityKey.getCapabilityKey("-adfinternal-bmpImage", true);

  static public final CapabilityKey CAP_SUPPORTS_DISABLED_OPTIONS = CapabilityKey
    .getCapabilityKey("-adfinternal-supportsDisabledOptions", true);


  static public final CapabilityKey CAP_IS_JDEV_VE = CapabilityKey
    .getCapabilityKey("-adfinternal-isJDevVE", true);

  static public final CapabilityKey CAP_IS_JDEV_JAVASCRIPT_VE = CapabilityKey
    .getCapabilityKey("-adfinternal-isJDevJavascriptVE", true);

  //
  // Values for CAP_DOM
  //
  static public Object DOM_CAP_NONE    =
          CapabilityValue.getCapabilityValue (CAP_DOM, "none");
  static public Object DOM_CAP_FORM    =
          CapabilityValue.getCapabilityValue (CAP_DOM, "form");
  static public Object DOM_CAP_LEVEL_1 =
          CapabilityValue.getCapabilityValue (CAP_DOM, "level1");
  static public Object DOM_CAP_LEVEL_2 =
          CapabilityValue.getCapabilityValue (CAP_DOM, "level2");

  //
  // Values for CAP_SCRIPTING_SPEED
  //
  static public Object SCRIPTING_SPEED_CAP_NONE =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED, "none");
  static public Object SCRIPTING_SPEED_CAP_SLOW =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED, "slow");
  static public Object SCRIPTING_SPEED_CAP_FAST =
          CapabilityValue.getCapabilityValue (CAP_SCRIPTING_SPEED,"fast");

  //
  // Values for CAP_TABLES
  //
  static public Object TABLES_CAP_BASIC          =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "basic");
  static public Object TABLES_CAP_ADVANCED_ATTRS =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "advanced_attrs");
  static public Object TABLES_CAP_ADVANCED       =
          CapabilityValue.getCapabilityValue (CAP_TABLES, "advanced");

  //
  // Values for CAP_STYLE_ATTRIBUTES
  //
  static public Object STYLES_NONE               =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "none");
  static public Object STYLES_INTERNAL           =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "internal");
  static public Object STYLES_EXTERNAL           =
          CapabilityValue.getCapabilityValue (CAP_STYLE_ATTRIBUTES, "external");

  // Values for CAP_CSS_SELECTORS
  static public Object SELECTORS_NONE            =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "none");
  static public Object SELECTORS_SINGLE          =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "single");
  static public Object SELECTORS_MULTIPLE        =
          CapabilityValue.getCapabilityValue (CAP_CSS_SELECTORS, "multiple");

  /**
   * Application constant for Desktop devices
   */
  static public final int TYPE_DESKTOP = 0;

  /**
   * Application constant for Phone-sized devices
   */
  static public final int TYPE_PHONE = 1;

  /**
   * Application constant for Palm-sized devices.  Pocket-PC,
   * Palm
   */
  static public final int TYPE_PDA = 2;

  /**
   * Application constant for voice
   */
  static public final int TYPE_VOICE = 3;



  /**
   * Application constant for an entirely unknown application.
   */
  static public final int APPLICATION_UNKNOWN   = 0;

  /**
   * Application constant for the Netscape Navigator browser.
   * Note that Netscape 6 is considered as Mozilla, since
   * its rendering engine is that of the Mozilla project.
   */
  static public final int APPLICATION_NETSCAPE  = 1;

  /**
   * Application constant for the Microsoft Internet Explorer
   * browser.
   */
  static public final int APPLICATION_IEXPLORER = 2;

  /**
   * Application constant for browsers based on the Gecko Layout Engine,
   * eg: Mozilla, Netscape 7.0+
   */
  static public final int APPLICATION_GECKO   = 3;

  /**
   * Application constant for the Mozilla browser, or browsers
   * based on it (like Netscape 6).
   * @deprecated since 2.2.0. Use {@link #APPLICATION_GECKO}.
   */
  @Deprecated
  static public final int APPLICATION_MOZILLA   = APPLICATION_GECKO;

  /**
   * Application constant for Palm Web Clippings
   */
  static public final int APPLICATION_WEB_CLIPPING = 4;

  /**
   * Application constant for the ICE Browser
   */
  static public final int APPLICATION_ICE = 5;

  /**
   * Application constant for the Pixo Microbrowser
   */
  static public final int APPLICATION_PIXO = 6;

  /**
   * Application constant for a WML Microbrowser
   */
  static public final int APPLICATION_WML = 7;

  /**
   * Application constant for SimpleResult intermediate Form
   */
  static public final int APPLICATION_SIMPLE_RESULT = 8;

  /**
   * Application constant for iAS wireless (PTG) client
   */
  static public final int APPLICATION_PTG = 9;

  /**
   * Application constant for the NetFront browser.
   */
  static public final int APPLICATION_NET_FRONT = 10;

  /**
   * Application constant for the Safari browser.
   */
  static public final int APPLICATION_SAFARI = 11;

  /**
   * Application constant for the BlackBerry browser.
   */
  static public final int APPLICATION_BLACKBERRY = 12;

  /**
   * Application constant for the Nokia S60 browser.
   */
  static public final int APPLICATION_NOKIA_S60 = 13;

  /**
   * Application constant for the basic HTMLbrowser.
   */
  static public final int APPLICATION_GENERICPDA = 14;

  /**
   * Application constant for Konqueror.
   */
  static public final int APPLICATION_KONQUEROR = 15;

  /**
   * OS constant for an unknown operating system.
   */
  static public final int OS_UNKNOWN = 0;

  /**
   * OS constant for any Microsoft Windows version.
   */
  static public final int OS_WINDOWS = 1;

  /**
   * OS constant for Apple MacOS.
   */
  static public final int OS_MACOS   = 2;

  /**
   * OS constant for any Linux version.
   */
  static public final int OS_LINUX   = 3;

  /**
   * OS constant for any Solaris version.
   */
  static public final int OS_SOLARIS = 4;

  /**
   * OS constant for any Palm version.
   */
  static public final int OS_PALM = 5;

  /**
   * OS constant for any Windows Pocket PC
   */
  static public final int OS_PPC = 6;

  /**
   * OS constant for any BlackBerry device
   */
  static public final int OS_BLACKBERRY = 7;

  /**
   * OS constant for all Safari browsers running in the iOS platform (iPhones/iPod/iPad)
   */
  static public final int OS_IPHONE   = 8;

  /**
   * OS constant for Symbian
   */
  static public final int OS_NOKIA_S60   = 9;

  /**
   * OS constant for generic PDA
   */
  static public final int OS_GENERICPDA   = 10;


  /**
   * Name Constant for Netfront agent
   */
  public static final String AGENT_NETFRONT = "netfront";

  /**
   * Name Constant for Netscape agent. Used only for Netscape versions that are not Gecko
   * based such as Netscape 4.7
   */
  public static final String AGENT_NETSCAPE = "netscape";

  /**
   * Name Constant for Palm Webpro agent
   * //@TODO: Check: Isn't webpro same as netfront access
   */
  public static final String AGENT_WEBPRO = "webpro";

  /**
   * Name constant for ICE browser agent
   */
  public static final String AGENT_ICE_BROWSER = "icebrowser";

  /**
   * Name Constant for Pixo agent
   * //@TODO: Check: Are we still supporting Pixo??
   */
  public static final String AGENT_PIXO = "pixo";

  /**
   * Name Constant for OracleAS Wireless.
   * //@TODO: Check: Do we still have to support this??
   */
  public static final String AGENT_PTG = "ptg";

  /**
   * Name Constant for Blazer agent
   */
  public static final String AGENT_BLAZER = "blazer";

  /**
   * Name Constant for Xiino agent
   */
  public static final String AGENT_XIINO = "xiino";

  /**
   * Name Constant for Palm Web clipping (Elaine) agent
   */
  public static final String AGENT_ELAINE = "elaine";

  /**
   * Returns the type of agent to which we're rendering.  Currently,
   * only web browsers are understood.
   */
  public int getAgentType();


  /**
   * Returns the specific application to which we're rendering.
   * Returns APPLICATION_UNKNOWN is the application couldn't
   * be identified.
   */
  public int getAgentApplication();


  /**
   * Returns the major version number of the application, or 0
   * if a version number couldn't be identified.
   */
  public int getAgentMajorVersion();


  /**
   * Returns the full, unparsed version string.  Returns null
   * if no version string could be identified.
   */
  public String getAgentVersion();


  /**
   * Returns the client operating system.  Returns OS_UNKNOWN if the
   * operating system can't be identified.
   */
  public int getAgentOS();

  /**
   * Returns a capability of a TrinidadAgent
   */
  public Object getCapability(CapabilityKey key);

  public Object clone();
}
