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
package org.apache.myfaces.trinidadinternal.share.config;

import java.util.TimeZone;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.context.RequestContext;

/** 
 * UIXCookie encapsulates the pieces of UIX global state that
 * can be stored in a cookie.  UIXCookie currently supports
 * the following state:
 * <ul>
 *   <li>The accessibility mode chosen by the user
 *   <li>The TimeZone of the user
 * </ul>
 * <p>
 * The cookie used by UIX contains only values that have no security
 * requirements and apply broadly and persistently.  Consequently,
 * it persists across browser sessions, and has its domain and path
 * scoped as broadly as possible.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/config/UIXCookie.java#0 $) $Date: 10-nov-2005.19:00:21 $
 */
public class UIXCookie
{
  /**
   * Gets the UIXCookie, creating it if needed.  This
   * method must be called before starting to write the response.
   * @param request the current servlet request
   * @param response the current servlet response
   */
  static public UIXCookie getUIXCookie(
    HttpServletRequest  request,
    HttpServletResponse response)
  {
    return getUIXCookie(request, response, true);
  }


  /**
   * Gets the UIXCookie.  If "createIfNew" is true, this
   * method must be called before starting to write the response.
   * @param request the current servlet request
   * @param response the current servlet response
   * @param createIfNew if true, the cookie will be created if it
   *   does not already exist;  otherwise, this function will return
   *   null if no cookie is present.
   */
  static public UIXCookie getUIXCookie(
    HttpServletRequest  request,
    HttpServletResponse response,
    boolean             createIfNew)
  {
    if (request == null)
      return null;

    UIXCookie cookie = (UIXCookie) request.getAttribute(_COOKIE_KEY);
    if ((cookie == null) && (response != null))
    {
      if (createIfNew || _cookieExists(request))
      {
        cookie = new UIXCookie(request, response);
        request.setAttribute(_COOKIE_KEY, cookie);
      }
    }

    return cookie;
  }


  /**
   * Get the TimeZone stored on the cookie.
   */
  public TimeZone getTimeZone()
  {
    return _timeZone;
  }


  /**
   * Set the TimeZone on the cookie.
   * <p>
   * The TimeZone cannot be a custom zone.  In particular,
   * <code>TimeZone.getTimeZone(zone.getID())</code>
   * must return the time zone.
   */
  public void setTimeZone(TimeZone zone)
  {
    // =-=AEW Might be worthwhile handling custom time zones
    // by converting them to "GMT+-XX:YY" form.
    if (((_timeZone == null) && (zone != null)) ||
        ((_timeZone != null) && !_timeZone.equals(zone)))
    {
      _timeZone = zone;
      _updateValue();
    }
  }


  /**
   * Get the AccessibilityMode stored on the cookie.
   */
  public RequestContext.Accessibility getAccessibilityMode()
  {
    return _accessibilityMode;
  }


  /**
   * Set the AccessibilityMode on the cookie.
   */
  public void setAccessibilityMode(RequestContext.Accessibility accessibilityMode)
  {
    if (accessibilityMode != _accessibilityMode)
    {
      _accessibilityMode = accessibilityMode;
      _updateValue();
    }
  }


  //
  // Returns true if the cookie exists.
  //
  static private boolean _cookieExists(HttpServletRequest request)
  {
    Cookie[] cookies = request.getCookies();
    if (cookies != null)
    {
      for (int i = 0; i < cookies.length; i++)
      {
        if (_COOKIE_NAME.equals(cookies[i].getName()))
          return true;
      }
    }

    return false;
  }

  //
  // Creates the UIXCookie
  //
  private UIXCookie(
    HttpServletRequest  request,
    HttpServletResponse response)
  {
    Cookie[] cookies = request.getCookies();
    if (cookies != null)
    {
      for (int i = 0; i < cookies.length; i++)
      {
        if (_COOKIE_NAME.equals(cookies[i].getName()))
        {
          _cookie = cookies[i];
          _decodeValues(_cookie.getValue());
          break;
        }
      }
    }

    _response = response;
    _request  = request;
  }

  //
  // Lazily instantiate the Cookie object.
  // 
  private Cookie _getCookie()
  {
    if (_cookie == null)
      _cookie = new Cookie(_COOKIE_NAME, "");

    return _cookie;
  }

  //
  // Updates the value of the cookie after any change.
  //
  private void _updateValue()
  {
    Cookie cookie = _getCookie();
    String value  = _encodeValue();
    _setCookieValue(cookie, value);
  }
  

  //
  // Set the cookie value.  Note that _every_ time you set the value
  // of the cookie, you have to set all of the accompanying properties;
  // if not, they will default.
  //
  private void _setCookieValue(Cookie cookie, String value)
  {
    cookie.setPath("/");
    String domain = _request.getServerName();


    // Scope the cookie to the widest allowable domain - if it's
    // allowed (which it currently isn't)
    if (_DEFAULT_WIDE_COOKIE_SCOPE && (domain != null))
    {
      int periodIndex = domain.indexOf(".");
      if ((periodIndex > 0) && domain.indexOf(".", periodIndex + 1) > 0)
      {
        String startOfDomain = domain.substring(0, periodIndex);
        try
        {
          Integer.parseInt(startOfDomain);
        }
        catch (NumberFormatException nfe)
        {
          domain = domain.substring(periodIndex);
        }
      }
    }
    
    cookie.setDomain(domain);
    cookie.setMaxAge(_MAXIMUM_AGE);
    cookie.setValue(value);

    // Re-add the cookie.
    _response.addCookie(_cookie);
  }

  // 
  // COOKIE ENCODING AND DECODING
  // ----------------------------
  // The UIXCookie is encoded as a caret-delimited series of values:
  //   (1) The version number:  currently the number zero ('0')
  //   (2) Accessibility mode:  one of the empty string, 'D', 'I', or 'S'
  //   (3) The time zone: the ID of the time zone
  //

  //
  // Decodes the cookie.
  //
  private void _decodeValues(String value)
  {
    if ((value == null) || "".equals(value))
      return;
    
    // Since I allow two consecutive carets (to delimit an unspecified
    // string), I can't use StringTokenizer.
    int length    = value.length();
    int nextCaret = value.indexOf('^');
    if (nextCaret < 0)
      return;
    int previousCaret = nextCaret;

    String version = value.substring(0, nextCaret);
    // We identify the version number of the cookie by the first character
    if (version.equals(_VERSION_1))
    {
      nextCaret = value.indexOf('^', previousCaret + 1);
      if (nextCaret < 0)
        nextCaret = length;
      _accessibilityMode = _decodeAccessibilityMode(
                  value.substring(previousCaret + 1, nextCaret));

      if (nextCaret != length)
      {
        previousCaret = nextCaret;
        nextCaret = value.indexOf('^', previousCaret + 1);
        if (nextCaret < 0)
          nextCaret = length;
        _timeZone = _decodeTimeZone(
                    value.substring(previousCaret + 1, nextCaret));
      }
    }
  }

  //
  // Encode the cookie.
  //
  private String _encodeValue()
  {
    StringBuffer buffer = new StringBuffer(40);
    buffer.append(_CURRENT_VERSION);
    buffer.append('^');
    buffer.append(_encodeAccessibilityMode());
    buffer.append('^');
    buffer.append(_encodeTimeZone());
    return buffer.toString();
  }

  private String _encodeAccessibilityMode()
  {
    RequestContext.Accessibility mode = getAccessibilityMode();
    if (mode == null)
      return "";
    if (mode == RequestContext.Accessibility.DEFAULT)
      return "D";
    if (mode == RequestContext.Accessibility.INACCESSIBLE)
      return "I";
    if (mode == RequestContext.Accessibility.SCREEN_READER)
      return "S";

    return "";
  }


  private RequestContext.Accessibility _decodeAccessibilityMode(String value)
  {
    if ("".equals(value))
      return null;
    else if ("D".equals(value))
      return RequestContext.Accessibility.DEFAULT;
    else if ("I".equals(value))
      return RequestContext.Accessibility.INACCESSIBLE;
    else if ("S".equals(value))
      return RequestContext.Accessibility.SCREEN_READER;

    return null;
  }

  private String _encodeTimeZone()
  {
    TimeZone zone = getTimeZone();
    // No time zone set at all == 0
    if (zone == null)
      return "";

    String id = zone.getID();
    /* =-=AEW Code that abbreviates the TimeZone to the shortest
       equivalent.  Probably a bad idea.
    String[] ids = TimeZone.getAvailableIDs(zone.getRawOffset());
    for (int i = 0; i < ids.length; i++)
    {
      String alternative = ids[i];
      if ((alternative.length() < id.length()) &&
          TimeZone.getTimeZone(alternative).hasSameRules(zone))
      {
        id = alternative;
      }
    }
    */

    return id;
  }

  private TimeZone _decodeTimeZone(String value)
  {
    if ((value == null) || "".equals(value))
      return null;

    TimeZone zone = TimeZone.getTimeZone(value);
    // TimeZone.getTimeZone() will default to GMT if it can't
    // find the value.  We DON'T want this behavior.
    if (!value.equals(zone.getID()))
    {
      // However, if the value is "GMT+XX:YY", then TimeZone.getTimeZone()
      // returns as a zone where getID() returns "Custom" - which will be
      // spectacularly useless when we have to encode the zone.
      if (value.startsWith("GMT"))
        zone.setID(value);
      // But, in other cases, if zone.getID() doesn't match the
      // value, that means that TimeZone.getTimeZone() didn't know our
      // time zone at all.  In _that_ case, return null to indicate
      // decoding failed.
      else
        return null;
    }

    return zone;
  }

  private RequestContext.Accessibility _accessibilityMode;
  private TimeZone            _timeZone;
  private Cookie              _cookie;
  private HttpServletRequest  _request;
  private HttpServletResponse _response;

  // Key that the UIXCookie is stored under.
  static private final String _COOKIE_KEY = "org.apache.myfaces.trinidadinternal.share.Cookie";
  static private final String _COOKIE_NAME = "oracle.uix";

  // Maximum age: 10 years
  static private final int    _MAXIMUM_AGE = 60 * 60 * 24 * 365 * 10;

  static private final String _VERSION_1 = "0";
  static private final String _CURRENT_VERSION = _VERSION_1;

  // Current Oracle Cookie design requirements require wide scoping
  // to at least default off
  static private boolean _DEFAULT_WIDE_COOKIE_SCOPE = false;
}
