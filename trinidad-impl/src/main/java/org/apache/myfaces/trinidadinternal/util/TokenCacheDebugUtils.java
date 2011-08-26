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
package org.apache.myfaces.trinidadinternal.util;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.WindowManager;


public final class TokenCacheDebugUtils 
{
  
  private TokenCacheDebugUtils(){}


  /**
   * Checks whether we are debugging the token cache.
   * No other method in TokenCacheDebugUtils should be called unless this method returns true.
   */
  public static Boolean debugTokenCache()
  {
    return _DEBUG_TOKEN_CACHE;
  }
  
  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static void addTokenToViewIdMap(String token)
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    Map<String,String> tokenToViewIdMap = getTokenToViewIdMap();      
    UIViewRoot root = context.getViewRoot();
    String viewId = root.getViewId();    
    tokenToViewIdMap.put(token, viewId);
    
    addToRequestStringBuffer("\nADDING " +  getTokenToViewIdString(tokenToViewIdMap, token));
  }

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static void removeTokenFromViewIdMap(String token)
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    Map<String,String> tokenToViewIdMap = getTokenToViewIdMap();  
    
    addToRequestStringBuffer("\nREMOVING " + getTokenToViewIdString(tokenToViewIdMap, token));
    tokenToViewIdMap.remove(token);
  }  


  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static String getTokenToViewIdString(Map<String,String> tokenToViewId, String token)
  {  
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    StringBuffer tokenBuffer = new StringBuffer();
    tokenBuffer.append(token);
    tokenBuffer.append(" (");
    tokenBuffer.append(tokenToViewId.get(token));
    tokenBuffer.append(")");
    
    return tokenBuffer.toString();
  }

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static <V> void logCacheInfo(Map<String, V> targetStore, Map<String, String> pinned, String logAddition)
  {  
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    Map<String,String> tokenToViewId = getTokenToViewIdMap();
    StringBuffer logString = new StringBuffer();

    if (logAddition != null)
    {
      logString.append(logAddition).append("\n");
    }
    
    logString.append("cached token keys:");
    
    for (String targetStoreToken: targetStore.keySet()) 
    {
      logString.append("\n    ");
      logString.append(getTokenToViewIdString(tokenToViewId, targetStoreToken));
    }  
    
    if (pinned != null)
    {
      logString.append("\n_pinned token keys:");
      
      for (String pinnedKeyToken: pinned.keySet()) 
      {
        logString.append("\n    ");
        logString.append(getTokenToViewIdString(tokenToViewId, pinnedKeyToken));
        
        logString.append("   pinned to     ");
        String pinnedValueToken = pinned.get(pinnedKeyToken);
        logString.append(getTokenToViewIdString(tokenToViewId, pinnedValueToken));
      }
    }
    
    addToRequestStringBuffer("\n" + logString.toString());
  }  

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static Map<String,String> getTokenToViewIdMap()
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    FacesContext context = FacesContext.getCurrentInstance();
    Map<String,String> tokenToViewId = (Map<String, String>)context.getExternalContext().getSessionMap().get("org.apache.myfaces.trinidadinternal.util.TOKEN_FOR_VIEW_ID");
    
    if (tokenToViewId == null) 
    {
      tokenToViewId = new ConcurrentHashMap<String, String>();
      context.getExternalContext().getSessionMap().put("org.apache.myfaces.trinidadinternal.util.TOKEN_FOR_VIEW_ID", tokenToViewId);
      
    }
    return tokenToViewId;
  }

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static void logIdString()
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    
    ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();    
    String sessionId = "";
    Object session = externalContext.getSession(false);
    
    if (session instanceof HttpSession)
    {
      sessionId = ((HttpSession)session).getId();
    }      

    addToRequestStringBuffer("Session Id = " + sessionId);
    
    WindowManager wm = RequestContext.getCurrentInstance().getWindowManager();
    String windowId = wm.getCurrentWindow(externalContext).getId();
    addToRequestStringBuffer("\nWindow Id = " + windowId );    
  }  

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static void addToRequestStringBuffer(String addString)
  {     
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    _getRequestStringBuffer().append(addString);
  }
  

  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static void clearRequestStringBuffer()
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap(); 
    requestMap.put("org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE.StringBuffer", null);
  }
  
  /**
   * Method to help with debugging, should only be called when 
   * debugTokenCache() is true
   */
  public static String getRequestString()
  {
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    return _getRequestStringBuffer().toString();
  }  

  private static StringBuffer _getRequestStringBuffer()
  {    
    if (!_DEBUG_TOKEN_CACHE)
      throw new UnsupportedOperationException(_UNSUPPORTED_OPERATION_MESSAGE);
    
    Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();   
    StringBuffer buff = (StringBuffer)requestMap.get("org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE.StringBuffer");
    
    if ( buff == null)
    {
      buff = new StringBuffer();
      requestMap.put("org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE.StringBuffer", buff);
    }
    
    return buff;
  }  
  
  private static final String _UNSUPPORTED_OPERATION_MESSAGE =  
           "Methods in TokenCacheDebugUtils can only be called when " +
           "TokenCacheDebugUtils.debugTokenCache() returns true. " + 
           "TokenCacheDebugUtils.debugTokenCache() returns true when the system property " +
           "'org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE' is true";
    
  // ViewExpiredExceptions are fairly common, and the token cache is used for 
  // page state tokens, but the tokens aren't really human readable. 
  // In order to make it easier to understand what is in the cache
  // we've added a system property for debugging purposes. When enabled
  // we store a map of token -> viewId on the session which we use
  // to log something more human readable.
  // 
  // in order to use this the tester would set the system property to:
  // -Dorg.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE=true

  static private final Boolean _DEBUG_TOKEN_CACHE;
  static
  {
    String dtcProp = System.getProperty("org.apache.myfaces.trinidadinternal.DEBUG_TOKEN_CACHE");
    _DEBUG_TOKEN_CACHE = Boolean.valueOf(dtcProp);  
  }
}
