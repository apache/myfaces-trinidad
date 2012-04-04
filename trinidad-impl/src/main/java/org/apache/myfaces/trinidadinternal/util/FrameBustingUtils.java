/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.util;


import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.util.ExternalContextUtils;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * @author Gabrielle Crawford
 */
public class FrameBustingUtils
{

  // This parameter controls the framebusting feature. framebusting stops content from
  //       running inside frames (meaning a frame or iframe tag).
  //
  //       This context parameter is ignored when
  //       org.apache.myfaces.trinidad.util.ExternalContextUtils.isPortlet is true,
  //       and will behave as if the context parameter is set to 'never'.
  //
  //       Possible values are:
  //         differentOrigin - only bust frames if the an ancestor window origin and the
  //                     frame origin are different. If the ancestor windows and frame have
  //                     the same origin then allow the content to run in a frame.
  //                     This is the default.
  //         always - always bust frames, meaning don't allow a page to be embedded in frames
  //         never - never bust frames, meaning always allow a page to be embedded in frames
  static public final String FRAME_BUSTING_PARAM = "org.apache.myfaces.trinidad.security.FRAME_BUSTING";
  
  public static enum FrameBustingParamValue
  {

    FRAME_BUSTING_NEVER("never"),
    FRAME_BUSTING_ALWAYS("always"),
    FRAME_BUSTING_DIFFERENT_ORIGIN("differentOrigin");

    FrameBustingParamValue(String value)
    {
      _value = value;
    }
    
    /**
     * toString returns the vary-type value.
     * @return the vary-type value.
     */
    public String toString()
    {
      return _value;
    }

    public String displayName()
    {
      return _value;
    }
    
    private String _value;
    
    public static FrameBustingParamValue valueOfDisplayName(String displayName) 
    {
      if (FRAME_BUSTING_NEVER.displayName().equalsIgnoreCase(displayName))
        return FRAME_BUSTING_NEVER;
      else if (FRAME_BUSTING_ALWAYS.displayName().equalsIgnoreCase(displayName))
        return FRAME_BUSTING_ALWAYS;
      else  if (FRAME_BUSTING_DIFFERENT_ORIGIN.displayName().equalsIgnoreCase(displayName))        
        return FRAME_BUSTING_DIFFERENT_ORIGIN;
      
      return null;
    }
    
  }
  
  public static FrameBustingParamValue getFrameBustingValue(FacesContext context, RequestContext reqContext)
  {   
    // Act as if the value is never if
    // 1. we're doing ppr, we should only need to bust frames on a full page render
    // 2. if we're in a portal    
    //     Portals have a concept of producers and consumers.
    //     The main page is the consumer, and the portlets inside that page are the producers.
    //     Producer content can only be accessed by trusted consumers.
    //
    //     The consumer page can set the context param as needed,
    //     but the producers will not do framebusting.
    //     In other words when ExternalContextUtils.isPortlet is true we will behave as if
    //     the context param is set to 'never'.
    if ( reqContext.isPartialRequest(context) ||
         ExternalContextUtils.isPortlet(context.getExternalContext()))
    {
      return FrameBustingParamValue.FRAME_BUSTING_NEVER;    
    }    
    
    ConcurrentMap<String, Object> appMap = reqContext.getApplicationScopedConcurrentMap();
    FrameBustingParamValue frameBusting = (FrameBustingParamValue)appMap.get(FRAME_BUSTING_PARAM);
    
    if (frameBusting == null)
    {
      String frameBustingString = context.getExternalContext().getInitParameter(FRAME_BUSTING_PARAM);   
      
      if  (frameBustingString == null)
      {
        frameBusting = FrameBustingParamValue.FRAME_BUSTING_DIFFERENT_ORIGIN;
      }
      else
      {        
        frameBusting = FrameBustingParamValue.valueOfDisplayName(frameBustingString);   
        
        if (frameBusting == null)
        {
          frameBusting = FrameBustingParamValue.FRAME_BUSTING_DIFFERENT_ORIGIN;
        
          // if the context param was set but does not match a known legal value then log a warning
          _LOG.warning("UNKNOWN_FRAME_BUSTING_VALUE");
        }
      }
      
      appMap.put(FRAME_BUSTING_PARAM, frameBusting);
    }

    return frameBusting;
  }  
  
  public static void overrideFrameBustingValue(RequestContext reqContext, FrameBustingParamValue frameBusting)
  {         
    ConcurrentMap<String, Object> appMap = reqContext.getApplicationScopedConcurrentMap();      
    appMap.put(FRAME_BUSTING_PARAM, frameBusting);
  }    


  static private final TrinidadLogger _LOG = 
                             TrinidadLogger.createTrinidadLogger(FrameBustingUtils.class);  
}
