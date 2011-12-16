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


import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.util.ExternalContextUtils;

import org.apache.myfaces.trinidad.context.RequestContext;

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
  static private final String _FRAME_BUSTING_PARAM = "org.apache.myfaces.trinidad.security.FRAME_BUSTING";
  static public final String FRAME_BUSTING_NEVER = "never";
  static public final String FRAME_BUSTING_ALWAYS = "always";
  static public final String FRAME_BUSTING_DIFFERENT_ORIGIN = "differentOrigin";
  
  public static String getFrameBustingValue(FacesContext context, RequestContext reqContext)
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
      return FRAME_BUSTING_NEVER;    
    }    
    
    ConcurrentMap<String, Object> appMap = reqContext.getApplicationScopedConcurrentMap();
    String frameBusting = (String)appMap.get(_FRAME_BUSTING_PARAM);
    
    if (frameBusting == null)
    {
      frameBusting = context.getExternalContext().getInitParameter(_FRAME_BUSTING_PARAM);
      
      if (FRAME_BUSTING_NEVER.equalsIgnoreCase(frameBusting))
        frameBusting = FRAME_BUSTING_NEVER;
      else if (FRAME_BUSTING_ALWAYS.equalsIgnoreCase(frameBusting))
        frameBusting = FRAME_BUSTING_ALWAYS;
      else 
        frameBusting = FRAME_BUSTING_DIFFERENT_ORIGIN;
      
      appMap.put(_FRAME_BUSTING_PARAM, frameBusting);
    }

    return frameBusting;
  }  
}
