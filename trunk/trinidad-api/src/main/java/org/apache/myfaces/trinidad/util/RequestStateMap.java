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
package org.apache.myfaces.trinidad.util;

import java.lang.reflect.Method;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.faces.context.ExternalContext;

import javax.portlet.faces.annotation.ExcludeFromManagedRequestScope;

import javax.servlet.ServletRequest;

/**
 * This object provides a map that is primarily used to enforce a consistent contract for "state" information in both
 * a portal environment and a servlet environment.  In a portal environment, request-scope attributes are not necessarily
 * "saved" between processAction and render.  According to the JSF Portlet Bridge Spec, a request-scope attribute may
 * hang around from a single processAction to multiple render requests unless it is excluded.  Of course, excluding the
 * object from the managed request scope will suffer from the problem of the request scope going away BETWEEN action and
 * render.
 * <p/>
 * This class will save an attribute in a processAction portion of the request, will preserve it on the session, and will
 * read it on the first render following the action.  This allows attributes to be reliably exchanged from action to render
 * without having to worry about subsequent renders inheriting the same state.  This is useful for storing things like
 * "ppr" data and such when a subsequent render may be expected to render the entire page.
 * <p/>
 * In a servlet environment, this class is the same as the request scope.
 */
 @ExcludeFromManagedRequestScope
 public class RequestStateMap extends HashMap<String, Object>
 {

   /**
    * Returns an instance of the RequestStateMap.  If one is not already present on the request, it will create a new one.
    * This version of the getInstance DOES NOT attempt to restore the map from the session because such an action is only
    * valid in a PortletEnvironment which should not have a ServletRequest.  This method is provided for the convenience
    * of getting access to the class from the filter.
    * 
    * @param req a servletRequest
    * @return the RequestStateMap for this servletRequest
    */
   static public RequestStateMap getInstance(ServletRequest req)
   {
     RequestStateMap map = (RequestStateMap)req.getAttribute(_STATE_MAP);
     
     if(map == null)
     {
       map = new RequestStateMap();
       req.setAttribute(_STATE_MAP, map);
     }
     
     return map;
   }
   
   /**
    * Returns an instance of the RequestStateMap.  If the RequestStateMap is not present, this class will attempt to find
    * a version that was saved from {@link #saveState(ExternalContext)}.  If no state was saved, then it will create a new
    * map and return it.
    * <p/>
    * Please keep in mind that regardless of whether a map was already present on the request or not, executing this method
    * will ALWAYS clear any state which might have been saved to the session.  This is what enforces the "single restore"
    * type functionality.
    * 
    * @param ec an ExternalContext
    * @return the RequestStateMap for this ExternalContext
    */
   static public RequestStateMap getInstance(ExternalContext ec)
   {
     Map<String, Object> reqMap = ec.getRequestMap();
     RequestStateMap map = (RequestStateMap)reqMap.get(_STATE_MAP);
     
     //For now, always check this on a render so it can be removed from the session.
     //This can be optimized to only save the state when request attributes are NOT preserved
     if(!ExternalContextUtils.isRequestFromClient(ec))
     {
       String uuid = ec.getRequestParameterMap().get(_STATE_MAP);
       if(uuid!= null)
       {
          RequestStateMap myMap= (RequestStateMap)ec.getSessionMap().remove(_STATE_MAP+"."+uuid);
          if(map == null)
          {
            map = myMap;
            reqMap.put(_STATE_MAP, map);
          }
          else
          {
            //TODO: put optimization code here
          }
       }
     }
     
     if(map == null)
     {
       map = new RequestStateMap();
       reqMap.put(_STATE_MAP, map);
     }
     
     return map;
   }
   
   private RequestStateMap(){};
   
   /**
    * Saves the current state to the session.  Running this is only manditory if you need to preserve a request across
    * the natural request boundry in a portlet environment.  Executing this function outside of a portlet environment will
    * not alter operation of his class.
    * 
    * @param ec the ExternalContext to save the stateMap to.
    */
   public void saveState(ExternalContext ec)
   {
     RequestType type = ExternalContextUtils.getRequestType(ec);
     if(type.isPortlet() && !type.isResponseWritable())
     {
       try
       {
         //TODO: use reflection here but it can be replaced..
         Object actionResp = ec.getResponse();
         Method m = actionResp.getClass().getMethod("setRenderParameter", String.class, String.class);
         String uuid = UUID.randomUUID().toString();

         ec.getSessionMap().put(_STATE_MAP+"."+uuid, this);
         m.invoke(actionResp, _STATE_MAP, uuid);
       }
       catch(Throwable t)
       {
         //TODO: Log exception
         t.printStackTrace();
       }
     }
   }
   
   private static final String _STATE_MAP = RequestStateMap.class.getName();
  @SuppressWarnings("compatibility:-6292931291923989771")
  private static final long serialVersionUID = 1L;
 }
