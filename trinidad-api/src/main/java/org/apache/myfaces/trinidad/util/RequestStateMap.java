package org.apache.myfaces.trinidad.util;

import java.lang.reflect.Method;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.faces.context.ExternalContext;

import javax.portlet.faces.annotation.ExcludeFromManagedRequestScope;

import javax.servlet.ServletRequest;

/**
 * TODO: get rid of this object
 */
 @ExcludeFromManagedRequestScope
 public class RequestStateMap extends HashMap<String, Object>
 {
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
   
   static public RequestStateMap getInstance(ExternalContext ec)
   {
     Map<String, Object> reqMap = ec.getRequestMap();
     RequestStateMap map = (RequestStateMap)reqMap.get(_STATE_MAP);
     
     //For now, always check this on a render so it can be removed from the session.
     //This can be optimized to only save the state when request attributes are NOT preserved
     if(!ExternalContextUtils.isAction(ec))
     {
       String uuid = ec.getRequestParameterMap().get(_STATE_MAP);
       if(uuid!= null)
       {
          RequestStateMap myMap= (RequestStateMap)ec.getSessionMap().remove(_STATE_MAP+"."+uuid);
          if(map == null)
          {
            map = myMap;
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
   
   public void saveState(ExternalContext ec)
   {
     if(ExternalContextUtils.isPortlet(ec) && ExternalContextUtils.isAction(ec))
     {
       try
       {
         //TODO: use reflection here but it can be replaced..
         Object actionResp = ec.getResponse();
         Method m = actionResp.getClass().getMethod("setRenderParameter", String.class, String.class);
         String uuid = UUID.randomUUID().toString();

         ec.getRequestMap().put(_STATE_MAP+"."+uuid, this);
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
 }
