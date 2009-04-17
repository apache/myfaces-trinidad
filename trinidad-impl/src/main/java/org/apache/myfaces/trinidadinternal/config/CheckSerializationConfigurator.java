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
package org.apache.myfaces.trinidadinternal.config;

import java.io.Serializable;

import java.util.Collections;
import java.util.Map;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.util.CollectionUtils;

/**
 * Configurator that uses a wrapped ExternalContext to return a Session Map that validates that
 * only Serializable Objects are placed in the Map.
 * @version $Revision$ $Date$
 */
public final class CheckSerializationConfigurator extends Configurator
{

  /**
   * Override to return our ExternalContext wrapped if session serialization checking is enabled
   * @param externalContext
   * @return
   */
  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    boolean checkSession = StateUtils.checkSessionSerialization(externalContext);
    boolean checkApplication = StateUtils.checkApplicationSerialization(externalContext);
      
    if (checkSession || checkApplication)
    {
      return new SessionSerializationChecker(externalContext, checkSession, checkApplication);
    }
    else
    {
      return externalContext;
    }
  }
  
  /**
   * ExternalContextDecorator returning our wrapped SessionMap
   */
  private static class SessionSerializationChecker extends ExternalContextDecorator
  {
    public SessionSerializationChecker(
      ExternalContext extContext,
      boolean checkSession,
      boolean checkApplication)
    {
      _extContext = extContext;
      
      Map<String, Object> sessionMap = extContext.getSessionMap();
      
      if (checkSession)
      {
        // skank using type erasure to finess the fact that Collections.checkedMap() expects and
        // will return a Map<String, Serializable> when in fact, we should be returning a
        // Map<String, Onject>.  Using checkedMap also has the disadvantage that a ClassCastException
        // is thrown when we would really prefer to throw a more-explanatory message
        //Map erasedMap = sessionMap;
        //_sessionMap = (Map)Collections.checkedMap(erasedMap, String.class, Serializable.class);
        _sessionMap =  CollectionUtils.getCheckedSerializationMap(sessionMap);
      }
      else
      {
        _sessionMap = sessionMap;
      }
 
      Map<String, Object> applicationMap = extContext.getApplicationMap();
      
      if (checkApplication)
      {
        // skank using type erasure to finess the fact that Collections.checkedMap() expects and
        // will return a Map<String, Serializable> when in fact, we should be returning a
        // Map<String, Onject>.  Using checkedMap also has the disadvantage that a ClassCastException
        // is thrown when we would really prefer to throw a more-explanatory message
        //Map erasedMap = applicationMap;
        //_applicationMap = (Map)Collections.checkedMap(erasedMap, String.class, Serializable.class);
        _applicationMap =  CollectionUtils.getCheckedSerializationMap(applicationMap);
      }
      else
      {
        _applicationMap = applicationMap;
      }
   }

    protected ExternalContext getExternalContext()
    {
      return _extContext;
    }

    public Map<String, Object> getSessionMap()
    {
      return _sessionMap;
    }

    public Map<String, Object> getApplicationMap()
    {
      return _applicationMap;
    }
   
    private final ExternalContext _extContext;
    private final Map _sessionMap;
    private final Map _applicationMap;
  }
}
