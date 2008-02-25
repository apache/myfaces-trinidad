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
package org.apache.myfaces.trinidadinternal.config.upload;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestParameterMap;
import org.apache.myfaces.trinidadinternal.context.external.ServletRequestParameterValuesMap;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

class ServletUploadedExternalContext extends ExternalContextDecorator
{
  ServletUploadedExternalContext(ExternalContext externalContext, Map<String, String[]> extractedParams)
  {
    _externalContext = externalContext;
    FileUploadConfiguratorImpl.apply(externalContext);

    @SuppressWarnings("unchecked")
    Map<String, String[]> params = new HashMap<String, String[]>(externalContext.getRequestParameterValuesMap());
    params.putAll(extractedParams);

    _wrappedRequest  = new UploadRequestWrapper((HttpServletRequest)_externalContext.getRequest(), params);
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.context.ExternalContextDecorator#getRequest()
   */
  @Override
  public Object getRequest()
  {
    return _wrappedRequest;
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.context.ExternalContextDecorator#getRequestParameterMap()
   */
  @Override
  public Map<String, String> getRequestParameterMap()
  {
    if(_requestParameterMap == null)
    {
      _requestParameterMap = new ServletRequestParameterMap(_wrappedRequest);
    }
    return _requestParameterMap;
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.context.ExternalContextDecorator#getRequestParameterNames()
   */
  @Override
  public Iterator<String> getRequestParameterNames()
  {
    if(_requestParameterNames == null)
    {
      if(_requestParameterMap != null)
      {
        _requestParameterNames = _requestParameterMap.keySet().iterator();
      }
      else
      {
        _requestParameterNames = getRequestParameterValuesMap().keySet().iterator();
      }
    }
    return _requestParameterNames;
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.context.ExternalContextDecorator#getRequestParameterValuesMap()
   */
  @Override
  public Map <String, String[]>getRequestParameterValuesMap()
  {
    if(_requestParameterValuesMap == null)
    {
      _requestParameterValuesMap = new ServletRequestParameterValuesMap(_wrappedRequest);
    }
    return _requestParameterValuesMap;
  }

  private ExternalContext _externalContext;
  private HttpServletRequest _wrappedRequest;
  private Map<String, String>   _requestParameterMap;
  private Map<String, String[]> _requestParameterValuesMap;
  private Iterator<String>      _requestParameterNames;

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.context.ExternalContextDecorator#getExternalContext()
   */
  @Override
  protected ExternalContext getExternalContext()
  {
    return _externalContext;
  }
}
