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
package org.apache.myfaces.trinidadinternal.config;

import java.io.UnsupportedEncodingException;

import java.net.URLDecoder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.util.URLEncoder;
import org.apache.myfaces.trinidad.util.URLEncoderFactory;
import org.apache.myfaces.trinidad.util.URLUtils;

public class URLEncoderExternalContext
  extends ExternalContextDecorator
{
  public URLEncoderExternalContext(ExternalContext ec)
  {
    _ec = ec;
  }

  protected ExternalContext getExternalContext()
  {
    return _ec;
  }

  @Override
  public String encodeBookmarkableURL(String string, Map<String, List<String>> map)
  {
    URLEncoder encoder = getURLEncoder();
    
    try
    {
      return encoder.encodeBookmarkableURL(string, map);
    }
    catch(UnsupportedOperationException e)
    {
      //This is a valid response if we have an old handler.  In this case, we need to do our own encoding
      Object response = _ec.getResponse();
      if (!(response instanceof HttpServletResponse))
      {
        throw new UnsupportedOperationException("Only valid for HttpServlet requests");
      }
      
      String url = URLUtils.encodeURL(string, map, _ec.getResponseCharacterEncoding());
      
      //For this guy, we simply return the URL I believe.  Other encoding should be done before or after this.
      return url;
    }
  }

  @Override
  public String encodeNamespace(String string)
  {
    // TODO Implement this method
    return super.encodeNamespace(string);
  }

  @Override
  public String encodePartialActionURL(String string)
  {
    // TODO Implement this method
    return super.encodePartialActionURL(string);
  }

  @Override
  public String encodeRedirectURL(String string, Map<String, List<String>> map)
  {
    URLEncoder encoder = getURLEncoder();
    
    try
    {
      return encoder.encodeRedirectURL(string, map);
    }
    catch(UnsupportedOperationException e)
    {
      //This is a valid response if we have an old handler.  In this case, we need to do our own encoding
      Object response = _ec.getResponse();
      if (!(response instanceof HttpServletResponse))
      {
        throw new UnsupportedOperationException("Only valid for HttpServlet requests");
      }
      
      String url = URLUtils.encodeURL(string, map, _ec.getResponseCharacterEncoding());
      
      //this version of the encodeRedirect method MUST be supported.
      return encoder.encodeRedirectURL(url);
    }
  }

  @Override
  public String encodeResourceURL(String url)
  {
    return getURLEncoder().encodeResourceURL(url);
  }

  @Override
  public String encodeActionURL(String url)
  {
    return getURLEncoder().encodeActionURL(url);
  }
  
  protected URLEncoder getURLEncoder()
  {
    return URLEncoderFactory.getFactory().getURLEncoder(_ec);
  }
  
  private ExternalContext _ec;
}
