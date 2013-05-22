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

import java.util.Collections;

import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
 * This class contains a URL encoder which delegates to an ExternalContext
 */
class ExternalContextURLEncoder extends URLEncoder
{
  public ExternalContextURLEncoder(ExternalContext ec)
  {
    assert(ec != null);
    _context = ec;
  }
  
  @Override
  public String encodePartialActionURL(String url)
  {
    return getExternalContext().encodePartialActionURL(url);
  }

  @Override
  public String encodeRedirectURL(String url)
  {
    return (encodeRedirectURL(url, null));
  }

  @Override
  public String encodeRedirectURL(String url, Map<String, List<String>> params)
  {
    return getExternalContext().encodeRedirectURL(url, params);
  }

  @Override
  public String encodeInProtocolResourceURL(String url)
  {
    return getExternalContext().encodeResourceURL(url);
  }
  
  @Override
  public String encodeSkinResourceURL(String url)
  {
    return url;
  }

  @Override
  public String encodeActionURL(String url)
  {
    return getExternalContext().encodeActionURL(url);
  }
  
  @Override
  public String encodeResourceURL(String url)
  {
    return getExternalContext().encodeResourceURL(url);
  }
  
  @Override
  public String encodeBookmarkableURL(String url, Map<String, List<String>> params)
  {
    return getExternalContext().encodeBookmarkableURL(url, params);
  }
  
  protected ExternalContext getExternalContext()
  {
    return _context;
  }
  
  private ExternalContext _context;
}
