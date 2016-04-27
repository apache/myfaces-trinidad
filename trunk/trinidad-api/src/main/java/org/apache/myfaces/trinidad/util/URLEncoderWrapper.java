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

import java.util.List;
import java.util.Map;

/**
 * Wrapper class for the URLEncoder that can be used to wrap and existing encoder and
 * set it on the factory.
 */
public abstract class URLEncoderWrapper
  extends URLEncoder
{
  public abstract URLEncoder getWrapped();
  
  /**
   * Encodes an action url.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeActionURL(String)} method.
   * 
   * @param url a string containing the url to encode
   * @return the encoded url
   */
  @Override
  public String encodeActionURL(String url)
  {
    return getWrapped().encodeActionURL(url);
  }

  /**
   * Encodes a resource url.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeResourceURL(String)} method.
   * 
   * @param url a string containing the url to encode
   * @return the encoded url
   */
  @Override
  public String encodeResourceURL(String url)
  {
    return getWrapped().encodeResourceURL(url);
  }

  /**
   * Encodes a resource url that is meant to be in-protocol.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeInProtocolResourceURL(String)} method.
   * 
   * @param url a string containing the url to encode
   * @return the encoded url
   */
  @Override
  public String encodeInProtocolResourceURL(String url)
  {
    return getWrapped().encodeInProtocolResourceURL(url);
  }

  /**
   * Encodes an action URL that is intended to represent a partial page submit.  By default, this method calls into the wrapped
   * encoder and executes the {@link URLEncoder#encodePartialActionURL(String)} method.
   * 
   * @param url a string containing the url to encode
   * @return the encoded url
   */
  @Override
  public String encodePartialActionURL(String url)
  {
    return getWrapped().encodePartialActionURL(url);
  }

  /**
   * Encodes a redirect url.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeRedirectURL(String)} method.
   * 
   * @param url a string containing the url to encode
   * @return the encoded url
   */
  @Override
  public String encodeRedirectURL(String url)
  {
    return getWrapped().encodeRedirectURL(url);
  }
  
  /**
   * Encodes a redirect url.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeRedirectURL(String, Map<String, List<String>>)} method.
   * 
   * @param url a string containing the url to encode
   * @param params a map containing additional params to add to the querystring
   * @return the encoded url
   * @throws UnsupportedOperationException if this method is not implemented or if this type of
   *         url cannot be encoded.
   * 
   * @since 2.1
   */  
  @Override
  public String encodeRedirectURL(String url, Map<String, List<String>> params)
  {
    return getWrapped().encodeRedirectURL(url, params);
  }

  /**
   * Encodes the URL of a Skin Resource.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeSkinResourceURL(String)} method.
   * 
   * @param url a string containing additional params to add to the querystring.
   * @return the encoded url
   */
  @Override
  public String encodeSkinResourceURL(String url)
  {
    return getWrapped().encodeSkinResourceURL(url);
  }

  /**
   * Encodes a bookmarkable url.  By default this method calls into the wrapped encoder and
   * executes the {@link URLEncoder#encodeBookmarkableURL(String,Map<String,List<String>>)} method.
   * 
   * @param url a string containing additional params to add to the querystring.
   * @return the encoded url
   * @throws UnsupportedOperationException if this method is not implemented or if this type of
   *         url cannot be encoded.
   *         
   * @since 2.1
   */
  @Override
  public String encodeBookmarkableURL(String url, Map<String, List<String>> params)
  {
    return getWrapped().encodeBookmarkableURL(url, params);
  }
}
