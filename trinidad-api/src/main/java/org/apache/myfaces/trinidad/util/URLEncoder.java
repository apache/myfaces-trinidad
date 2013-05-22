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

import java.io.Serializable;

import java.io.UnsupportedEncodingException;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
 * Encodes URL's based on thier type.  While the ExternalContext does this to
 * some extent, the types of URL's it encodes are often ill-defined.  This utility
 * method allows the caller to ensure that URL's are encoded in the proper fashion
 * depending on which container is active at the time.
 * <p/>
 * Out of the box, this class supports Servlet and Portlet encoding, but it may
 * be extended on a per-request basis to support other types of URL encoding with
 * the use of the "registerURLEncoder" method.
 * <p/>
 * It is also important to note that this does not impact the encoding done by the
 * ExternalContext.  As such, all current applications should work without
 * modification if they do not choose to use this API for encoding.
 */
public abstract class URLEncoder
{
  /**
   * This function should be the same as the {@link ExternalContext#encodeActionURL(String)}
   * method.  By default it call the code in the ExternalContext.  The reason its
   * provided here is that certain URLEncoderUtility instances may wish to override
   * the default functionality and have the ExternalContext pull its default encoding
   * from here.
   */
  public abstract String encodeActionURL(String url);
  
  /**
   * This encodes a URL so that it can be used to make a PPR request in all containers.
   * JSF 2.0 has the ability to encode URL's in such a fashion, but this is missing
   * from JSF 1.2 containers.  This method provides the same functionality for JSF 1.2.
   * 
   * @param url the unencoded url
   * @return the encoded url
   * 
   * @throws IllegalArgumentException if the URL cannot be encoded
   */
  public abstract String encodePartialActionURL(String url);
  
  /**
   * Encodes a url to be explicitly used for a redirect.  In some containers, this is
   * encoded as-is.  In other containers this may not be encoded or must contain a
   * fully qualified url.  By default this method calls {@link #encodeRedirectURL(String,Map<String,List<String>>)}
   * with an empty parameter map.
   * 
   * @param url the unencoded url
   * @return the encoded url
   * 
   * @throws IllegalArgumentException if the URL cannot be encoded
   */
  public abstract String encodeRedirectURL(String url);

  /**
   * Encodes a url to be explicitly used for a redirect.  In some containers, this is
   * encoded as-is.  In other containers this may not be encoded or must contain a
   * fully qualified url.  This version of the method allows for a map of parameters
   * to be included in the URL.  These parameters will generate a query string and add
   * it to a URL that may or may-not already have its own existing query parameters.
   * The parameter values should be encoded appropriately for the environment so that
   * the resulting URL can be used at the target of the redirect.
   * 
   * The default implementation throws and UnsupportedOperationException and is procided
   * for the sole purpose of maintaining binary compatibility. 
   * 
   * @param url the unencoded url
   * @return the encoded url
   * @throws IllegalArgumentException if the URL cannot be encoded
   * @throws UnsupportedOperationException if the implementation of the URLEncoder is not implemented.
   * 
   * @since 2.1
   * @see ExternalContext#encodeRedirectURL
   */
  public String encodeRedirectURL(String url, Map<String, List<String>> parameters)
  {
    throw new UnsupportedOperationException();
  }
  
  /**
   * Encodes a url as a resource.  Generally speaking this will be equivalent to
   * {@link ExternalContext#encodeResourceURL(String)}.  The url returned from this
   * method is NOT guarenteed to be in-protocol (meaning that it MAY not have access
   * to session information).  The advantage of encoding something in this fashion
   * is that in certain types of containers, like portals, the URL generated may 
   * have faster access and will generally work better for the purposes of caching
   * do to its RESTful state.
   * 
   * @param url the unencoded url
   * @return the encoded url
   * 
   * @throws IllegalArgumentException if the URL cannot be encoded
   */
  public abstract String encodeResourceURL(String url);
  
  /**
   * Encodes a url to a resource such that it is inProtocol.  This means that the
   * URL returned will be encoded in such a way that the resource will have access
   * to the parent application's session.  While these URL's do have access to the
   * session information, they may not be written in a format that is easily cachable.
   * 
   * @param url the unencoded url
   * @return the encoded url
   * 
   * @throws IllegalArgumentException if the URL cannot be encoded
   */
  public abstract String encodeInProtocolResourceURL(String url);
  
  /**
   * Encodes a resource URL that is mapped to a skinning resources.  Trinidad has
   * an extensive skinning system that allows for extraction of certain properties
   * like images so that they can be used with the componentry.  Generally these
   * image resources are on the same server and whatnot as the actual skin.  In
   * a servlet environment, this is always the same server, but in portal-like
   * environments these resources may be on different servers.  This encodes a
   * URL that comes from a skin and sends it to the right location.
   * 
   * @param url
   * @return
   */
  public abstract String encodeSkinResourceURL(String url);
  
  /**
   * The purpose of this method is to generate a query string from the collection of Parameter 
   * objects provided by the parameters argument and append that query string to the baseUrl. This 
   * method must be able to encode the parameters to a baseUrl that may or may not have existing 
   * query parameters. The parameter values should be encoded appropriately for the environment 
   * so that the resulting URL can be used as the target of a link (e.g., in an href attribute) in 
   * a JSF response. It's possible for an ExternalContext implementation to override this method in 
   * any way that would make the URL bookmarkable in that environment.
   * <p/>
   * The default implementation simply throws an UnsupportedOperationException and is provided only
   * for the purposes of ensuring binary compatibility.
   * 
   * @param url the base url
   * @param params a map of parameters
   * @return
   * 
   * @throws UnsupportedOperationException if the default implementation does not support encoding
   *         of bookmarkable urls.
   */
  public String encodeBookmarkableURL(String url, Map<String, List<String>> params)
  {
    throw new UnsupportedOperationException();
  }
}
