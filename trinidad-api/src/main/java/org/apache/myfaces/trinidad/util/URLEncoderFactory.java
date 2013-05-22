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

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
 * This is a factory for the URLEncoder.  The URL encoder is a convenience class that
 * handles URL encoding in a container independant fashion.  The Portlet bridge spec
 * and later JSF specifications have tried to make URL encoding more specific, but
 * these technologies do not always address the needs of Trinidad URL encoding in
 * all circumstances.  This method aims to do just that.
 * <p>
 * This factory can both create URLEncoders as well as keep track of an encoder on
 * a per-request basis.  Additionally, custom URLEncoders can be added manually via
 * the {@link #setURLEncoder} method.
 */
public class URLEncoderFactory
{
  /**
   * Returns the current URLEncoderFactory.
   * 
   * @return a URLEncoder factory
   */
  public static URLEncoderFactory getFactory()
  {
    return _ENCODER;
  }
  
  /**
   * Returns the current URLEncoder if it has been set.  If it has not been set it
   * creates a new URLEncoder, sets it to the current thread, and returns its value.
   * 
   * @return the currently set URL encoder.
   * @throws IllegalStateException if no controller has been set and FacesContext is
   *         currently unavailble.
   */
  public URLEncoder getURLEncoder()
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if(null == fc)
    {
      return getURLEncoder(null);
    }

    return getURLEncoder(fc.getExternalContext());
  }
  
  /**
   * Returns a URLEncoder if one has been set.  If it has not been set, it
   * creates a new URLEncoder using the provided ExternalContext.
   * 
   * @return a URLEncoder for the given request
   * @throws IllegalStateException if no controller has been set and the ExternalContext object
   *         is null
   */
  public URLEncoder getURLEncoder(ExternalContext ec)
  {
    //even though we should wait until we have a faces context and throw an error
    //if we don't, go ahead and fudge it if the threadlocal is not null.  This just
    //means that a URLEncoder has already been set for this thread.
    URLEncoder enc = _local.get();
    
    if(null != enc)
    {
      return enc;
    }
    
    if(null == ec)
    {
      throw new IllegalStateException("An ExternalContext must be a available");
    }
    
    if(ExternalContextUtils.isPortlet(ec))
    {
      setURLEncoder(new PortletURLEncoder(ec));
    }
    else
    {
      setURLEncoder(new ExternalContextURLEncoder(ec));
    }
    
    return _local.get();
  }
  
  public void setURLEncoder(URLEncoder encoder)
  {
    _local.set(encoder);
  }
  
  private static final URLEncoderFactory _ENCODER = new URLEncoderFactory();
  
  //This threadlocal should get cleaned up when the request is done.  It's handled
  //by the configurators.
  private static final ThreadLocal<URLEncoder> _local = ThreadLocalUtils.newRequestThreadLocal();
  
}
