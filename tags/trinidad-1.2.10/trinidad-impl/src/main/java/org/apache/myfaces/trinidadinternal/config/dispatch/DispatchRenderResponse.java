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
package org.apache.myfaces.trinidadinternal.config.dispatch;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.faces.context.ExternalContext;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;

import org.apache.myfaces.trinidadinternal.webapp.wrappers.RenderResponseWrapper;

@SuppressWarnings("deprecation")
class DispatchRenderResponse extends RenderResponseWrapper
{
  public DispatchRenderResponse(ExternalContext ec)
  {
    super((RenderResponse)ec.getResponse());
     _request = (RenderRequest)ec.getRequest();
  }

  @Override
  public void setContentType(
    String contentTypeAndCharset)
  {
    if(contentTypeAndCharset != null)
    {
      Matcher matcher = _CONTENT_TYPE_PATTERN.matcher(contentTypeAndCharset);
      if (matcher.matches())
      {
        String contentType = matcher.group(1);
        String charset = (matcher.groupCount() > 1) ? matcher.group(2) : null;

        // capture the content type on the request
        _request.setAttribute(DispatchResponseConfiguratorImpl.__CONTENT_TYPE_KEY, contentType);

        // TODO: use Agent APIs when available
        if ("application/xhtml+xml".equals(contentType))
        {
          //TODO: Is this still needed in IE7??
          String userAgent = _request.getProperty("User-agent");
          if (userAgent != null && userAgent.indexOf("compatible; MSIE") != -1)
          {
            // IE must serve XHTML as text/html
            contentTypeAndCharset = "text/html";

            if (charset != null)
              contentTypeAndCharset += ";charset=" + charset;
          }
        }
      }
    }
    super.setContentType(contentTypeAndCharset);
  }

  private final RenderRequest _request;

  static private final Pattern _CONTENT_TYPE_PATTERN =
                                  Pattern.compile("([^;]+)(?:;charset=(.*))?");
}
