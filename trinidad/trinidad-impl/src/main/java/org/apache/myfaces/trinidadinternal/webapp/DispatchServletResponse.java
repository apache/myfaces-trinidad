/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.webapp;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

public class DispatchServletResponse extends HttpServletResponseWrapper
{
  public DispatchServletResponse(
    HttpServletResponse delegate,
    HttpServletRequest  request)
  {
    super(delegate);
    _request = request;
  }

  public void setContentType(
    String contentTypeAndCharset)
  {
    Matcher matcher = _CONTENT_TYPE_PATTERN.matcher(contentTypeAndCharset);
    if (matcher.matches())
    {
      String contentType = matcher.group(1);
      String charset = (matcher.groupCount() > 1) ? matcher.group(2) : null; 
      
      // capture the content type on the request
      _request.setAttribute(_CONTENT_TYPE_KEY, contentType);

      // TODO: use Agent APIs when available
      if ("application/xhtml+xml".equals(contentType))
      {
        String userAgent = _request.getHeader("User-agent");
        if (userAgent.indexOf("compatible; MSIE") != -1)
        {
          // IE must serve XHTML as text/html
          contentTypeAndCharset = "text/html";

          if (charset != null)
            contentTypeAndCharset += ";charset=" + charset;
        }
      }
    }

    super.setContentType(contentTypeAndCharset);
  }
  
  static public String getContentType(
    FacesContext context)
  {
    Map requestMap = context.getExternalContext().getRequestMap();
    return (String) requestMap.get(_CONTENT_TYPE_KEY);
  }
  
  private final HttpServletRequest _request;
 
  static private final String _CONTENT_TYPE_KEY = 
                                  "org.apache.myfaces.trinidadinternal.CONTENT_TYPE";
                                  
  static private final Pattern _CONTENT_TYPE_PATTERN = 
                                  Pattern.compile("([^;]+)(?:;charset=(.*))?");
}
