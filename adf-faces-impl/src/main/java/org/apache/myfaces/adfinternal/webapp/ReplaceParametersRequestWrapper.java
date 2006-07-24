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
package org.apache.myfaces.adfinternal.webapp;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

public class ReplaceParametersRequestWrapper extends HttpServletRequestWrapper
{
  public ReplaceParametersRequestWrapper(
    HttpServletRequest request,
    Map                parameters)
  {
    super(request);
    _parameters = parameters;
  }

  public void setCharacterEncoding(String encoding)
  {
    // Do nothing
  }

  public String getParameter(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value[0];
  }

  public Map getParameterMap()
  {
    return Collections.unmodifiableMap(_parameters);
  }

  public Enumeration getParameterNames()
  {
    return Collections.enumeration(_parameters.keySet());
  }

  public String[] getParameterValues(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return (String[]) value.clone();
  }

  private String[] _getParameterValues(String param)
  {
    return (String[]) _parameters.get(param);
  }

  private Map _parameters;
}
