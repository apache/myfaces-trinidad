/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

import java.io.UnsupportedEncodingException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.share.util.CaboHttpUtils;

/**
 * Request wrapper class that hooks in parameters identified in
 * the servlet request.
 *
 * @todo Stop going String -> bytes -> String;  change MultipartFormHandler
 *  to simply extract byte arrays, and do all the type conversion here.
 * @author The Oracle ADF Faces Team
 */
class UploadRequestWrapper extends HttpServletRequestWrapper
{
  public UploadRequestWrapper(HttpServletRequest request, Map extractedParams)
  {
    super(request);
    _extractedParams = extractedParams;
  }

  /**
   * Hide the content type so that no one tries to re-download the
   * uploaded files.
   */
  public String getContentType()
  {
    return _WWW_FORM_URLENCODED_TYPE;
  }

  /**
   * Trap calls to setCharacterEncoding() to decode parameters correctly
   */
  public void setCharacterEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    super.setCharacterEncoding(encoding);
    if (_LOG.isFine())
      _LOG.fine("Switching encoding of wrapper to " + encoding);

    _extractedAndDecodedParams = new HashMap(_extractedParams.size());
    Iterator entries = _extractedParams.entrySet().iterator();

    byte[] buffer = new byte[256];

    while (entries.hasNext())
    {
      Map.Entry entry = (Map.Entry) entries.next();
      String key = (String) entry.getKey();
      key = CaboHttpUtils.decodeRequestParameter(key, encoding, buffer);

      String[] oldValue = (String[]) entry.getValue();
      int length = oldValue.length;
      String[] newValue = new String[length];
      for (int i = 0; i < length; i++)
      {
        newValue[i] = CaboHttpUtils.decodeRequestParameter(oldValue[i],
                                                           encoding,
                                                           buffer);
        if (_LOG.isFinest())
          _LOG.finest("Parameter " + key + ":" + newValue[i]);
      }

      _extractedAndDecodedParams.put(key, newValue);
    }

    // Let the UploadedFiles know, so it can fix up filenames
    UploadedFiles.setCharacterEncoding(this, encoding);
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
    Map map = _getMap();
    return Collections.unmodifiableMap(map);
  }

  public Enumeration getParameterNames()
  {
    return Collections.enumeration(_getMap().keySet());
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
    return (String[]) _getMap().get(param);
  }

  /**
   * Get the correct map of parameters whether or not setCharacterEncoding()
   * was called.
   */
  private Map _getMap()
  {
    if (_extractedAndDecodedParams != null)
      return _extractedAndDecodedParams;

    return _extractedParams;
  }

  private Map _extractedAndDecodedParams;
  private Map _extractedParams;

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(UploadRequestWrapper.class);
  private static final String _WWW_FORM_URLENCODED_TYPE =
    "application/x-www-form-urlencoded";
}
