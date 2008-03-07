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

import java.io.UnsupportedEncodingException;

import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.ActionRequest;
import javax.portlet.ActionResponse;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;
import org.apache.myfaces.trinidadinternal.webapp.wrappers.ActionRequestWrapper;

public class ActionUploadRequestWrapper
  extends ActionRequestWrapper
{
  public ActionUploadRequestWrapper(
      ExternalContext ec,
      Map<String, String[]> params)
  {
    super((ActionRequest)ec.getRequest());
    _response = (ActionResponse)ec.getResponse();

    @SuppressWarnings("unchecked")
    Map<String, String[]> origionalMap = super.getParameterMap();

    _extractedParams = new HashMap<String, String[]>(origionalMap);
    _extractedParams.putAll(params);

    _encoding = super.getCharacterEncoding();
  }

  /**
   * Hide the content type so that no one tries to re-download the
   * uploaded files.
   */
  @Override
  public String getContentType()
  {
    return _WWW_FORM_URLENCODED_TYPE;
  }

  @Override
  public String getCharacterEncoding()
  {
    return _encoding;
  }


  /**
   * Trap calls to setCharacterEncoding() to decode parameters correctly
   */
  @Override
  public void setCharacterEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    // If the encoding is already right, we can bail
    if (encoding.equals(_encoding))
      return;
    
    // Don't call super.setCharacterEncoding() - it's too late
    // and we'll get a warning
    _encoding = encoding;
    if (_LOG.isFine())
      _LOG.fine("Switching encoding of wrapper to " + encoding);

    _extractedAndDecodedParams =
      new HashMap<String, String[]>(_extractedParams.size());

    byte[] buffer = new byte[256];

    for(Map.Entry<String, String[]> entry : _extractedParams.entrySet())
    {
      String key = entry.getKey();
      key = CaboHttpUtils.decodeRequestParameter(key, encoding, buffer);

      String[] oldValue = entry.getValue();
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
      _response.setRenderParameters(_extractedAndDecodedParams);
    }

    // Let the UploadedFiles know, so it can fix up filenames
    UploadedFiles.setCharacterEncoding(this, encoding);
  }

  @Override
  public String getParameter(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value[0];
  }

  @Override
  public Map<String, String[]> getParameterMap()
  {
    Map<String, String[]> map = _getMap();
    return Collections.unmodifiableMap(map);
  }

  @Override
  public Enumeration<String> getParameterNames()
  {
    return Collections.enumeration(_getMap().keySet());
  }

  @Override
  public String[] getParameterValues(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return (String[]) value.clone();
  }

  private String[] _getParameterValues(String param)
  {
    return _getMap().get(param);
  }

  /**
   * Get the correct map of parameters whether or not setCharacterEncoding()
   * was called.
   */
  private Map<String, String[]> _getMap()
  {
    if (_extractedAndDecodedParams != null)
      return _extractedAndDecodedParams;

    return _extractedParams;
  }

  private Map<String, String[]> _extractedAndDecodedParams;
  private Map<String, String[]> _extractedParams;
  private ActionResponse _response;
  private String         _encoding;
  private static final String _WWW_FORM_URLENCODED_TYPE =
    "application/x-www-form-urlencoded";
  private static final TrinidadLogger _LOG =
     TrinidadLogger.createTrinidadLogger(ActionUploadRequestWrapper.class);
}
