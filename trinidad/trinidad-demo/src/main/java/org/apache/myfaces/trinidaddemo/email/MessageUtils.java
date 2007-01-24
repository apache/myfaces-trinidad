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
package org.apache.myfaces.trinidaddemo.email;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

public class MessageUtils
{
  static public FacesMessage getErrorMessage(
    FacesContext          context,
    String                key,
    Object[]              params)
  {
    return getMessage(context, key, FacesMessage.SEVERITY_ERROR, params);
  }

  static public FacesMessage getMessage(
    FacesContext          context,
    String                key,
    FacesMessage.Severity severity,
    Object[]              params)
  {
    Locale locale = context.getViewRoot().getLocale();
    ResourceBundle bundle = ResourceBundle.getBundle(
        "org.apache.myfaces.trinidaddemo.email.resource.EmailDemoBundle",
        locale,
        Thread.currentThread().getContextClassLoader());

    String summary;
    String detail;

    try
    {
      summary = bundle.getString(key);
    }
    catch (Exception e)
    {
      _LOG.log(Level.SEVERE, "Can't load key " + key, e);
      summary = "???" + key + "???";
    }

    try
    {
      detail = bundle.getString(key + "_detail");
    }
    catch (Exception e)
    {
      detail = null;
    }

    summary = _format(summary, params);
    detail = _format(detail, params);

    return new FacesMessage(severity, summary, detail);
  }

  static public String getString(FacesContext context, String key)
  {
    try
    {
      Locale locale = context.getViewRoot().getLocale();
      ResourceBundle bundle = ResourceBundle.getBundle(
        "org.apache.myfaces.trinidaddemo.email.resource.EmailDemoBundle",
        locale,
        Thread.currentThread().getContextClassLoader());
      return bundle.getString(key);
    }
    catch (Exception e)
    {
      _LOG.log(Level.SEVERE, "Can't load key " + key, e);
      return "???" + key + "???";
    }
  }


  static private String _format(String mask, Object[] params)
  {
    if ((mask == null) || (params == null))
      return mask;

    return MessageFormat.format(mask, params);
  }

  private MessageUtils()
  {
  }

  static private final Logger _LOG =
    Logger.getLogger(MessageUtils.class.getName());
}
