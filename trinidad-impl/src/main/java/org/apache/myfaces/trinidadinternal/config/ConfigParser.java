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
package org.apache.myfaces.trinidadinternal.config;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;

import javax.faces.context.ExternalContext;
import javax.faces.el.ValueBinding;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.config.upload.UploadedFileProcessorImpl;
import org.apache.myfaces.trinidadinternal.context.RequestContextBean;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.apache.myfaces.trinidadinternal.util.DateUtils;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;



/**
 *
 */
public class ConfigParser
{
  /**
   *
   */
  static public RequestContextBean parseConfigFile(
    ExternalContext externalContext)
  {
    RequestContextBean bean = new RequestContextBean();

    InputStream in = externalContext.getResourceAsStream(_CONFIG_FILE);
    if (in != null)
    {
      try
      {
        InputSource input = new InputSource();
        input.setByteStream(in);
        input.setPublicId(_CONFIG_FILE);

        XMLReader reader = _SAX_PARSER_FACTORY.newSAXParser().getXMLReader();

        reader.setContentHandler(new Handler(bean,externalContext));
        reader.parse(input);
      }
      catch (IOException ioe)
      {
        _LOG.warning(ioe);
      }
      catch (ParserConfigurationException pce)
      {
        _LOG.warning(pce);
      }
      catch (SAXException saxe)
      {
        _LOG.warning(saxe);
      }
      finally
      {
        try
        {
          in.close();
        }
        catch (IOException ioe)
        {
          // Ignore
          ;
        }
      }
    }

    String className = (String)
      bean.getProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY);
    if (className != null)
    {
      className = className.trim();

      try
      {
        Class<?> clazz = ClassLoaderUtils.loadClass(className);
        bean.setProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY,
                         clazz.newInstance());
      }
      catch (Exception e)
      {
        _LOG.severe("CANNOT_INSTANTIATE_UPLOADEDFILEPROCESSOR", e);
        bean.setProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY,
                         new UploadedFileProcessorImpl());
      }
    }
    else
    {
      bean.setProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY,
                       new UploadedFileProcessorImpl());
    }

    UploadedFileProcessor ufp = (UploadedFileProcessor)
      bean.getProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY);

    ufp.init(externalContext.getContext());

    if (_LOG.isInfo())
    {
      Object debug = bean.getProperty(RequestContextBean.DEBUG_OUTPUT_KEY);
      if (Boolean.TRUE.equals(debug))
        _LOG.info("RUNNING_IN_DEBUG_MODE",_CONFIG_FILE);
    }
    return bean;
  }

  static private class Handler extends DefaultHandler
  {
    @SuppressWarnings("unchecked")
    public Handler(RequestContextBean bean, ExternalContext context)
    {
        _applicationMap = context.getApplicationMap();
        _bean = bean;
    }

    @Override
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes atts)
    {
      _currentText = "";
    }

    @Override
    public void characters(char[] ch, int start, int length)
    {
      if (_currentText != null)
        _currentText = _currentText + new String(ch, start, length);
    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
    {
      String currentText = _currentText;
      if (currentText == null)
        return;
      
      currentText = currentText.trim();
      if (!"".equals(currentText))
      {
        PropertyKey key = _bean.getType().findKey(localName);
        if (key == null)
        {
          if (_LOG.isWarning())
            _LOG.warning("ELEMENT_NOT_UNDERSTOOD", qName);
        }
        else
        {
          if (currentText.startsWith("#{") &&
              currentText.endsWith("}"))
          {
            if (!key.getSupportsBinding())
            {
              if (_LOG.isWarning())
                _LOG.warning("NOT_SUPPORT_EL_EXPRESSION", qName);
            }
            else
            {
              ValueBinding binding =
                LazyValueBinding.createValueBinding(currentText);
              _bean.setValueBinding(key, binding);
            }
          }
          else
          {
            Object value;

            if (key.getType() == Character.class)
            {
              value = currentText.charAt(0);
            }
            else if (key.getType() == Integer.class)
            {
              value = _getIntegerValue(currentText, qName);
            }
            else if (key.getType() == Boolean.class)
            {
              value = ("true".equalsIgnoreCase(currentText)
                       ? Boolean.TRUE : Boolean.FALSE);
            }
            else if (key.getType() == TimeZone.class)
            {
              value = DateUtils.getSupportedTimeZone(currentText);
              if (value == null)
              {
                _LOG.warning("INVALID_TIMEZONE_IN_CONFIG", currentText);
              }
            }
            else if (key.getType() == Locale.class)
            {
              currentText = currentText.replace('_', '-');
              value = LocaleUtils.getLocaleForIANAString(currentText);
            }
            else if (key.getType().isEnum())
            {
              // TODO: warn when value is not OK
              try
              {
                value = Enum.valueOf((Class<? extends Enum>) key.getType(),
                                     currentText);
              }
              catch (IllegalArgumentException iae)
              {
                _LOG.warning("INVALID_ENUM_IN_CONFIG",
                             new Object[]{currentText, qName});
                return;
              }
            }
            else if (key.getType() == AccessibilityProfile.class)
            {
              value = _getAccessibilityProfile(currentText);
            }
            else
            {
              value = currentText;
            }

            if (key == RequestContextBean.REMOTE_DEVICE_REPOSITORY_URI)
            {
              _applicationMap.put("remote-device-repository-uri",value);
            }
            else if (key == RequestContextBean.CLIENT_VALIDATION_DISABLED_KEY)
            {
              if (Boolean.TRUE.equals(value))
                _bean.setProperty(RequestContextBean.CLIENT_VALIDATION_KEY,
                                  RequestContext.ClientValidation.DISABLED);
            }
            else
            {
              _bean.setProperty(key, value);
            }
          }
        }
      }

      _currentText = null;
    }

    private static Integer _getIntegerValue(String text, String qName)
    {
      Integer value = null;
      try
      {
        value = Integer.valueOf(text);
      }
      catch (NumberFormatException nfe)
      {
        if (_LOG.isWarning())
        {
          _LOG.warning("ELEMENT_ONLY_ACCEPT_INTEGER", qName);
        }
      }
      return value;
    }

    // Parses the text into an AccessibilityProfile.
    private static AccessibilityProfile _getAccessibilityProfile(
      String text)
    {
      AccessibilityProfile.ColorContrast colorContrast = null;
      AccessibilityProfile.FontSize fontSize = null;

      // Note: we do the parsing here in the ConfigParser instead of in 
      // RequestContextImpl so that we can easily detect/log any problems 
      // once at startup.  Also nice to do this here so that we have some 
      // chance of actually logging line numbers, though at the moment it 
      // looks like our Handler doesn't implement Locator.
      
      StringTokenizer tokens = new StringTokenizer(text);
      while (tokens.hasMoreTokens())
      {
        String token = tokens.nextToken();
        
        if ("high-contrast".equals(token))
        {
          colorContrast = AccessibilityProfile.ColorContrast.HIGH;
        }
        else if ("large-fonts".equals(token))
        {
          fontSize = AccessibilityProfile.FontSize.LARGE;
        }
        else
        {
          _LOG.warning("INVALID_ACC_PROFILE", new Object[]{token});
        }
      }

      return AccessibilityProfile.getInstance(colorContrast, fontSize);
    }

    private RequestContextBean  _bean;
    private String              _currentText;
    private Map<String, Object> _applicationMap;
  }

  private static final SAXParserFactory _SAX_PARSER_FACTORY;
  static
  {
      _SAX_PARSER_FACTORY = SAXParserFactory.newInstance();
      _SAX_PARSER_FACTORY.setNamespaceAware(true);
  } 

  static private final String _CONFIG_FILE = "/WEB-INF/trinidad-config.xml";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ConfigParser.class);
}
