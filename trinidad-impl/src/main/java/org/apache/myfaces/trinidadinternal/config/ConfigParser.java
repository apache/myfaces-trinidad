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
import java.util.TimeZone;

import javax.faces.context.ExternalContext;
import javax.faces.el.ValueBinding;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.config.upload.UploadedFileProcessorImpl;
import org.apache.myfaces.trinidadinternal.context.RequestContextBean;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
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

        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        XMLReader reader = factory.newSAXParser().getXMLReader();

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
        _LOG.severe("Could not instantiate UploadedFileProcessor", e);
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
        _LOG.info("Trinidad is running in debug mode. "+
                  "Do not use in a production environment. See:"+_CONFIG_FILE);
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
      if ((_currentText != null) && !"".equals(_currentText))
      {
        PropertyKey key = _bean.getType().findKey(localName);
        if (key == null)
        {
          if (_LOG.isWarning())
            _LOG.warning("Element {0} is not understood", qName);
        }
        else
        {
          if (_currentText.startsWith("#{") &&
              _currentText.endsWith("}"))
          {
            if (!key.getSupportsBinding())
            {
              if (_LOG.isWarning())
                _LOG.warning("Element {0} does not support EL expressions.",
                             qName);
            }
            else
            {
              ValueBinding binding =
                LazyValueBinding.createValueBinding(_currentText);
              _bean.setValueBinding(key, binding);
            }
          }
          else
          {
            Object value;

            if (key.getType() == Character.class)
            {
              value = _currentText.charAt(0);
            }
            else if (key.getType() == Integer.class)
            {
              value = _getIntegerValue(_currentText, qName);
            }
            else if (key.getType() == Boolean.class)
            {
              value = ("true".equalsIgnoreCase(_currentText)
                       ? Boolean.TRUE : Boolean.FALSE);
            }
            else if (key.getType() == TimeZone.class)
            {
              value = TimeZone.getTimeZone(_currentText);
            }
            else if (key.getType() == Locale.class)
            {
              value = LocaleUtils.getLocaleForIANAString(_currentText);
            }
            else
            {
              value = _currentText;
            }
            if (key == RequestContextBean.REMOTE_DEVICE_REPOSITORY_URI){
                _applicationMap.put("remote-device-repository-uri",value);
            }
            _bean.setProperty(key, value);
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
        _LOG.warning("Element {0} only accepts integer values",
                     qName);
      }
    }
    return value;
  }


    private RequestContextBean  _bean;
    private String              _currentText;
    private Map<String, Object> _applicationMap;
  }

  static private final String _CONFIG_FILE = "/WEB-INF/trinidad-config.xml";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ConfigParser.class);
}
