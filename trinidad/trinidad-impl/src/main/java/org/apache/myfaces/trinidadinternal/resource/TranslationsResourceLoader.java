/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;

import java.net.URL;

import java.util.Enumeration;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.StringContentResourceLoader;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.share.nls.LocaleContextImpl;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

abstract public class TranslationsResourceLoader
                           extends StringContentResourceLoader
{
  /**
   * Constructs a dynamic resouce loader for this path which serves up
   * translations.
   * 
   * @param path the path of this dynamic resource loader
   */
  public TranslationsResourceLoader(String path)
  {
    super(path);
  }

  abstract protected String getJSVarName();

  abstract protected String getBundleName();

  /**
   * Override to increase the default size of the buffer.
   */
  protected int getDefaultSize()
  {
    return 10000;
  }

  protected String getLocaleString(FacesContext context)
  {
    Object localeObj = context.getExternalContext().getRequestParameterMap().
      get("loc");
    return (localeObj == null || "".equals(localeObj))
      ? null : localeObj.toString();
  }

  @Override
  protected String getContentType(String path)
  {
    return _CONTENT_TYPE;
  }
  
  @Override
  protected URL findResource(
    String path) throws IOException
  {
    return getURL(path);
  }

  @Override
  protected String getString(String path) throws IOException
  {
    FacesContext context = FacesContext.getCurrentInstance();
    String localeStr = getLocaleString(context);
    // Make sure it's in IANA format
    if (localeStr != null)
      localeStr = localeStr.replace('_', '-');

    Locale locale = LocaleUtils.getLocaleForIANAString(localeStr);
    if (locale == null)
      locale = Locale.getDefault();

    ResourceBundle bundle;
    try
    {
      bundle = _getResourceBundle(locale);
    }
    catch (MissingResourceException mre)
    {
      _LOG.severe("Could not find bundle " + getBundleName(), mre);
      return "/* COULD NOT FIND BUNDLE " + getBundleName() + " */";
    }

    // FIXME: would be much better to directly stream the contents
    // rather than using StringContentResourceLoader
    StringBuilder builder = new StringBuilder(getDefaultSize());

    builder.append(getJSVarName())
      .append("=")
      .append("{\n");

    _processBundle(context, builder, bundle, locale);
    
    builder.append("\n}");

    return builder.toString();
  }
  
  private ResourceBundle _getResourceBundle(Locale locale)
    throws MissingResourceException
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    return ResourceBundle.getBundle(getBundleName(),
                                    locale,
                                    loader);
  }

  protected Skin getSkin(FacesContext context)
  {
    Skin skin = null;
    SkinFactory skinFactory = SkinFactory.getFactory();
    Object skinIdObj = context.getExternalContext().getRequestParameterMap().
      get("skinId");
    if (skinIdObj != null)
      skin = skinFactory.getSkin(context, skinIdObj.toString());

    return skin;
  }

  private void _processBundle(
    FacesContext   context,
    StringBuilder  builder,
    ResourceBundle bundle,
    Locale         locale)
  {
    Skin             skin = getSkin(context);
    LocaleContext    lc = new LocaleContextImpl(locale);

    // We get the keys from the bundle, but try to get the values from
    // the skin if possible
    Enumeration<String> keys = bundle.getKeys();
    boolean writtenOne = false;
    while (keys.hasMoreElements())
    {
      if (writtenOne)
        builder.append(",\n");
      else
        writtenOne = true;

      String key = keys.nextElement();
      String value;
      // If we can get it from the skin, that's better, but if not,
      // go to the bundle
      if (skin == null)
        value = bundle.getString(key);
      else
        value = skin.getTranslatedString(lc, key);
      
      builder.append("'");
      builder.append(key);
      builder.append("':'");
      _appendUnicodeString(builder, value);
      builder.append("'");
    }
  }

  private void _appendUnicodeString(
    StringBuilder builder,
    String        value)
  {
    if (value == null)
      return;

    int length = value.length();
    for (int i = 0; i < length; i++)
    {
      char c = value.charAt(i);
      if ((c >= 0x20) && (c < 0x80))
      {
        if (c == '\'')
          builder.append("\\'");
        else if (c == '\\')
          builder.append("\\\\");
        else
          builder.append(c);
      }
      else
      {
        // Unicode escape any non-ascii characters
        builder.append("\\u");
        String hex = Integer.toHexString(c);
        int hexLen = hex.length();
        // Javascript is lame, and requires padding Unicode escapes
        // to four-digits
        if (hexLen == 1)
          builder.append("000");
        else if (hexLen == 2)
          builder.append("00");
        else if (hexLen == 3)
          builder.append("0");
        builder.append(hex);
      }
    }
  }

  private static final String _CONTENT_TYPE = "text/javascript";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
                                       TranslationsResourceLoader.class);
}
