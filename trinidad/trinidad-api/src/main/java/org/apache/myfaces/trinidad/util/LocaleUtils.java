/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidad.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.ADFLogger;


/**
 * Used for getting localized strings for Validators and Converters from the
 * Resource Bundle.&nbsp; First checks for strings in application specific
 * bundle.&nbsp; If not found checks for the resource in faces bundle.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/LocaleUtils.java#0 $) $Date: 10-nov-2005.19:08:38 $
 * @author The Oracle ADF Faces Team
 */
class LocaleUtils
{
  private LocaleUtils()
  {
  }

  // Looks for the key and key_detail values in the resource bundle and
  // returns it after formatting it with the parameters in the place holders
  // of the obtained value for the given key.
  static ErrorMessages __getErrorMessages(
    FacesContext context,
    String resourceId
    )
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, resourceId);
    String summary = info.getSummary();
    if (summary == null)
    {
      summary = "???" + resourceId + "???";
    }
    ResourceBundle bundle  = info.getBundle();
    if (null == bundle)
    {
      throw new NullPointerException("bundle not found");
    }

    // Look up for key_detail now
    String detailKey = _getDetailKey(resourceId);
    String detail = _getBundleString(bundle, detailKey);
    if (detail == null)
    {
      detail = "???" + resourceId + "_detail???";
    }

    return new ErrorMessages(summary, detail);
  }

  private static ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = ClassLoader.getSystemClassLoader();

    return loader;
  }

  static String __getSummaryString(
    FacesContext context,
    String messageId)
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, messageId);
    return info.getSummary();
  }

  static String __getDetailString(
    FacesContext context,
    String messageId)
  {
    BundleSummaryInfo info = _identifyBundleSummaryInfo(context, messageId);
    ResourceBundle bundle  = info.getBundle();
    if (null == bundle)
    {
      throw new NullPointerException("bundle not found");
    }

    // Look up for key_detail now
    String detailKey = _getDetailKey(messageId);
    return _getBundleString(bundle, detailKey);

  }

  private static String _getDetailKey(
    String messageId)
  {
    return messageId + "_detail";
  }

  private static BundleSummaryInfo _identifyBundleSummaryInfo(
    FacesContext context,
    String resourceId
    )
  {
    _assertContextNotNull(context);

    if (null == resourceId)
      throw new NullPointerException("resourceId is null");

    Locale locale = _getLocale(context);
    ClassLoader loader = _getClassLoader();

    ResourceBundle bundle = _getAdfFacesMessageBundle(locale, loader);
    String summary = null;

    // unable to find adf faces app level bundle
    if (null != bundle)
    {
      summary = _getBundleString(bundle, resourceId);
    }

    // Lookup in faces bundle
    if (null == summary)
    {
      bundle = _getFacesMessageBundle(locale, loader);

      if (null == bundle)
        throw new NullPointerException("faces bundle not available ");
      summary = _getBundleString(bundle, resourceId);
    }

    return new BundleSummaryInfo(bundle, summary);
  }

  /**
   * Return the bundle for the appropriate locale.&nbsp;First checks for the
   * ADF Faces application level Message bundle in the cache, if not found
   * looks up for faces bundle.&nbsp; Once the bundle is found it is
   * cached for faster retrieval in subsequent calls.
   *
   * @param locale Locale for which the bundle is to be indentifed
   * @param loader ClassLoader to pickup the bundle
   * @return Resource bundle for the given locale.
   */
  private static ResourceBundle _getAdfFacesMessageBundle(
    Locale locale,
    ClassLoader loader
    )
  {
    ResourceBundle bundle = _getCachedBundle(locale);

    // if not available in cache
    if (null == bundle)
    {
      try
      {
        bundle = ResourceBundle.getBundle(_ADF_FACES_MESSAGE_BUNDLE,
                                          locale, loader);
        // let us cache the found bundle
        _cacheBundle(locale, bundle);
      }
      catch (MissingResourceException missingResource)
      {
        _LOG.severe("unable to load bundle " +_ADF_FACES_MESSAGE_BUNDLE);
        _LOG.severe(missingResource);
      }
    }

    return bundle;
  }

  private static ResourceBundle _getFacesMessageBundle(
    Locale locale,
    ClassLoader loader
    )
  {
    ResourceBundle bundle = null;
    try
    {
      bundle =
        ResourceBundle.getBundle(FacesMessage.FACES_MESSAGES, locale, loader);
    }
    catch (MissingResourceException missingResource)
    {
      _LOG.severe("Unable to load faces bundle " + FacesMessage.FACES_MESSAGES);
      _LOG.severe(missingResource);
    }
    return bundle;
  }

  private static Locale _getLocale(FacesContext context)
  {
    Locale locale = null;
    if (context.getViewRoot() != null)
      locale = context.getViewRoot().getLocale();

    if (locale == null)
      locale = Locale.getDefault();

    return locale;
  }

  private static ResourceBundle _getCachedBundle(Locale locale)
  {
    return (ResourceBundle) _bundleCache.get(locale);
  }

  private static void _cacheBundle(Locale locale, ResourceBundle bundle)
  {
    _bundleCache.put(locale, bundle);
  }

  private static void _assertContextNotNull(FacesContext context)
  {
    if (null == context)
      throw new NullPointerException("FacesContext is null");
  }

  /**
   * @param bundle Bundle in which translated string is to be found for given key
   * @param key
   * @return
   */
  private static String _getBundleString(ResourceBundle bundle, String key)
  {
    Object localeStr = null;
    try
    {
      localeStr =  bundle.getObject(key);
      if (null != localeStr )
        localeStr = localeStr.toString();
    }
    catch (MissingResourceException mre)
    {
      _LOG.warning("Key " + key + " not found in " + bundle);
    }

    return (String)localeStr;
  }


  // Encapuslation which stores the identified bundle and the summary message.
  private static class BundleSummaryInfo
  {
    BundleSummaryInfo(ResourceBundle bundle, String summary)
    {
      _bundle  = bundle;
      _summary = summary;
    }

    public ResourceBundle getBundle()
    {
      return _bundle;
    }

    public String getSummary()
    {
      return _summary;
    }

    private ResourceBundle _bundle;
    private String  _summary;
  }


  private static final String _ADF_FACES_MESSAGE_BUNDLE
    = "org.apache.myfaces.adf.resource.MessageBundle";

  // cache Bundles based on locale
  private static  Map  _bundleCache;

  static
  {
    _bundleCache = Collections.synchronizedMap(new HashMap(13));
  }

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(LocaleUtils.class);
}
