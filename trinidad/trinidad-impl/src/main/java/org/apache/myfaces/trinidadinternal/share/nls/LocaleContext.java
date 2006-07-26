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
package org.apache.myfaces.adfinternal.share.nls;

import java.util.Hashtable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TimeZone;

import org.apache.myfaces.adf.util.ClassLoaderUtils;

import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;

/**
 * Context for locale-specific operations and properties.  All of the properties
 * should initially default to those of the base Locale, while allowing
 * the locale-specific properties to be overridden.
 * <p>
 * It is expected that additional properties will be added to this class
 * over time in order to support overriding the date and number formats.
 * <p>
 * Clients should never subclass this class.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/nls/LocaleContext.java#0 $) $Date: 10-nov-2005.19:00:03 $
 * @author The Oracle ADF Faces Team
 */
public class LocaleContext
{
  /**
   * Creates a LocaleContext based off of the default Locale.
   */
  public LocaleContext()
  {
    this(null);
  }


  /**
   * Creates a LocaleContext based off of the specified Locale.
   */
  public LocaleContext(
    Locale baseLocale
    )
  {
    this(baseLocale, null);
  }


  /**
   * Creates a LocaleContext based off of the specified Locale and using
   * a different Locale for translations.  Applications that only provide
   * translations for a subset of the Locales provided by subcomponents
   * can use the translation Locale to force subcomponents to only
   * use translations in a language supported by the application.
   * <p>
   * @param baseLocale Locale providing default behavior for the LocaleContext.
   *                   If not specified, the defualt Locale is used.
   * @param translationLocale Locale to use for translations.  If not
   *                          specified, the baseLocale is used.
   */
  public LocaleContext(
    Locale baseLocale,
    Locale translationLocale
    )
  {
    if (!getClass().getName().startsWith("org.apache.myfaces.adfinternal.share.nls."))
      throw new IllegalStateException("User-defined subclasses not supported.");

    if (baseLocale == null)
    {
      baseLocale = Locale.getDefault();
    }

    _locale = baseLocale;

    // default the translation locale to the baseLocale
    if (translationLocale == null)
      translationLocale = baseLocale;

    _transLocale = translationLocale;

    setTimeZone(null);
  }

  /**
   * Returns the locale that should be used for rendering.  Attributes
   * on the current node should override this.
   */
  public Locale getLocale()
  {
    return _locale;
  }

  /**
   * Returns the locale that should be used for translations..
   */
  public Locale getTranslationLocale()
  {
    return _transLocale;
  }


  /**
   * Returns the Locale in IANA String format.
   */
  public String getIANALocaleString()
  {
    if (_ianaLocale == null)
    {
      String localeString = _locale.toString();

      _ianaLocale = localeString.replace('_', '-');
    }

    return _ianaLocale;
  }


  /**
   * Returns the translation Locale in IANA String format.
   */
  public String getTranslationIANALocaleString()
  {
    if (_transIanaLocale == null)
    {
      String transLocaleString = getTranslationLocale().toString();

      _transIanaLocale = transLocaleString.replace('_', '-');
    }

    return _transIanaLocale;
  }

  public boolean isRightToLeft()
  {
    return (getReadingDirection() == LocaleUtils.DIRECTION_RIGHTTOLEFT);
  }

  /**
   * Returns the reading direction that should be used for rendering.
   * @return a reading direction from
   * <code>org.apache.myfaces.adfinternal.util.nls.LocaleUtils</code>.
   * This method will never return
   * <code>LocaleUtils.DIRECTION_DEFAULT</code>.
   * <p>
   * @see #setReadingDirection
   * @see org.apache.myfaces.adfinternal.util.nls.LocaleUtils
   */
  public int getReadingDirection()
  {
    if (_readingDirection != LocaleUtils.DIRECTION_DEFAULT)
    {
      return _readingDirection;
    }
    else
    {
      return LocaleUtils.getReadingDirectionForLocale(getTranslationLocale());
    }
  }


  /**
   * Sets the new reading direction to be one of the reading directions
   * defined in <code>org.apache.myfaces.adfinternal.util.nls.LocaleUtils</code>.
   *
   * @deprecated  use MutableLocaleContext.setReadingDirection() instead
   * @see org.apache.myfaces.adfinternal.share.nls.MutableLocaleContext
   */
  public void setReadingDirection(
    int newReadingDirection
    )
  {
    if ((newReadingDirection != LocaleUtils.DIRECTION_DEFAULT) &&
        (newReadingDirection != LocaleUtils.DIRECTION_LEFTTORIGHT) &&
        (newReadingDirection != LocaleUtils.DIRECTION_RIGHTTOLEFT))
    {
      throw new IllegalArgumentException("Unknown reading direction:" +
                                         newReadingDirection);
    }

    _readingDirection = newReadingDirection;
  }


  /**
   * Returns the TimeZone that the user is running in.
   */
  public TimeZone getTimeZone()
  {
    return _timeZone;
  }


  /**
   * Sets the TimeZone that the user is running in.  Setting this value
   * to null will set the TimeZone to the default TimeZone.
   *
   * @deprecated  use MutableLocaleContext.setTimeZone() instead
   * @see org.apache.myfaces.adfinternal.share.nls.MutableLocaleContext
   */
  public void setTimeZone(
    TimeZone newTimeZone
    )
  {
    if (newTimeZone == null)
    {
      newTimeZone = TimeZone.getDefault();
    }

    _timeZone = newTimeZone;
  }


  /**
   * Override of Object.toString().
   */
  public String toString()
  {
    StringBuffer buffer = new StringBuffer(super.toString());

    buffer.append(" locale=");
    buffer.append(getLocale());
    buffer.append(", direction=");
    buffer.append(getReadingDirection());
    buffer.append(", timeZone=");
    buffer.append(getTimeZone());
    buffer.append(", dateFormatContext=");
    buffer.append(getDateFormatContext());
    buffer.append(", decimalFormatContext=");
    buffer.append(getDecimalFormatContext());

    return buffer.toString();
  }


  /**
   * Returns the resource bundle with the specified name, for this
   * <strong>translation</strong> locale.
   * <p>
   * As the LocaleContext maintains a cache of found ResourceBundles,
   * this is much faster than using
   * <code>ResourceBundle.getBundle</code>
   * <p>
   * @see java.util.ResourceBundle#getBundle
   */
  public ResourceBundle getBundle(
    String baseBundleName
    ) throws MissingResourceException
  {
    if (_bundles == null)
    {
      _bundles = new Hashtable(13);
    }

    ResourceBundle bundle = (ResourceBundle)_bundles.get(baseBundleName);

    if (bundle == null)
    {
      ClassLoader loader = ClassLoaderUtils.getContextClassLoader();

      Locale translationLocale = getTranslationLocale();

      if (loader == null)
      {
        bundle = ResourceBundle.getBundle(baseBundleName, translationLocale);
      }
      else
      {
        bundle = ResourceBundle.getBundle(baseBundleName,
                                          translationLocale,
                                          loader);
      }

      // cache the bundle
      _bundles.put(baseBundleName, bundle);
    }

    return bundle;
  }

  /**
   * Returns the DateFormatContext containing all date format parameters,
   * falling back on defaults when <code>getDateFormatContextImpl</code>
   * returns null;
   */
  public final DateFormatContext getDateFormatContext()
  {
    DateFormatContext dfc = getDateFormatContextImpl();

    if (dfc == null)
      dfc = _sDefaultDateFormatContext;

    return dfc;
  }

  /**
   * Returns the DecimalFormatContext containing all number format parameters,
   * falling back on defaults when <code>getDecimalFormatContextImpl</code>
   * returns null;
   */
  public final DecimalFormatContext getDecimalFormatContext()
  {
    DecimalFormatContext dfc = getDecimalFormatContextImpl();

    if (dfc == null)
      dfc = _sDefaultDecimalFormatContext;

    return dfc;
  }


  /**
   * Override of Object.hashCode().
   */
  public int hashCode()
  {
    return getLocale().hashCode();
  }

  /**
   * Override of Object.equals().
   */
  public boolean equals(Object obj)
  {
    if (obj == this)
      return true;

    if (obj == null)
      return false;

    LocaleContext that = (LocaleContext)obj;

    return
      (this.getLocale().equals(that.getLocale())                         &&
       this.getTranslationLocale().equals(that.getTranslationLocale())   &&
       this.getTimeZone().equals(that.getTimeZone())                     &&
       (this.getReadingDirection() == that.getReadingDirection())        &&
       this.getDateFormatContext().equals(that.getDateFormatContext())   &&
       this.getDecimalFormatContext().equals(that.getDecimalFormatContext()));
  }

  /**
   * Returns the DateFormatContext containing all date format parameters.
   * If this method returns null, <code>getDateFormatContext</code> will
   * use the default value instead.
   */
  protected DateFormatContext getDateFormatContextImpl()
  {
    // use defaults
    return null;
  }

  /**
   * Returns the DecimalFormatContext containing all number format parameters.
   * If this method returns null, <code>getDecimalFormatContext</code> will
   * use the default value instead.
   */
  protected DecimalFormatContext getDecimalFormatContextImpl()
  {
    // use defaults
    return null;
  }


  static private class DefaultDecimal extends DecimalFormatContext
  {
    public char getGroupingSeparator()
    {
      return (char) 0;
    }

    public char getDecimalSeparator()
    {
      return (char) 0;
    }
  }

  static private class DefaultDate extends DateFormatContext
  {
    public int getTwoDigitYearStart()
    {
      return 1950;
    }
  }

  private static final DateFormatContext _sDefaultDateFormatContext =
                                                new DefaultDate();
  private static final DecimalFormatContext _sDefaultDecimalFormatContext =
                                                new DefaultDecimal();

  private Hashtable _bundles;

  private Locale   _locale;
  private Locale   _transLocale;
  private transient String _ianaLocale;
  private transient String _transIanaLocale;
  private TimeZone _timeZone;

  private int _readingDirection = LocaleUtils.DIRECTION_DEFAULT;
}
