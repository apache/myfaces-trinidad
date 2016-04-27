/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.skin;

import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.util.Args;
import org.apache.myfaces.trinidad.util.ToStringHelper;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.cache.FileSystemStyleCache;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.util.CopyOnWriteArrayMap;


/**
 * An extension of the FileSystemStyleCache which defers to
 * a Skin for loading the StyleSheetDocument.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleProvider.java#0 $) $Date: 10-nov-2005.18:58:59 $
 */
public class SkinStyleProvider extends FileSystemStyleCache
{
  /**
   * Returns a shared instance of the SkinStyleProvider.
   * The StyleProvider combines styles from two sources.  First,
   * styles are pulled from the current Skin, which is
   * retrieved from the RenderingContext.  Then, styles are pulled
   * from the custom style sheet, as identified by the (possibly null)
   * customStyleSheetPath argument.  Styles specified by the custom
   * style sheet take precedence over styles provided by the
   * Skin.
   *
   * @param skin The skin for which the style provider is needed.
   * @param targetDirectoryPath The full file system path of the
   *          directory where generated CSS files are stored.
   *          If the directory does not exist and cannot be
   *          created, an IllegalArgumentException is thrown.
   * @throws IllegalArgumentException This exception is thrown
   *         if no Skin is found, or if either of the
   *         paths are invalid.
   */
  public static StyleProvider getSkinStyleProvider(
    Skin   skin,
    String targetDirectoryPath
    ) throws IllegalArgumentException
  {
    if (skin == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_SKIN_SPECIFIED"));

    // If the skin is actually one of our request-specific wrapper
    // skins, rip off the wrapper and look up the StyleProvider
    // for the wrapped skin.  If we don't do this, we end up creating
    // a new StyleProvider for every request.
    if (skin instanceof RequestSkinWrapper)
      skin = ((RequestSkinWrapper)skin).getWrappedSkin();

    // Create the key object that we use to look up our
    // shared SkinStyleProvider instance
    ProviderKey key = new ProviderKey(skin,
                                      targetDirectoryPath);

    // Get our cache of existing StyleProviders
    ConcurrentMap<ProviderKey, StyleProvider> providers = _getProviders();
    StyleProvider provider = providers.get(key);

    if (provider != null)
    {
      _LOG.fine("Style provider served from cache {0}", provider);
      return provider;
    }

    // If we haven't created an instance for this skin/custom style sheet
    // yet, create it now.
    provider = new SkinStyleProvider(skin,
                                     targetDirectoryPath);
    // assume caching is "on"
    boolean cacheStyleProvider = true;
    boolean logStyleProviderCreated = false;

    // skin framework caches only internal skins.
    // Internal skins are marked by internal SkinProviders.
    // So essentially skins created by external SkinProviders or using
    // SkinFactory.createSkin are not internal skins.
    if (skin instanceof SkinImpl && !((SkinImpl) skin).isCacheable())
    {
      cacheStyleProvider = false;
      logStyleProviderCreated = true;
    }

    // Store the provider in our cache
    if (cacheStyleProvider)
    {
      StyleProvider existing = providers.putIfAbsent(key, provider);
      if (existing != null)
        provider = existing;
      else
        logStyleProviderCreated = true;
    }

    if (logStyleProviderCreated && _LOG.isFine())
      _LOG.fine("Create a new SkinStyleProvider for skin {0} and targetDirectoryPath {1}. Skin cacheability is: {2}",
                new Object[]{skin.getId(), targetDirectoryPath, cacheStyleProvider});

    return provider;
  }

  @Override
  public String toString()
  {
    return new ToStringHelper(this)
        .append("skinId", _skin.getId())
        .toString();
  }

   /**
   * Creates SkinStyleProvider instance.
   * Only subclasses should call this method.  All other
   * clients should use getSkinStyleProvider().
   * @param skin The Skin which defines the
   *   look and feel-specific style information for this
   *   StyleProvider.
   * @param targetDirectoryPath The full file system path
   *   to the directory where generated CSS files will be
   *   stored.
   * @see #SkinStyleProvider
   * @throws IllegalArgumentException This exception is thrown
   *         if no Skin is null, or if either of the
   *         paths are invalid.
   */
  protected SkinStyleProvider(
    Skin skin,
    String targetDirectoryPath
    ) throws IllegalArgumentException
  {
    super(targetDirectoryPath);

    if (skin == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "NO_SKIN_SPECIFIED"));

    _skin = skin;
  }

  /**
   * Override of FileSystemStyleCache.createStyleSheetDocument().
   * Merges the Skin's styles with custom styles to
   * produce a single StyleSheetDocument
   */
  @Override
  protected StyleSheetDocument createStyleSheetDocument(
    StyleContext context
    )
  {
    // First, get the StyleSheetDocument for the custom style
    // sheet from the FileSystemStyleCache.
    StyleSheetDocument customDocument = super.createStyleSheetDocument(
                                                context);

    // Now, get the Skin's StyleSheetDocument
    StyleSheetDocument skinDocument = null;

    // Synchronize access to _skinDocument
    synchronized (this)
    {
      // gets the skin's StyleSheetDocument (it creates it if needed)
      skinDocument = _skinDocument =
        ((DocumentProviderSkin) _skin).getStyleSheetDocument(context);
    }


    // Merge the two StyleSheetDocuments
    return StyleSheetDocumentUtils.mergeStyleSheetDocuments(skinDocument,
                                                            customDocument);
  }

  /**
   * Override of FileSystemStyleCache.hasSourceDocumentChanged()
   * which checks for changes to the Skin's style sheet.
   */
  @Override
  protected boolean hasSourceDocumentChanged(StyleContext context)
  {
    if (super.hasSourceDocumentChanged(context))
      return true;

    // Just check to see whether the Skin has a new
    // StyleSheetDocument.

    // Synchronize access to _skinDocument
    synchronized (this)
    {
      return (_skinDocument !=
              ((DocumentProviderSkin) _skin).getStyleSheetDocument(context));
    }
  }

  /**
   * Override of FileSystemStyleCache.getTargetStyleSheetName().
   */
  @Override
  protected String getTargetStyleSheetName(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    // Get the base name from the FileSystemStyleCache.
    String name = super.getTargetStyleSheetName(context, document);

    // Use the LAF's id as a prefix
    String id = _skin.getId();
    if (id != null)
    {
      StringBuffer buffer = new StringBuffer(id.length() + name.length() + 1);

      // We know that some LAF ids contain the '.' character.  Replace
      // this with '-' to make the file name look nicer.
      buffer.append(id.replace('.', '-'));
      buffer.append('-');
      buffer.append(name);

      return buffer.toString();
    }

    return name;
  }

  // Returns a Map which hashes ProviderKeys to shared instances of SkinStyleProviders.
  private static ConcurrentMap<ProviderKey, StyleProvider> _getProviders()
  {
    ConcurrentMap<String, Object> appMap = 
      RequestContext.getCurrentInstance().getApplicationScopedConcurrentMap();

    ConcurrentMap<ProviderKey, StyleProvider> styleProviders =
      (ConcurrentMap<ProviderKey, StyleProvider>)appMap.get(_SKIN_PROVIDERS_KEY);

    if (styleProviders == null)
    {
      styleProviders = _createProvidersCache();

      ConcurrentMap<ProviderKey, StyleProvider> oldStyleProviders =
        (ConcurrentMap<ProviderKey, StyleProvider>)appMap.putIfAbsent(_SKIN_PROVIDERS_KEY, styleProviders);

      if (oldStyleProviders != null)
        styleProviders = oldStyleProviders;
    }

    return styleProviders;
  }
  
  private static ConcurrentMap<ProviderKey, StyleProvider> _createProvidersCache()
  {
    return CopyOnWriteArrayMap.newLRUConcurrentMap(_getCacheSize());
  }

  private static int _getCacheSize()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    String lruCacheSize = context.getExternalContext().getInitParameter(_MAX_SKINS_CACHED);
    int lruCacheSizeInt = _MAX_SKINS_CACHED_DEFAULT;
    boolean invalidInt = false;

    if (lruCacheSize != null && !lruCacheSize.isEmpty())
    {
      try
      {
        lruCacheSizeInt = Integer.parseInt(lruCacheSize);
      }
      catch (NumberFormatException nfe)
      {
        invalidInt = true;
      }
      if (lruCacheSizeInt <= 0)
      {
        invalidInt = true;
      }
    }

    // Invalid number or number less than zero specified by used in context parameter
    // so log a warning
    if (invalidInt)
    {
      if (_LOG.isWarning())
        _LOG.warning("INVALID_INTEGER_MAX_SKINS_CACHED", new Object[]{lruCacheSize, _MAX_SKINS_CACHED_DEFAULT});
      // re-initialize to max size because it could have been assigned to a negative number above while parsing
      lruCacheSizeInt = _MAX_SKINS_CACHED_DEFAULT;
    }

    return lruCacheSizeInt;
  }

  // Key that we use to retrieve a shared SkinStyleProvider
  // instance
  private static class ProviderKey
  {
    public ProviderKey(
      Skin skin,
      String targetDirectoryPath
      )
    {
      Args.notNull(skin, "skin");
      Args.notNull(targetDirectoryPath, "targetDirectoryPath");

      _skinId = skin.getId();
      _targetDirectoryPath = targetDirectoryPath;
    }

    @Override
    public String toString()
    {
      return new ToStringHelper(this)
          .append("skinId", _skinId)
          .append("targetDirectoryPath", _targetDirectoryPath)
          .toString();
    }

    // Test for equality
    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if (!(o instanceof ProviderKey))
        return false;

      ProviderKey key = (ProviderKey)o;

      return (_equals(_skinId, key._skinId) &&
              _equals(_targetDirectoryPath, key._targetDirectoryPath));
    }

    // Produce the hash code
    @Override
    public int hashCode()
    {
      int result = 17;
      result = 37 * result + _skinId.hashCode();
      result = 37 * result + _targetDirectoryPath.hashCode();
      return result;
    }

    // Tests two objects for equality, taking possible nulls
    // into account
    private boolean _equals(Object o1, Object o2)
    {
      if (o1 == null)
        return (o1 == o2);

      return o1.equals(o2);
    }

    private final String _skinId;
    private final String _targetDirectoryPath;
  }

  // The Skin which provides styles
  private Skin _skin;

  // The Skin-specific StyleSheetDocument
  private StyleSheetDocument _skinDocument;

  // Key to cache of shared SkinStyleProvider instances
  private static final String _SKIN_PROVIDERS_KEY =
    "org.apache.myfaces.trinidadinternal.skin.SKIN_PROVIDERS_KEY";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SkinStyleProvider.class);
  private static final String _MAX_SKINS_CACHED =
    "org.apache.myfaces.trinidad.skin.MAX_SKINS_CACHED";
  private static final int _MAX_SKINS_CACHED_DEFAULT = 20;
}
