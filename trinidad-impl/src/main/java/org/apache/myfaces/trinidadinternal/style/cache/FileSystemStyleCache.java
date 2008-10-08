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
package org.apache.myfaces.trinidadinternal.style.cache;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.StyleSheetRenderer;
import org.apache.myfaces.trinidadinternal.share.io.CachingNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.InputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.xml.JaxpXMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.Style;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleMap;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.util.CSSGenerationUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleWriterFactory;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.SkinPropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.xml.sax.SAXException;


/**
 * The FileSystemStyleCache is a StyleProvider implementation which
 * caches generated CSS style sheets on the file system.
 * 
 * Note that StyleProviders are responsible for providing access
 * both to style information (eg. getStyleSheetURI(), getStyleMap()) as
 * well as to icons registered via style sheets (see getIcons()).
 *
 * @see org.apache.myfaces.trinidadinternal.style.StyleProvider
 * @see org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/cache/FileSystemStyleCache.java#0 $) $Date: 10-nov-2005.18:58:54 $
 */
// -= Simon Lessard =-
// TODO: Synchronization does not seem to be needed since there's
//       synchronized blocks in the code, using HashMap hence
//       looks like a better choice than Hashtable.
public class FileSystemStyleCache implements StyleProvider
{
  /**
   * Returns the mime type for the styles provided by this
   * FileSystemStyleCache - "text/css".
   */
  public String getContentStyleType(StyleContext context)
  {
    return "text/css";
  }

  /**
   * Creates a FileSystemStyleCache.
   *
   * @param source The path of the source XSS document.  The
   *   specified file must be a valid XSS document.  If the specified
   *   file does not exists, an IllegalArgumentException is thrown.
   * @param target The path of the target directory.  Generated
   *   CSS files are stored in this directory.  If the directory
   *   does not exist and can not be created, an IllegalArgumentException
   *   is thrown.
   */
  protected FileSystemStyleCache(String source, String target)
  {
    // The source arg may actually be null if we are using a
    // SkinStyleProvider with no custom style sheet.
    if (source != null)
    {
      File sourceFile = new File(source);

      if (!sourceFile.exists())
        throw new IllegalArgumentException(
        "\nRequired XSS file " +
        source +
        " does not exist.");
      _sourceFile = sourceFile;

      // Get the base file name: name minus the extension
      String baseName = sourceFile.getName();
      int dotIndex = baseName.indexOf('.');
      if (dotIndex != -1)
        baseName = baseName.substring(0, dotIndex);
      _baseName = baseName;
    }

    // If the target directory does not exist, create it now.
    // Note: If we can't create the target directory, we just
    // plug along anyway instead of throwing an IllegalArgumentException.
    // That way, we can still use the StyleMap for this style sheet
    // even if the style sheet isn't generated.
    File targetDirectory = new File(target);
    if (!targetDirectory.exists())
      targetDirectory.mkdirs();

    _targetPath = target;
  }

  /**
   * Implementation of StyleCache.getStyleSheetURI().
   */
  public List<String> getStyleSheetURIs(StyleContext context)
  {
    Entry entry = _getEntry(context);

    if (entry == null)
    {
      return null;
    }

    return entry.uris;
  }

  /**
   * Implementation of StyleProvider.getStyleMap().
   */
  public StyleMap getStyleMap(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;

    return entry.map;
  }
  
  /**
   * Implementation of StyleProvider.getSkinProperties()
   */
  public ConcurrentMap<Object, Object> getSkinProperties(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;

    return entry.skinProperties;
  }
  
  /**
   * Implementation of StyleProvider.getIcons()
   */
  public ConcurrentMap<String, Icon> getIcons(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;

    return entry.icons;
  }

  /**
   * Returns a Map which maps style class names to
   * equivalent shorter names.
   * <p>
   * FileSystemStyleCache automatically generates short versions
   * of every style class that is found the the underlying XSS
   * document.  FileSystemStyleCache clients can reduce the
   * size of generated content by using this method to obtain
   * short versions of any rendered style classes.
   * <p>
   * Note: The returned Map uses String keys to represent
   * the full class names.  However, the short style class values
   * may not necessarily be type java.lang.String.  Clients must
   * avoid explicitly casting the values contained in the Map
   * to type String.  Instead, such values should be passed directly
   * to the ResponseWriter API to be rendered.  Or, if the String
   * representation is required, toString() should be called on
   * the value.
   *
   * @param context The StyleContext
   *
   * @return A Map which maps the full style class names to
   *   the shorter equivalents.
   */
  public Map<String, String> getShortStyleClasses(StyleContext context)
  {
    return _shortStyleClassMap;
  }

  /**
   * Creates the StyleSheetDocument for this StyleProvider.
   * @param context The StyleContext
   *                (not needed here, but is needed in  subclass)
   * @return The StyleSheetDocument which defines the styles
   *         for this StyleProvider.
   */
  protected StyleSheetDocument createStyleSheetDocument(
    StyleContext context
    )
  {
    // If we don't have a sourceFile, we don't have a StyleSheetDocument
    if (_sourceFile == null)
      return null;

    StyleSheetDocument document = null;

    // Get the XML Provider
    XMLProvider provider = new JaxpXMLProvider();

    try
    {
      document = StyleSheetDocumentUtils.createStyleSheetDocument(
                                                        provider,
                                                        _resolver,
                                                        _sourceFile.getPath());
    }
    catch (SAXException e)
    {
      // The error is logged by the TreeBuilder
      ;
    }
    catch (IOException e)
    {
      if (_LOG.isWarning())
        _LOG.warning("IOEXCEPTION_IN_PHASE", _sourceFile);
        _LOG.warning(e);
    }

    return document;
  }

  /**
   * Tests whether the source style sheet files have been modified
   * since the last call to createStyleSheetDocument().
   * @return true if the underlying source style sheets have been
   *              modified, false otherwise.
   */
  protected boolean hasSourceDocumentChanged(StyleContext context)
  {
    // If we haven't parsed yet, don't bother checking the time stamp
    if (_document == null)
      return true;

    InputStreamProvider provider = _getInputStreamProvider();
    if (provider != null)
      return provider.hasSourceChanged();

    // Couldn't get an InputStreamProvider, what to do?
    return false;
  }

  /**
   * Returns the name to use for the generated style sheet file .
   *
   * @param context The StyleContext
   * @param document The StyleSheetDocument which provides the styles
   */
  protected String getTargetStyleSheetName(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    StringBuffer buffer = new StringBuffer();

    String baseName = _baseName;
    if (baseName != null)
      buffer.append(baseName);

    String contextName = NameUtils.getContextName(context, document);
    if ((contextName != null) && contextName.length() > 0)
    {
      if (baseName != null)
        buffer.append(_NAME_SEPARATOR);

      buffer.append(contextName);
    }
    
    if (_isCompressStyles(null))
    {
      if (baseName != null || contextName != null)
        buffer.append(_NAME_SEPARATOR);
      buffer.append(_COMPRESSED);
    }
    
    buffer.append(_CSS_EXTENSION);

    return buffer.toString();
  }
  
  // Returns the current StyleSheetDocument - used by StyleMapImpl only
  StyleSheetDocument __getStyleSheetDocument()
  {
    return _document;
  }

  // Gets the entry for the specified StyleContext, creating it if
  // necessary. Part of creating an entry is creating the style sheet file itself.
  // And Entry contains the style sheet URI.
  private Entry _getEntry(StyleContext context)
  {
    Hashtable<Key, Entry> cache = null;
    Hashtable<Object, Entry> entryCache = null;
    StyleSheetDocument document = null;
    Map<String, String> shortStyleClassMap = null;
    String[] namespacePrefixes = null;

    boolean checkModified  = context.checkStylesModified();

    // Synchronize while set up the _cache, _entryCache, _document, etc...
    synchronized (this)
    {
      // Before we do anything, set up the NameResolver and
      // InputStreamProvider.   _getEntry() is the single entry point
      // through which all calls into the FileSystemStyleCache flow.
      // So, by initializing the NameResolver/InputStreamProvider
      // here, we know they will always be available to other code.
      _initResolver();

      // If checking for modified files, then check to see if the XSS or CSS
      // document has been modified.  If so, we dump our in-memory style cache.
      if (checkModified && hasSourceDocumentChanged(context))
      {
        _cache = null;
        _entryCache = null;
        _document = null;
        _shortStyleClassMap = null;
        _namespacePrefixes  = null;
      }

      // We get references to our two caches (the "normal" cache,
      // and the cache of shared Entry objects) up front.  We do
      // this because the actual caches could change at any time.
      // (The caches get reallocated if the source document is
      // modified.)  We need to use a consistent set of caches
      // throughout the entire request, to avoid adding bogus entries
      // to a new re-allocated cache.
      // Note: It would probably make sense to use Map for the
      // cache type in our vars and method prototypes.  We explicitly
      // use Hashtable, because our implementation relies on the
      // synchronization provided by Hashtable.  If we change the
      // cache data structure, we might need to re-code to add
      // synchronization.  Thus the somewhat ugly explicit references
      // to Hashtable everywhere.
      if (_cache == null)
        _cache = new Hashtable<Key, Entry>();
      if (_entryCache == null)
        _entryCache = new Hashtable<Object, Entry>(19);

      cache = _cache;
      entryCache = _entryCache;

      // Get the document up front too.
      // Returns the StyleSheetDocument, parsing the source file if necessary
      // this sets up _shortStyleClassMap and _namespacePrefixes
      document = _getStyleSheetDocument(context);
      if (document == null)
        return null;

      shortStyleClassMap = _shortStyleClassMap;
      namespacePrefixes = _namespacePrefixes;
    }

    // Look up the style sheet
    // The Key class is a private static class that is used for hashing. It implements
    // hashCode and equals which are based on locale, direction, browser, version, platform.
    Key key = new Key(context);
    Entry entry = _getEntry(cache, key, checkModified);
    if (entry != null)
      return entry;

    // Next see if this is an entry which is compatible with this request
    entry = _getCompatibleEntry(context, document, cache, key, entryCache, checkModified);

    if (entry != null)
      return entry;

    // If we didn't find an entry in the cache, create a new entry
    // This generates the CSS file.
    return _createEntry(context,
                        document,
                        cache,
                        key,
                        entryCache,
                        shortStyleClassMap,
                        namespacePrefixes,
                        checkModified);
  }

  private Entry _getEntry(
    Map<?, Entry> cache,
    Object        key,
    boolean       checkModified
    )
  {
    Entry entry = cache.get(key);
    if (entry == null)
    {
      return null;
    }
    
    if (checkModified)
    {
      List<String> uris = entry.uris;
      assert uris != null && !uris.isEmpty();

      boolean valid = true;
      List<File> existing = new LinkedList<File>();
      // Make sure the entry's file exists.  If it no longer
      // exists, we remove the entry from the cache
      for (String name : uris)
      {
        File file = new File(_targetPath, name);
        if (file.exists())
        {
          existing.add(file);
        }
        else
        {
          valid = false;
        }
      }
      
      if (!valid)
      {
        _deleteAll(existing);
        
        synchronized (cache)
        {
          if (cache.get(key) == entry)
          {
            cache.remove(key);
          }
        }

        return null;
      }
    }
    
    return entry;
  }

  /**
   * Creates and caches an Entry for the specified StyleContext
   * This generates a style sheet for the specific StyleContext
   * (locale, direction, etc), and puts that style sheet's uri in the Entry.
   * It also caches it in the "normal" cache (the one that is based on the StyleContext),
   * and the entry cache (the one that is based on the StyleSheetNodes)
   */
  private Entry _createEntry(
    StyleContext             context,
    StyleSheetDocument       document,
    Hashtable<Key, Entry>    cache,
    Key                      key,
    Hashtable<Object, Entry> entryCache,
    Map<String, String>      shortStyleClassMap,
    String[]                 namespacePrefixes,
    boolean                  checkModified
    )
  {
    // Next, get the fully resolved styles for this context. This will be
    // those StyleNodes that match the locale, direction, browser, etc -- the
    // info that is in the StyleContext.
    StyleNode[] styles = _getStyleContextResolvedStyles(context, document);
    if (styles == null)
      return null;

    // Generate the style sheet file, if it isn't already generated,
    // and return the uri.
    List<String> uris = _createStyleSheetFiles(context,
                                       document,
                                       styles,
                                       shortStyleClassMap,
                                       namespacePrefixes,
                                                   checkModified);

    _LOG.fine("Finished processing stylesheet {0}", uris);
    
    
    // Next, get the fully resolved icons and skin properties for this context.
    // This will be those Icons and Skin Properties that match the locale, direction,
    // browser, etc -- the info that is in the StyleContext
    ConcurrentMap<String, Icon> icons =
      _getStyleContextResolvedIcons(context, document);
    ConcurrentMap<Object, Object> skinProperties =
      _getStyleContextResolvedSkinProperties(context, document);

    // Create a new entry and cache it in the "normal" cache. The "normal" cache is one
    // where the key is the Key object which is built based on information from the StyleContext,
    // like browser, agent, locale, direction.
    Entry entry = new Entry(uris, new StyleMapImpl(), icons, skinProperties);
    cache.put(key, entry);

    // Also, cache the new entry in the entry cache
    DerivationKey derivationKey = _getDerivationKey(context, document);
    entryCache.put(derivationKey, entry);

    return entry;
  }

  /**
   * Look in the entry cache for a compatible entry.
   * A compatible entry is one with the same DerivationKey, which is essentially the
   * same StyleSheetNodes.
   */
  private Entry _getCompatibleEntry(
    StyleContext             context,
    StyleSheetDocument       document,
    Map<Key, Entry>    cache,
    Key                      key,
    Map<Object, Entry> entryCache,
    boolean                  checkModified
    )
  {
    DerivationKey derivationKey = _getDerivationKey(context, document);
    Entry entry = _getEntry(entryCache, derivationKey, checkModified);
    if (entry != null)
    {
      // If we've got a compatible entry, cache it
      cache.put(key, entry);
    }
    return entry;
  }

  /**
   * Returns the derivation key that would be used for entries
   * based on the provided context
   */
  private DerivationKey _getDerivationKey(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    // Entries with the same style sheet derivation are compatible.
    // Get the style sheet derivation list.
    Iterator<StyleSheetNode> e = document.getStyleSheets(context);
    // -= Simon Lessard =-
    // TODO: Check if synchronization is truly required
    Vector<StyleSheetNode> v = _copyIterator(e);
    StyleSheetNode[] styleSheets;
    if (v == null)
    {
      styleSheets = new StyleSheetNode[0];
    }
    else
    {
      styleSheets= new StyleSheetNode[v.size()];
      v.copyInto(styleSheets);
    }

    // Create a key out of the style sheet derivation list
    return new DerivationKey(context, styleSheets);
  }

  /**
   * Returns the StyleSheetDocument, parsing the source file if necessary
   */
  private StyleSheetDocument _getStyleSheetDocument(StyleContext context)
  {
    StyleSheetDocument document = _document;

    // If we have a StyleSheetDocument already, just return it.
    if (document != null)
      return document;

    // Otherwise, we create the StyleSheetDocument now
    document = createStyleSheetDocument(context);

    // If we weren't able to create the StyleSheetDocument,
    // use a non-null placeholder
    if (document == null)
      document = _EMPTY_DOCUMENT;

    // Save the document
    if (_document == null)
      _document = document;

    // Re-initialize our Array of namespace prefixes that are in the selectors
    // Re-initialize our Map of short style class names
    _namespacePrefixes = _getNamespacePrefixes(context, _document);
    _shortStyleClassMap = _getShortStyleClassMap(context, _document, _namespacePrefixes);

    return document;
  }

  /**
   * Returns an array of fully resolved StyleNodes for the
   * specified StyleContext  and StyleSheetDocument.
   * This will be those StyleNodes that match the locale, direction, browser, etc -- the
   * info that is in the StyleContext.
   */
  private StyleNode[] _getStyleContextResolvedStyles(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    Iterator<StyleNode> e = document.getStyles(context);
    if ((e == null) || !e.hasNext())
    {
      if (_LOG.isWarning())
        _LOG.warning("NO_STYLES_FOUND_CONTEXT", context);
      return null;
    }

    List<StyleNode> v = new ArrayList<StyleNode>();
    while (e.hasNext())
      v.add(e.next());

    return v.toArray(new StyleNode[v.size()]);
  }

  /**
   * Returns a Map of skin property names to values for the specified
   * styleSheetNodes that have been filtered from the StyleContext and StyleSheetDocument.
   */
  private ConcurrentMap<Object, Object> _getStyleContextResolvedSkinProperties(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    Iterator<StyleSheetNode> styleSheetNodes = document.getStyleSheets(context);

    ConcurrentMap<Object, Object> skinProperties = new ConcurrentHashMap<Object, Object>();
    while (styleSheetNodes.hasNext())
    {
      StyleSheetNode styleSheetNode = styleSheetNodes.next();
      Collection<SkinPropertyNode> skinPropertyNodes = styleSheetNode.getSkinProperties();
      
      if (skinPropertyNodes != null)
      {
        for (SkinPropertyNode skinPropertyNode : skinPropertyNodes)
        {
          skinProperties.put(skinPropertyNode.getKey(), skinPropertyNode.getValue());
        }
      }
    }

    return skinProperties;
  }

  /**
   * Returns a Map of icon names to Icons for the specified
   * styleSheetNodes that have been filtered from the StyleContext and StyleSheetDocument.
   */
  private ConcurrentMap<String, Icon> _getStyleContextResolvedIcons(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    Iterator<StyleSheetNode> styleSheetNodes = document.getStyleSheets(context);

    ConcurrentMap<String, Icon> icons = new ConcurrentHashMap<String, Icon>();
    while (styleSheetNodes.hasNext())
    {
      StyleSheetNode styleSheetNode = styleSheetNodes.next();
      Collection<IconNode> iconNodes = styleSheetNode.getIcons();
      
      if (iconNodes != null)
      {
        for (IconNode iconNode : iconNodes)
        {
          icons.put(iconNode.getIconName(), iconNode.getIcon());
        }
      }
    }

    return icons;
  }

  /**
   * Generates the CSS files for the specified context and styles.
   * @return the names of the generated CSS files.
   */
  private List<String> _createStyleSheetFiles(
    StyleContext        context,
    StyleSheetDocument  document,
    StyleNode[]         styles,
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixes,
    boolean             checkModified)
  {
    // Get the current files
    List<File> outputFiles = _getOutputFiles(context, document);

    // If at least one output file exists, check the last modified time.
    if (!outputFiles.isEmpty())
	  {
      if (checkModified)
	  {
        if (!_checkSourceModified(document, outputFiles.get(0)))
        {
          return _getFileNames(outputFiles);
	  }
        // If the output file is older than the source file, we
        // need to regenerate the output file.  But first we
        // need to delete the old output file before we attempt to
        // create a new version.
        _deleteAll(outputFiles);
      }
      else
      {
        return _getFileNames(outputFiles);
      }
    }
    
    // Make sure the output directory exists in case it's been
    // blown away since the creation of the cache
    File outputDir = new File(_targetPath);
    if (!outputDir.exists())
      outputDir.mkdirs();
    
    // Write out the style sheet
    // First figure out whether or not we need to compress the style classes.
    // We don't compress the style classes if the content compression flag is disabled or
    // if the skin is a portlet skin.
    Skin skin = RenderingContext.getCurrentInstance().getSkin();
    boolean compressStyles = _isCompressStyles(skin);

    StyleWriterFactoryImpl writerFactory = new StyleWriterFactoryImpl(_targetPath,
      getTargetStyleSheetName(context, document));
    CSSGenerationUtils.writeCSS(context,
                                skin.getStyleSheetName(),
                                styles,
                                writerFactory,
                                compressStyles,
                                shortStyleClassMap,
                                namespacePrefixes,
                                _STYLE_KEY_MAP
                                );
      
    writerFactory.close();

    // Return the name of the new style sheet
    return _getFileNames(writerFactory.getFiles());
      }
      
  private File _getOutputFile(String name, int number)
      {
    assert number >= 1;
    if (number == 1)
      {
      return new File(_targetPath, name);
      }
    int index = name.lastIndexOf(".");
    if (index < 0)
  {
      return new File(_targetPath, name + number);
  }
    else
  {
      // file name + number + file extension
      return new File(_targetPath,
        new StringBuilder(name.length() + 2).append(name.substring(0, index)).append(number)
          .append(name.substring(index)).toString());
      }
    }
  
  private void _deleteAll(Iterable<File> files)
  {
    for (File file : files)
    {
      if (file.exists())
      {
        file.delete();
      }
    }
  }
  
  /**
   * First figure out whether or not we need to compress the style classes.
   * We don't compress the style classes if the content compression flag is disabled or
   * if the skin is a portlet skin.
   */
  private boolean _isCompressStyles(Skin skin)
  {
    if (skin == null)
      skin = RenderingContext.getCurrentInstance().getSkin();
      

    String disableContentCompression =
      FacesContext.getCurrentInstance().getExternalContext().
      getInitParameter(StyleSheetRenderer.DISABLE_CONTENT_COMPRESSION);
    // we do not compress if it is a portlet skin
    boolean isPortletSkin =
    CoreRenderKit.OUTPUT_MODE_PORTLET.equals(skin.getRenderKitId());

    return (!"true".equals(disableContentCompression)) &&
                                             !isPortletSkin;
  }

  private List<String> _getFileNames(List<File> files)
  {
    List<String> names = new ArrayList<String>(files.size());
    for (File file : files)
    {
      names.add(file.getName());
    }
    return Collections.unmodifiableList(names);
  }

  /**
   * Returns the name of the output files that have been created for the given context and
   * document. If there are no files found, and empty list will be returned.
   * @return The list of list that currently exist, or null if none found
   */
  private List<File> _getOutputFiles(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    // use a linked list as we only iterate and linked lists are faster for iteration & appending
    // than array lists.
    List<File> files = new LinkedList<File>();
    String name = getTargetStyleSheetName(context, document);
    for (int i = 1; true; ++i)
    {
      // we don't know in advance if there are any files, and if there are, how many of them
      // there are. Therefore, we keep incrementing the counter until the file doesn't exist and
      // at that point we know we have all of them
      File f = _getOutputFile(name, i);
      if (f.exists())
      {
        files.add(f);
    }
      else
	  {
        break;
	  }
    }

    return files;
  }

  /**
   * Returns the PrintWriter to use for the specified file
   */
  private PrintWriter _getWriter(File file)
  {
    PrintWriter out = null;

    try
    {
      File parentFile = file.getParentFile();
      if (parentFile != null)
        parentFile.mkdirs();

      FileOutputStream fos = new FileOutputStream(file);
      OutputStreamWriter writer = null;
        
      // Use UTF8 encoding for output, in case font names have non-ascii
      // characters.
      try
      {
        writer = new OutputStreamWriter(fos, _UTF8_ENCODING);
      }
      catch (UnsupportedEncodingException e)
      {
        // UTF-8 should always be supported!
        assert false;

        // Just use default encoding instead
        writer = new OutputStreamWriter(fos);
      }

      out = new PrintWriter(new BufferedWriter(writer));
    }
    catch (IOException e)
    {
      if (_LOG.isWarning())
        _LOG.warning("IOEXCEPTION_OPENNING_FILE", file);
        _LOG.warning(e);
      }
    
    return out;
  }

  /**
   * Checks to see whether the source file has been modified
   * since the specified output file was generated.  If so,
   * we need to regenerate the output file.
   */
  private boolean _checkSourceModified(
    StyleSheetDocument document,
    File               outputFile
    )
  {
    assert (outputFile != null);

    return (document.getDocumentTimestamp() > outputFile.lastModified());
  }

  /**
   * Gets the InputStreamProvider for the source file
   */
  private InputStreamProvider _getInputStreamProvider()
  {
    // Source file may be null
    File sourceFile = _sourceFile;
    if (sourceFile == null)
      return null;

    assert (_resolver != null);

    try
    {
      return _resolver.getProvider(sourceFile.getPath());
    }
    catch (IOException e)
    {
      if (_LOG.isWarning())
        _LOG.warning(e);
    }

    return null;
  }

  /**
   * Initiializes the NameResolver
   */
  private void _initResolver()
  {
    // Synchronize just in case two different threads hit this
    // at the same time
    synchronized (this)
    {
      if (_resolver == null)
      {
        NameResolver resolver = new DefaultNameResolver(_sourceFile, null);

        // We explicitly wrap the NameResolver in a CachingNameResolver
        // since that conveniently handles checking for modifications to
        // dependent (imported) files.
        // The default storage for cached files is a bit large,
        // so we use a smaller hash table.  Also, always enable
        // modification checking.
        // FIXME: Should probably be a ConcurrentHashMap
        resolver = new CachingNameResolver(resolver,
                                           new Hashtable<Object, InputStreamProvider>(17),
                                           true);
      
        _resolver = resolver;
      }
    }
  }

  /**
   * Copies an enumeration into a Vector
   */
  private <T> Vector<T> _copyIterator(Iterator<T> e)
  {
    if (e == null)
      return null;

    Vector<T> v = new Vector<T>();
    while (e.hasNext())
      v.addElement(e.next());

    return v;
  }
  
  /**
   * Create an array of all the namespace prefixes in the xss/css file. E.g., "af|" or "tr|"
   */
  private static String[] _getNamespacePrefixes(
    StyleContext       context,
    StyleSheetDocument document)
  {

    assert (document != null);
    Iterator<StyleSheetNode> styleSheets = document.getStyleSheets(context);
    assert (styleSheets != null);
    Set<String> namespacePrefixesSet = new HashSet<String>();
    while (styleSheets.hasNext())
    {
      StyleSheetNode styleSheet = styleSheets.next();
      Iterable<StyleNode> styles = styleSheet.getStyles();
      assert (styles != null);
      for (StyleNode style : styles)
      {
        String selector = style.getSelector();

        if (selector != null)
        {
          CSSGenerationUtils.getNamespacePrefixes(namespacePrefixesSet, selector);
        }
      }
    }
    return namespacePrefixesSet.toArray(_EMPTY_STRING_ARRAY);
  }

  /**
   * Create the map of full style classes to short style classes
   * Do not shorten styleclasses that start with SkinSelectors.STATE_PREFIX
   */
  private static Map<String, String> _getShortStyleClassMap(
    StyleContext       context,
    StyleSheetDocument document,
    String[]           namespacePrefixes)
  {
    // Use a HashMap to avoid unnecessary synchronization of Hashtable
    Map<String, String> map = new HashMap<String, String>();

    assert (document != null);

    Iterator<StyleSheetNode> styleSheets = document.getStyleSheets(context);
    assert (styleSheets != null);

    Set<String> emptySelectors = new HashSet<String>();
    Set<String> nonEmptySelectors = new HashSet<String>(512);

    while (styleSheets.hasNext())
    {
      StyleSheetNode styleSheet = styleSheets.next();
      Iterable<StyleNode> styles = styleSheet.getStyles();
      assert (styles != null);
      for (StyleNode style : styles)
      {
        String selector = style.getSelector();

        if (selector != null)
        {
          // If we've got a single style class selector, add it
          // to the map. Otherwise, we need to search the selector
          // for style classes.
          if (CSSGenerationUtils.isSingleStyleClassSelector(selector))
          {
            String styleClass = selector.substring(1);
            _putStyleClassInShortMap(styleClass, map);
            // don't shorten styleclasses that are states since they are likely to be added
            // and removed on the client.
            if (styleClass != null && !styleClass.startsWith(SkinSelectors.STATE_PREFIX))
              if (!map.containsKey(styleClass))
                map.put(styleClass, _getShortStyleClass(map.size()));

            if (style.isEmpty())
              emptySelectors.add(styleClass);
            else
              nonEmptySelectors.add(styleClass);
          }
          else
          {
            Iterator<String> styleClasses =
              CSSGenerationUtils.getStyleClasses(selector);

            if (styleClasses != null)
            {
              while (styleClasses.hasNext())
              {
                String styleClass = styleClasses.next();
                _putStyleClassInShortMap(styleClass, map);
                // Don't remove any styleclass that is referred to
                nonEmptySelectors.add(styleClass);
              }
            }

            // now search for the selectors that have namespaces and add those to the map
            int length = namespacePrefixes.length;

            for (int i=0; i < length; i++)
            {
              String nsPrefix = namespacePrefixes[i];
              Iterator<String> afSelectors =
                CSSGenerationUtils.getNamespacedSelectors(selector,
                                                          nsPrefix,
                                                          _STYLE_KEY_MAP);
              if (afSelectors != null)
              {
                boolean isFirst = true;
                while (afSelectors.hasNext())
                {
                  String styleClass = afSelectors.next();
                  _putStyleClassInShortMap(styleClass, map);
                  if (isFirst && !afSelectors.hasNext() && style.isEmpty())
                  {
                    emptySelectors.add(styleClass);
                  }
                  else
                  {
                    nonEmptySelectors.add(styleClass);
                  }

                  isFirst = false;
                }
              }
            }
          }
        }
      }
    }

    emptySelectors.removeAll(nonEmptySelectors);

    // Replace all empty keys with an empty string as the selector
    for (String emptyKey : emptySelectors)
      map.put(emptyKey, CoreRenderingContext.EMPTY_STYLE_CLASS);

    // We actually need a Map, since this object is exposed
    // via public APIs.  Also, we need the Map to be immutable,
    // or else we would need to create a new copy of the Map
    // each time it is requested.
    return Collections.unmodifiableMap(map);
  }
  
  /**
   * Method to put styleclasses in the shortened map.
   * We don't put 'state' styleclasses in the shortened map. Those are styleclasses
   * that start with SkinSelectors.STATE_PREFIX. The reason is that those
   * are likely to be added and removed on the client as the state changes, and
   * we don't want to require the shortened map on the client.
   */
  private static void _putStyleClassInShortMap(String styleClass, Map map)
  {
    if (styleClass != null &&
        !styleClass.startsWith(SkinSelectors.STATE_PREFIX) &&
        !map.containsKey(styleClass))
    {
      map.put(styleClass, _getShortStyleClass(map.size()));
    }
  }

  /**
   * Helper method used by _getShortStyleClassMap().  Returns a new
   * short style class selector.  The count is the number of style
   * classes seen so far.
   */
  private static String _getShortStyleClass(int count)
  {
    // At the moment the short style class is just based on the nubmer
    // of style classes and not the style class itself.
    return _SHORT_CLASS_PREFIX + Integer.toString(count, Character.MAX_RADIX);
  }
  

  /**
   * Key class used for hashing style sheet URIs
   */
  private static class Key
  {
    public Key(StyleContext context)
    {
      TrinidadAgent agent = context.getAgent();
      LocaleContext localeContext = context.getLocaleContext();
      AccessibilityProfile accProfile = context.getAccessibilityProfile();

      _init(
       localeContext.getTranslationLocale(),
       LocaleUtils.getReadingDirection(localeContext),
       agent.getAgentApplication(),
       agent.getAgentVersion(),
       agent.getAgentOS(),
       true,
       accProfile);
    }

    @Override
    public int hashCode()
    {
      int shortHash = (_short ? 1 : 0);

      return (_locale.hashCode()           ^
               (_direction)                ^
               (_browser  << 2)            ^
               (_version.hashCode())       ^
               (_platform << 8)            ^
               shortHash                   ^
               _accProfile.hashCode());
    }

    @Override
    public boolean equals(Object o)
    {
      if (this == o)
        return true;

      if (o instanceof Key)
      {
        Key key = (Key)o;

        // Check the easy stuff first
        if ((key._short != _short)           ||
            (_direction != key._direction)   ||
            (_browser != key._browser)       ||
            (!_version.equals(key._version)) ||
            (_platform != key._platform)     ||
            !_locale.equals(key._locale)     ||
            !_accProfile.equals(key._accProfile))
        {
          return false;
        }
      }

      return true;
    }

    private void _init(
      Locale locale,
      int direction,
      int browser,
      String version,
      int platform,
      boolean useShort,
      AccessibilityProfile accessibilityProfile
      )
    {
      // Make sure direction is non-null
      _locale = (locale == null) ? Locale.getDefault() : locale;

      // Make sure direction is not default
      _direction =
        (direction == LocaleUtils.DIRECTION_DEFAULT) ?
          LocaleUtils.getReadingDirectionForLocale(_locale) :
          direction;

      _browser = browser;
      _version = version;
      _platform = platform;
      _short = useShort;
      _accProfile = accessibilityProfile;
    }

    private Locale         _locale;
    private int            _direction;
    private int            _browser;
    private String         _version;
    private int            _platform;
    private boolean        _short;  // Do we use short style classes?
    private AccessibilityProfile _accProfile;
  }

  /**
   * Cache entry class
   */
  private static class Entry
  {
    public final List<String> uris;
    public final StyleMap map;
    public final ConcurrentMap<String, Icon> icons;
    public final ConcurrentMap<Object, Object> skinProperties;

    public Entry(
      List<String> uris,
      StyleMap map,
      ConcurrentMap<String, Icon> icons,
      ConcurrentMap<Object, Object> skinProperties)
    {
      this.uris = uris;
      this.map = map;
      this.icons = icons;
      this.skinProperties = skinProperties;
    }
  }

  /**
   * A key object which is used to hash Entrys in the entry cache.  The key for the entry
   * cache is the style sheet derivation list - that is a list of StyleSheetNodes, sorted
   * by specficity.
   */
  private static class DerivationKey
  {
    public DerivationKey(StyleContext context, StyleSheetNode[] styleSheets)
    {
      _styleSheets = new StyleSheetNode[styleSheets.length];
      System.arraycopy(styleSheets, 0, _styleSheets, 0, styleSheets.length);
      _short = true;
    }

    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if (!(o instanceof DerivationKey))
        return false;

      DerivationKey key = (DerivationKey)o;

      if ((_short != key._short) ||
          (_styleSheets.length != key._styleSheets.length))
        return false;

      // Test each StyleSheetNode for equality.  We start
      // at the end of the list, as the last style sheet
      // is more likely to be different.  (The first entry
      // is probably the common base style sheet.)
      for (int i = _styleSheets.length - 1; i >= 0; i--)
      {
        if (!_styleSheets[i].equals(key._styleSheets[i]))
          return false;
      }

      return true;
    }

    @Override
    public int hashCode()
    {
      int hashCode = 0;

      for (int i = 0; i < _styleSheets.length; i++)
        hashCode ^= _styleSheets[i].hashCode();

      int shortHash = (_short ? 1 : 0);

      return hashCode ^ shortHash;
    }

    private StyleSheetNode[] _styleSheets;
    private boolean          _short;   // Do we use short style classes?
  }

  /**
   * A StyleMap implemenation which creates Style objects as needed
   */
  private class StyleMapImpl implements StyleMap
  {
    // Implementation of StyleMap.getStyleBySelector()
    public Style getStyleBySelector(
      StyleContext context,
      String       selector
      )
    {
      if (_selectorMap == null)
        _selectorMap = _createMap();

      return _getStyle(context, _selectorMap, selector, "", false);
    }

    // Implementation of StyleMap.getStyleByClass()
    public Style getStyleByClass(
      StyleContext context,
      String       styleClass
      )
    {
      if (_classMap == null)
        _classMap = _createMap();
      String prefix = (styleClass.indexOf('|') > -1) ? "" : ".";
      return _getStyle(context, _classMap, styleClass, prefix , false);
    }

    // Implementation of StyleMap.getStyleByClass()
    public Style getStyleByName(
      StyleContext context,
      String       name
      )
    {
      if (_nameMap == null)
        _nameMap = _createMap();

      return _getStyle(context, _nameMap, name, "", true);
    }

    // Do all of the real work
    private Style _getStyle(
      StyleContext       context,
      Map<String, Style> map,
      String             id,
      String             prefix,
      boolean            isName
      )
    {
      CSSStyle style = (CSSStyle)map.get(id);

      if (style == _MISS)
        return null;
      if (style != null)
        return style;

      // Next, try getting the Style from the StyleSheetDocument
      StyleSheetDocument document = __getStyleSheetDocument();
      if (document == null)
        return null;

      StyleNode styleNode = null;

      if (isName)
      {
        styleNode = document.getStyleByName(context, id);
      }
      else
      {
        styleNode = document.getStyleBySelector(context, prefix + id);
      }

      if (styleNode == null)
      {
        map.put(id, _MISS);
        return null;
      }

      // Convert the styleNode into a Style
      style = new CSSStyle();

      // Add in the properties for the style
      Iterable<PropertyNode> propertyNodeList = styleNode.getProperties();
      for (PropertyNode property : propertyNodeList)
      {
        String name = property.getName();
        String value = property.getValue();

        style.setProperty(name, value);
      }

      map.put(id, style);
      return style;
    }

    // Creates a map of the specified size
    private Hashtable<String, Style> _createMap()
    {
      return new Hashtable<String, Style>(19);
    }


    // Our local Style maps
    // -= Simon Lessard =-
    // TODO: Check if synchronization is truly needed
    private Hashtable<String, Style> _selectorMap;
    private Hashtable<String, Style> _classMap;
    private Hashtable<String, Style> _nameMap;
  }

  private class StyleWriterFactoryImpl
    implements StyleWriterFactory
  {
    private String _outputDirectory;
    private String _baseFilename;
    private PrintWriter _out;
    private List<File> _files = new LinkedList<File>();

    StyleWriterFactoryImpl(String outputDirectory, String baseName)
    {
      _outputDirectory = outputDirectory;
      _baseFilename = baseName;
    }

    List<File> getFiles()
    {
      return _files;
    }

    public PrintWriter createWriter()
    {
      if (_out != null)
      {
        _out.close();
      }

      File outputFile = _getOutputFile(_baseFilename, _files.size() + 1);
      // We never want to do anything other then read it or delete it:
      outputFile.setReadOnly();

      _files.add(outputFile);
      _out = _getWriter(outputFile);

      return _out;
    }

    void close()
    {
      if (_out != null)
      {
        _out.close();
        _out = null;
      }
    }
  }

  private File   _sourceFile; // The source XSS file
  private String _targetPath; // The location of the cache
  private String _baseName;   // The base file name for generated CSS files

  /**
   * The NameResolver and InputStreamProvider we use to
   * resolve/load all files.  We also use the InputStreamProvider
   * to check for modifications to any dependent files.
   */
  private NameResolver        _resolver;

  /** The parsed StyleSheetDocument */
  private StyleSheetDocument _document;

  /** The cache of style sheet URIs */
  private Hashtable<Key, Entry> _cache;

  /**
   * We cache Entry objects, hashed by DerivationKey (ie.
   * hashed based on the StyleSheetNode derivation list).
   */
  private Hashtable<Object, Entry> _entryCache;

  /** Map which maps from full style class names to our compressed names. */
  private Map<String, String> _shortStyleClassMap;
  private String[]            _namespacePrefixes;

  // Constants

  // Separator for variants in file names
  private static final char _NAME_SEPARATOR = '-';
  private static final String _COMPRESSED = "cmp";

  /** Extension for CSS files */
  private static final String _CSS_EXTENSION = ".css";

  /** Java name for UTF8 encoding */
  private static String _UTF8_ENCODING = "UTF8";

  /** Stub StyleSheetDocument instance */
  private static final StyleSheetDocument _EMPTY_DOCUMENT =
    new StyleSheetDocument(null, null, StyleSheetDocument.UNKNOWN_TIMESTAMP);

  /**
   * Style used to represent misses in the StyleMap.
   * Package private to allow access from nested StyleMapImpl class
   */
          static final Style _MISS = new CSSStyle();

  /** Prefix to use for short style classes */
  private static final String _SHORT_CLASS_PREFIX = "x";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileSystemStyleCache.class);


  /**
   * use this map to map from the public style selector names to
   * our internal style selector names. The public style selector
   * names do not contain html, whereas our internal style selector
   * names may. We write out the shortened version of the mapped
   * selector names to the css file.
   * jmw.
   * @todo Need to find a better spot for this, like the skin?
   */
  private static final Map<String, String> _STYLE_KEY_MAP;

  static
  {
    _STYLE_KEY_MAP =  new HashMap<String, String>();
    // we don't use a styleClass on tr:body. Instead we use the html element
    // BODY to style it. This makes it easier for users to use an external
    // stylesheet and not have to know our styleClass names.
    _STYLE_KEY_MAP.put("af|body", "BODY");

    _STYLE_KEY_MAP.put("af|panelHeader::level-one",
                 "H1.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-two",
                 "H2.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-three",
                 "H3.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-four",
                 "H4.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-five",
                 "H5.af|panelHeader");
    _STYLE_KEY_MAP.put("af|panelHeader::level-six",
                 "H6.af|panelHeader");

    // showDetailHeader
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-one",
                 "H1.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-two",
                 "H2.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-three",
                 "H3.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-four",
                 "H4.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-five",
                 "H5.af|showDetailHeader");
    _STYLE_KEY_MAP.put("af|showDetailHeader::level-six",
                 "H6.af|showDetailHeader");

    _STYLE_KEY_MAP.put("af|menuTabs::selected-link",
                 "af|menuTabs::selected A");
    _STYLE_KEY_MAP.put("af|menuTabs::enabled-link",
                 "af|menuTabs::enabled A");

    _STYLE_KEY_MAP.put("af|menuBar::enabled-link",
                 "af|menuBar::enabled A");
    _STYLE_KEY_MAP.put("af|menuBar::selected-link",
                 "af|menuBar::selected A");

    _STYLE_KEY_MAP.put("OraLinkEnabledLink",
                 "OraLinkEnabled A:link");
    _STYLE_KEY_MAP.put("OraLinkSelectedLink",
                 "OraLinkSelected A:link");
    _STYLE_KEY_MAP.put("OraLinkEnabledActive",
                 "OraLinkEnabled A:active");
    _STYLE_KEY_MAP.put("OraLinkSelectedActive",
                 "OraLinkSelected A:active");
    _STYLE_KEY_MAP.put("OraLinkEnabledVisited",
                 "OraLinkEnabled A:visited");
    _STYLE_KEY_MAP.put("OraLinkSelectedVisited",
                 "OraLinkSelected A:visited");

    _STYLE_KEY_MAP.put("af|panelPage::about-link",
                 "af|panelPage::about A");
    _STYLE_KEY_MAP.put("af|panelPage::copyright-link",
                 "af|panelPage::copyright A");
    _STYLE_KEY_MAP.put("af|panelPage::privacy-link",
                 "af|panelPage::privacy A");

    _STYLE_KEY_MAP.put("af|panelList::link",
                 "af|panelList A");
    _STYLE_KEY_MAP.put("af|panelList::unordered-list",
                 "af|panelList UL");

    _STYLE_KEY_MAP.put("af|inputDate::nav-link",
                 "af|inputDate::nav A");
    _STYLE_KEY_MAP.put("af|inputDate::content-link",
                 "af|inputDate::content A");
    _STYLE_KEY_MAP.put("af|inputDate::disabled-link",
                 "af|inputDate::disabled A");
    _STYLE_KEY_MAP.put("af|chooseDate::nav-link",
                 "af|chooseDate::nav A");
    _STYLE_KEY_MAP.put("af|chooseDate::content-link",
                 "af|chooseDate::content A");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWMANYACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|showManyAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWMANYACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|showManyAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWONEACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|showOneAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_SHOWONEACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|showOneAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_PANELACCORDION_TITLE_LINK_STYLE_CLASS,
        "A.af|panelAccordion::title-link");
    _STYLE_KEY_MAP.put(
        SkinSelectors.AF_PANELACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS,
        "A.af|panelAccordion::title-disabled-link");

    _STYLE_KEY_MAP.put("af|panelTabbed::tab-link",
        "af|panelTabbed::tab A");
    _STYLE_KEY_MAP.put("af|panelTabbed::tab-selected-link",
        "af|panelTabbed::tab-selected A");

  }

  private static final String[] _EMPTY_STRING_ARRAY = new String[0];

}
