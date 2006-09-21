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

package org.apache.myfaces.trinidadinternal.style.cache;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.share.io.CachingNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.InputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.nls.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.xml.JaxpXMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.Style;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleMap;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.UserStyleSheet;
import org.apache.myfaces.trinidadinternal.style.util.CSSGenerationUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.xml.sax.SAXException;

/**
 * The FileSystemStyleCache is a StyleProvider implementation which
 * caches generated CSS style sheets on the file system.
 * FileSystemStyleCache instances are shared across applications
 * running in the same VM.  getSharedCache() can be used to obtain
 * a shared FileSystemStyleCache instance for a particular source
 * XSS document.
 *
 * @see org.apache.myfaces.trinidadinternal.style.StyleProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/cache/FileSystemStyleCache.java#0 $) $Date: 10-nov-2005.18:58:54 $
 * @author The Oracle ADF Faces Team
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
   * Returns a shared ImageProvider instance for the specified
   * XSS document and target cache directory.
   *
   * @param source The path of the source XSS document.  The
   *   specified file must be a valid XSS document.  If the specified
   *   file does not exists, an IllegalArgumentException is thrown.
   * @param target The path of the target directory.  Generated
   *   CSS files are stored in this directory.  If the directory
   *   does not exist and can not be created, an IllegalArgumentException
   *   is thrown.
   */
  public static StyleProvider getSharedCache(
    String source,
    String target
    )
  {
    // Make sure we have some source/target.
    if (source == null)
      throw new IllegalArgumentException("No source specified.");
    if (target == null)
      throw new IllegalArgumentException("No target specified.");

    // First, get the key to use to look up the cache
    String key = _getSharedCacheKey(source, target);

    // See if we've got a shared cache
    StyleProvider cache = _sSharedCaches.get(key);

    // If we didn't find a shared cache, create a new cache
    // and cache it in the shared cache cache.  :-)
    if (cache == null)
    {
      // Create the new cache
      cache = new FileSystemStyleCache(source, target);

      // Before we save the new cache, make sure another thread hasn't
      // already cached a different instance.  Synchronize to lock up
      // _sSharedCaches.
      synchronized (_sSharedCaches)
      {
        StyleProvider tmp = _sSharedCaches.get(key);
        if (tmp != null)
        {
          // Stick with tmp
          cache = tmp;
        }
        else
        {
          _sSharedCaches.put(key, cache);
        }
      }
    }

    return cache;
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
  public String getStyleSheetURI(StyleContext context)
  {

    Entry entry = _getEntry(context);

    if (entry == null)
      return null;

    return entry.uri;
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
        _LOG.warning("IOException during parse of " + _sourceFile, e);
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

      // If checking for modified files, then check to see if the XSS
      // doc has been modified.  If so, we dump our in-memory style cache.
      if (checkModified && hasSourceDocumentChanged(context))
      {
        _cache = null;
        _entryCache = null;
        _document = null;
        _shortStyleClassMap = null;
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
      document = _getStyleSheetDocument(context);
      if (document == null)
        return null;

      shortStyleClassMap = _shortStyleClassMap;
    }

    // Look up the style sheet
    Key key = new Key(context);
    Entry entry = _getEntry(cache, key, checkModified);
    if (entry != null)
      return entry;

    // Next see if we an entry which is compatible with this request
    entry = _getCompatibleEntry( context,
                                 document,
                                 cache,
                                 key,
                                 entryCache,
                                 checkModified);
    if (entry != null)
      return entry;

    // If we didn't find an entry in the cache, create a new entry
    return _createEntry(context,
                        document,
                        cache,
                        key,
                        entryCache,
                        shortStyleClassMap,
                        checkModified);
  }

  // Returns the cache entry with the specified key
  private Entry _getEntry(
    Hashtable<Key, Entry> cache,
    Key                   key,
    boolean               checkModified)
  {
    Entry entry = cache.get(key);
    if (entry == null)
      return null;

    // Make sure the entry's file exists.  If it no longer
    // exists, we remove the entry from the cache
    if (checkModified &&
         (entry.uri != null) &&
         !(new File(_targetPath, entry.uri).exists()))
    {
      synchronized (cache)
      {
        if (cache.get(key) == entry)
          cache.remove(key);
      }

      return null;
    }

    return entry;
  }

  // Creates and caches an Entry for the specified StyleContext
  // This generates a style sheet for the specific StyleContext
  // (locale, direction, etc), and puts that style sheet's uri in the Entry.
  private Entry _createEntry(
    StyleContext             context,
    StyleSheetDocument       document,
    Hashtable<Key, Entry>    cache,
    Key                      key,
    Hashtable<Object, Entry> entryCache,
    Map<String, String>      shortStyleClassMap,
    boolean                  checkModified
    )
  {
    // Next, get the fully resolved styles for this context. This will be
    // those StyleNodes that match the locale, direction, browser, etc -- the
    // info that is in the StyleContext.
    StyleNode[] styles = _getStyles(context, document);
    if (styles == null)
      return null;

    // Generate the style sheet file, if it isn't already generated,
    // and return the uri.
    String uri = _createStyleSheetFile(context,
                                       document,
                                       styles,
                                       shortStyleClassMap,
                                       checkModified);

    _LOG.fine("Finished processing stylesheet {0}", uri);

    // Create a new entry and cache it
    Entry entry = new Entry(uri, new StyleMapImpl());
    cache.put(key, entry);

    // Also, cache the new entry in the entry cache
    DerivationKey derivationKey = _getDerivationKey(context, document);
    entryCache.put(derivationKey, entry);

    return entry;
  }

  // Look in the entry cache for a compatible entry.
  private Entry _getCompatibleEntry(
    StyleContext             context,
    StyleSheetDocument       document,
    Hashtable<Key, Entry>    cache,
    Key                      key,
    Hashtable<Object, Entry> entryCache,
    boolean                  checkModified
    )
  {
    DerivationKey derivationKey = _getDerivationKey(context, document);
    Entry entry = entryCache.get(derivationKey);
    if (entry == null)
      return null;

    // Make sure the entry's file exists.  If the file no
    // longer exists, we remove the entry from the entry cache.
    if (checkModified &&
         (entry.uri != null) &&
         !(new File(_targetPath, entry.uri).exists()))
    {
      synchronized (this)
      {
        if (entryCache.get(derivationKey) == entry)
          entryCache.remove(derivationKey);
      }

      return null;
    }

    // If we've got a compatible entry, cache it
    cache.put(key, entry);

    return entry;
  }

  // Returns the derivation key that would be used for entries
  // based on the provided context
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

  // Returns the StyleSheetDocument, parsing the source file if necessary
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

    // Re-initialize our Map of short style class names
    _shortStyleClassMap = _getShortStyleClassMap(context, _document);

    return document;
  }

  // Returns an array of fully resolved StyleNodes for the
  // specified context and document.
  private StyleNode[] _getStyles(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    Iterator<StyleNode> e = document.getStyles(context);
    if (e == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("No styles found context - " + context);
      return null;
    }

    // -= Simon Lessard =- 
    // TODO: Check if synchronization is truly required
    Vector<StyleNode> v = new Vector<StyleNode>();
    while (e.hasNext())
      v.addElement(e.next());

    StyleNode[] styles = new StyleNode[v.size()];
    v.copyInto(styles);

    return styles;
  }

  // Generates the CSS file for the specified context and styles.
  // Returns the name of the generated CSS file.
  private String        _createStyleSheetFile(
    StyleContext        context,
    StyleSheetDocument  document,
    StyleNode[]         styles,
    Map<String, String> shortStyleClassMap,
    boolean             checkModified)
  {
    // Get a name for the new style sheet
    File outputFile = _getOutputFile(context, document);
    String name = outputFile.getName();

    // If the output file already exists, check the last modified time.
    // This isn't really
    if (outputFile.exists())
    {
      if (checkModified)
      {
        if (!_checkSourceModified(document, outputFile))
          return name;

        // If the output file is older than the source file, we
        // need to regenerate the output file.  But first we
        // need to delete the old output file before we attempt to
        // create a new version.
        outputFile.delete();
      }
      else
        return name;
    }

    // Now, try to create the file
    boolean created = false;

    try
    {
      // If we can't create the new file, bail
      created = outputFile.createNewFile();
    }
    catch (IOException e)
    {
      if (_LOG.isWarning())
        _LOG.warning("IOException while creating file: " + outputFile, e);
    }

    if (!created)
    {
      if (_LOG.isWarning())
        _LOG.warning("\nUnable to generate the style sheet "
                     + outputFile.getName() + " in cache directory\n"
                     + outputFile.getParent() + ".\n"
                     + "Please make sure that the cache directory exists "
                     + "and is writable.\n" );
      return null;
    }

    // Get the Writer to output to
    PrintWriter out = _getWriter(outputFile);
    if (out == null)
      return null;

    // Write out the style sheet
    CSSGenerationUtils.writeCSS(context,
                                styles,
                                out,
                                outputFile,
                                shortStyleClassMap,
                                AF_STYLE_NAMESPACE,
                                _STYLE_KEY_MAP
                                );

    out.close();

    // Return the name of the new style sheet
    return name;
  }

  // Returns the name of the output file to use for the
  // style sheet associated with the specified context
  private File _getOutputFile(
    StyleContext context,
    StyleSheetDocument document
    )
  {
    String name = getTargetStyleSheetName(context, document);

    return new File(_targetPath, name);
  }

  // Returns the PrintWriter to use for the specified file
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
        _LOG.warning("IOException while opening file for writing: " + file, e);
    }

    return out;
  }

  // Checks to see whether the source file has been modified
  // since the specified output file was generated.  If so,
  // we need to regenerate the output file.
  private boolean _checkSourceModified(
    StyleSheetDocument document,
    File               outputFile
    )
  {
    assert (outputFile != null);

    return (document.getDocumentTimestamp() > outputFile.lastModified());
  }

  // Gets the InputStreamProvider for the source file
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

  // Initiializes the NameResolver
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
        if (!(resolver instanceof CachingNameResolver))
        {
          // The default storage for cached files is a bit large,
          // so we use a smaller hash table.  Also, always enable
          // modification checking.
          resolver = new CachingNameResolver(resolver,
                                             new Hashtable<Object, InputStreamProvider>(17),
                                             true);
        }

        _resolver = resolver;
      }
    }
  }

  // Copies an enumeration into a Vector
  private <T> Vector<T> _copyIterator(Iterator<T> e)
  {
    if (e == null)
      return null;

    Vector<T> v = new Vector<T>();
    while (e.hasNext())
      v.addElement(e.next());

    return v;
  }

  // Return a key which we can use to look up the cache with
  // the specifeid properties
  private static String _getSharedCacheKey(String source, String target)
  {
    // Make sure we used canonical paths when looking up the cache.
    // Otherwise, slight difference in how the paths are specified might
    // cause us to create multiple FileSystemStyleCache instances even
    // when referring to the same canonical directory.
    source = _getCanonicalPath(source);
    target = _getCanonicalPath(target);

    // Just combine the source and path to form the key
    return source + target;
  }

  // Create the map of full style classes to short style classes
  private static Map<String, String> _getShortStyleClassMap(
    StyleContext       context,
    StyleSheetDocument document)
  {
    // Use a HashMap to avoid unnecessary synchronization of Hashtable
    Map<String, String> map = new HashMap<String, String>();

    assert (document != null);

    Iterator<StyleSheetNode> styleSheets = document.getStyleSheets(context);
    assert (styleSheets != null);

    while (styleSheets.hasNext())
    {
      StyleSheetNode styleSheet = styleSheets.next();
      Iterator<StyleNode> styles = styleSheet.getStyles();
      assert (styles != null);
      while (styles.hasNext())
      {
        StyleNode style = styles.next();
        String selector = style.getSelector();

        if (selector != null)
        {


          // If we've got a single style class selector, add it
          // to the map. Otherwise, we need to search the selector
          // for style classes.
          if (CSSGenerationUtils.isSingleStyleClassSelector(selector))
          {
            String styleClass = selector.substring(1);
            if (!map.containsKey(styleClass))
              map.put(styleClass, _getShortStyleClass(map.size()));
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

                if (!map.containsKey(styleClass))
                  map.put(styleClass, _getShortStyleClass(map.size()));
              }
            }
          }

          // now go through the selectors and get the list of af| selectors and
          // put those into the map
          Iterator<String> afSelectors =
            CSSGenerationUtils.getNamespacedSelectors(selector,
                                                      AF_STYLE_NAMESPACE,
                                                      _STYLE_KEY_MAP);
          if (afSelectors != null)
          {
            while (afSelectors.hasNext())
            {
              String styleClass = afSelectors.next();

              if (!map.containsKey(styleClass))
                map.put(styleClass, _getShortStyleClass(map.size()));
            }
          }
        }
      }
    }

    // We actually need a Map, since this object is exposed
    // via public APIs.  Also, we need the Map to be immutable,
    // or else we would need to create a new copy of the Map
    // each time it is requested.
    // =-ags We could just clone the Map and wrap it in a Map
    //       if we want to allow clients to modify the Map.
    return new ImmutableMapAdapter<String, String>(map);
  }

  // Helper method used by _getShortStyleClassMap().  Returns a new
  // short style class selector.  The count is the number of style
  // classes seen so far.
  private static String _getShortStyleClass(int count)
  {
    // At the moment the short style class is just based on the nubmer
    // of style classes and not the style class itself.
    return _SHORT_CLASS_PREFIX + Integer.toString(count, Character.MAX_RADIX);
  }



  // Utility method for getting canonical paths.  File.getCanonicalPath()
  // can be slow, so we cache canonical paths to avoid calling
  // getCanonicalPath() each time we need to get a shared FileSystemStyleCache
  // instance.
  // =-=ags This code is copied from FileSystemImageCache.  Might want to
  //        add this as a generic utility method somewhere, but where?
  private static String _getCanonicalPath(String path)
  {
    String canonicalPath = _sCanonicalPaths.get(path);
    if (canonicalPath != null)
      return canonicalPath;

    File file = new File(path);

    try
    {
      canonicalPath =  file.getCanonicalPath();
    }
    catch (IOException e)
    {
      throw new IllegalArgumentException("Could not get directory path: " +
                                         path);
    }

    if (canonicalPath != null)
      _sCanonicalPaths.put(path, canonicalPath);

    return canonicalPath;
  }

  // Key class used for hashing style sheet URIs
  private static class Key
  {
    public Key(StyleContext context)
    {
      TrinidadAgent agent = context.getAgent();

      LocaleContext localeContext = context.getLocaleContext();

      UserStyleSheet styleSheet = UserStyleSheet.getUserStyleSheet(context);

      _init(
       localeContext.getTranslationLocale(),
       localeContext.getReadingDirection(),
       agent.getAgentApplication(),
       agent.getAgentMajorVersion(),
       agent.getAgentOS(),
       styleSheet,
       true);
    }

    @Override
    public int hashCode()
    {
      // We treat the UserStyleSheet specially.  We want to have one and
      // only one entry for a particular user style sheet ID.  So, we avoid
      // hashing directly on the UserStyleSheet object.  That way, if the
      // styles for a particular UserStyleSheet ID changes, we can still find
      // the same old entry in the FileSystemStyleCache, and re-generate the
      // CSS/StyleMap as necessary.
      int userStyleSheetHashCode = 0;
      if (_userStyleSheet != null)
        userStyleSheetHashCode = _userStyleSheet.getID().hashCode();

      int shortHash = (_short ? 1 : 0);

      return (_locale.hashCode()           ^
               (_direction)                ^
               (_browser  << 2)            ^
               (_version  << 4)            ^
               (_platform << 8)            ^
               shortHash                   ^
               userStyleSheetHashCode);
    }

    @Override
    public boolean equals(Object o)
    {
      // As documented in our hashCode() implementation, we treat
      // UserStyleSheets specially.  Two keys are equals if they have the
      // UserStyleSheet ID.  Later on we can actually check to see if the
      // styles have changed, but we still want to use the same Key even if
      // that is the case.
      if (this == o)
        return true;

      if (o instanceof Key)
      {
        Key key = (Key)o;

        // Check the easy stuff first
        if ((key._short != _short)         ||
            (_direction != key._direction) ||
            (_browser != key._browser)     ||
            (_version != key._version)     ||
            (_platform != key._platform)   ||
            !_locale.equals(key._locale))
        {
          return false;
        }

        // Check for matching UserStyleSheets IDs
        if (_userStyleSheet == null)
        {
          if (key._userStyleSheet != null)
            return false;
        }
        else if ((key._userStyleSheet == null) ||
                 !_userStyleSheet.getID().equals(key._userStyleSheet.getID()))
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
      int version,
      int platform,
      UserStyleSheet userStyleSheet,
      boolean useShort
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
      _userStyleSheet = userStyleSheet;
      _short = useShort;
    }

    private Locale         _locale;
    private int            _direction;
    private int            _browser;
    private int            _version;
    private int            _platform;
    private UserStyleSheet _userStyleSheet;
    private boolean        _short;  // Do we use short style classes?
  }

  // Cache entry class
  private static class Entry
  {
    public final String uri;
    public final StyleMap map;

    public Entry(String uri, StyleMap map)
    {
      this.uri = uri;
      this.map = map;
    }
  }

  // A key object which is used to hash Entrys in the
  // entry cache.  The key for the entry cache is the
  // style sheet derivation list - that is a list of
  // StyleSheetNodes, sorted by specficity.
  private static class DerivationKey
  {
    public DerivationKey(StyleContext context, StyleSheetNode[] styleSheets)
    {
      _userStyleSheet = UserStyleSheet.getUserStyleSheet(context);
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

      // Check for matching UserStyleSheets IDs
      if (_userStyleSheet == null)
      {
        if (key._userStyleSheet != null)
          return false;
      }
      else if ((key._userStyleSheet == null) ||
               !_userStyleSheet.getID().equals(key._userStyleSheet.getID()))
      {
        return false;
      }

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

      // Only hash on the UserStyleSheet ID.  We don't hash on the styles as
      // any two UserStyleSheets with the same ID should use the same key.
      if (_userStyleSheet != null)
        hashCode ^= _userStyleSheet.getID().hashCode();

      for (int i = 0; i < _styleSheets.length; i++)
        hashCode ^= _styleSheets[i].hashCode();

      int shortHash = (_short ? 1 : 0);

      return hashCode ^ shortHash;
    }

    private StyleSheetNode[] _styleSheets;
    private UserStyleSheet   _userStyleSheet;
    private boolean          _short;   // Do we use short style classes?
  }

  // A StyleMap implemenation which creates Style objects as needed
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
      String prefix = (styleClass.startsWith(AF_STYLE_NAMESPACE)) ? "" : ".";
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
      Iterator<PropertyNode> e = styleNode.getProperties();
      while (e.hasNext())
      {
        PropertyNode property = e.next();
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

  // Wraps a Map in an immutable Map
  // -= Simon Lessard =-
  // FIXME: WAHHHH! This is BAD... extending HashMap creates a
  //        big array that will never be used... Furthermore,
  //        Collections.unmodifiableMap does just that!!!
  private static class ImmutableMapAdapter<K, V> extends HashMap<K, V>
  {
    public ImmutableMapAdapter(Map<K, V> map)
    {
      _map = map;
    }

    @Override
    public int size()
    {
      return _map.size();
    }

    @Override
    public boolean isEmpty()
    {
      return _map.isEmpty();
    }

    public Iterator<K> keys()
    {
      Set<K> keys = _map.keySet();
      if (keys == null)
        return null;

      return keys.iterator();
    }

    public Iterator<V> elements()
    {
      Collection<V> values = _map.values();
      if (values == null)
        return null;

      return values.iterator();
    }

    @Override
    public V get(Object key)
    {
      return _map.get(key);
    }

    @Override
    public V put(K key, V value)
    {
      throw new IllegalArgumentException();
    }

    @Override
    public V remove(Object key)
    {
      throw new IllegalArgumentException();
    }

    private final Map<K, V> _map;
  }


  private File   _sourceFile; // The source XSS file
  private String _targetPath; // The location of the cache
  private String _baseName;   // The base file name for generated CSS files

  // The NameResolver and InputStreamProvider we use to
  // resolve/load all files.  We also use the InputStreamProvider
  // to check for modifications to any dependent files.
  private NameResolver        _resolver;

  // The parsed StyleSheetDocument
  private StyleSheetDocument _document;

  // The cache of style sheet URIs
  private Hashtable<Key, Entry> _cache;

  // We cache Entry objects, hashed by DerivationKey (ie.
  // hashed based on the StyleSheetNode derivation list).
  private Hashtable<Object, Entry> _entryCache;

  // Map which maps from full style class names to
  // our compressed names.
  private Map<String, String> _shortStyleClassMap;

  // Constants

  // Separator for variants in file names
  private static final char _NAME_SEPARATOR = '-';

  // Extension for CSS files
  private static final String _CSS_EXTENSION = ".css";

  // Table of shared FileSystemStyleCaches, hashed by path.
  // Note on table size: We don't expect to have very many instances
  // running in a single VM - table can be small.
  private static final Hashtable<String, StyleProvider> _sSharedCaches = 
    new Hashtable<String, StyleProvider>(19);

  // Java name for UTF8 encoding
  private static String _UTF8_ENCODING = "UTF8";

  // Stub StyleSheetDocument instance
  private static final StyleSheetDocument _EMPTY_DOCUMENT =
    new StyleSheetDocument(null, null, StyleSheetDocument.UNKNOWN_TIMESTAMP);

  // Style used to represent misses in the StyleMap.
  // Package private to allow access from nested StyleMapImpl class
          static final Style _MISS = new CSSStyle();

  // Prefix to use for short style classes
  private static final String _SHORT_CLASS_PREFIX = "x";

  // -= Simon Lessard =- 
  // TODO: Check if synchronization is truly needed
  private static final Hashtable<String, String> _sCanonicalPaths = new Hashtable<String, String>(19);
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileSystemStyleCache.class);


  // use this map to map from the public style selector names to
  // our internal style selector names. The public style selector
  // names do not contain html, whereas our internal style selector
  // names may. We write out the shortened version of the mapped
  // selector names to the css file.
  // jmw. @todo Need to find a better spot for this, like the skin?
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

     // oracle.desktop special styles used in oracle.desktop.HeaderRenderer.
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-one",
                 "H1.af|panelHeader::color");
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-two",
                 "H2.af|panelHeader::color");
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-three",
                 "H3.af|panelHeader::color");
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-four",
                 "H4.af|panelHeader::color");
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-five",
                 "H5.af|panelHeader::color");
    _STYLE_KEY_MAP.put("af|panelHeader::color-level-six",
                 "H6.af|panelHeader::color");

    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-one",
                 "H1.af|panelHeader::dark");
    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-two",
                 "H2.af|panelHeader::dark");
    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-three",
                 "H3.af|panelHeader::dark");
    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-four",
                 "H4.af|panelHeader::dark");
    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-five",
                 "H5.af|panelHeader::dark");
    _STYLE_KEY_MAP.put("af|panelHeader::dark-level-six",
                 "H6.af|panelHeader::dark");

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

    /*
    _STYLE_KEY_MAP.put(":read-only",
                       ".p_AFReadOnly");
    _STYLE_KEY_MAP.put(":disabled",
                       ".p_AFDisabled");
    _STYLE_KEY_MAP.put(":error",
                       ".p_AFError");
    */
  }
  private static final String AF_STYLE_NAMESPACE = "af|";
}
