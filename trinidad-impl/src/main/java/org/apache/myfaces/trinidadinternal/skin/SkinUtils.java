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

import java.beans.Beans;

import java.io.IOException;
import java.io.InputStream;

import java.net.URL;
import java.net.URLConnection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

import java.util.HashSet;
import java.util.Iterator;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import java.util.Map;
import java.util.Set;
import java.util.Stack;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.SkinResourceLoader;
import org.apache.myfaces.trinidad.share.io.NameResolver;
import org.apache.myfaces.trinidad.skin.CustomMetadata;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinFeatures;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.share.xml.ClassParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.skin.parse.SkinsNode;
import org.apache.myfaces.trinidadinternal.skin.parse.XMLConstants;
import org.apache.myfaces.trinidadinternal.style.StyleContext;

import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;
import org.xml.sax.InputSource;


/**
 * Utility functions for creating Skin objects and SkinExtension objects
 * from the trinidad-skins.xml file and
 * adding them to the SkinFactory. It also adds SkinAdditions to the Skin objects.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinUtils.java#0 $) $Date: 10-nov-2005.18:59:00 $
 */
public class SkinUtils
{
  /**
   * An enumeration which determined how much information to return in the skin
   * debug info.
   */

  public static enum DebugInfoLevel 
  {
    TERSE,
    NORMAL,
    VERBOSE;

    @SuppressWarnings("compatibility:-5980442644319693066")
    private static final long serialVersionUID = 1L;
  }

  /**
   * Tries to retrieve the default FacesContext
   * @param context FacesContext object or null
   * @return the same context object if not null, or the default FacesContext
   * @throws java.lang.NullPointerException if unable to retrieve default FacesContext
   */
  public static FacesContext getNonNullFacesContext(FacesContext context)
  {
    if (context != null)
      return context;

    // get the current
    context = FacesContext.getCurrentInstance();

    // this cannot happen as we have a FacesContext always. If there is some case where we don't,
    // then it is a fail
    if (context == null)
      throw new NullPointerException("FacesContext is null. Cannot retrieve default FacesContext");

    return context;
  }

  /**
   * Adds skinAddition passed into the skin object passed, if the skin object does not have the same
   * skin addition already.
   *
   * @param skin
   * @param skinAddition
   * @return true if the SkinAddition was added into Skin and false if it was not.
   */
  static public boolean addSkinAdditionToSkinIfAbsent(Skin skin, SkinAddition skinAddition)
  {
    if (skin == null || skinAddition == null)
      throw new NullPointerException("Skin or SkinAddition passed is null");

    List<SkinAddition> additions = skin.getSkinAdditions();

    if (skinAddition != null)
    {
      for (SkinAddition addn : additions)
        if (addn != null && addn.equals(skinAddition))
          return false;
    }

    skin.addSkinAddition(skinAddition);
    return true;
  }

  /**
   * @param provider    skin provider
   * @param context     faces context
   * @param renderKitId renderKit Id for default skin
   * @return the default skin for the renderKit passed. Assumes renderKit as DESKTOP if null. does
   * not return null, returns the DESKTOP simple skin in worst case.
   */
  public static Skin getDefaultSkinForRenderKitId(
    SkinProvider provider,
    ExternalContext context,
    String renderKitId)
  {

    if (provider == null || context == null)
      throw new NullPointerException("SkinProvider or FacesContext is passed as null.");

    String defaultSkinId;

    if (TrinidadRenderingConstants.APACHE_TRINIDAD_PORTLET.equals(renderKitId))
      defaultSkinId = TrinidadRenderingConstants.SIMPLE_PORTLET_ID;
    else if (TrinidadRenderingConstants.APACHE_TRINIDAD_PDA.equals(renderKitId))
      defaultSkinId = TrinidadRenderingConstants.SIMPLE_PDA_ID;
    else
      defaultSkinId = TrinidadRenderingConstants.SIMPLE_DESKTOP_ID;

    Skin defaultSkin = provider.getSkin(context,
                                        new SkinMetadata.Builder().id(defaultSkinId).build());

    assert (defaultSkin != null);

    return defaultSkin;
  }

  /**
   * Builds the SkinMetadata hierarchy from trinidad-skins.xml
   *
   * @return
   */
  static public List<SkinsNode> buildSkinsNodes(ExternalContext extCtxt)
  {
    List<SkinsNode> metaInfSkinsNode = _getMetaInfSkinsNodeList();
    SkinsNode webInfSkinsNode = _getWebInfSkinsNode(extCtxt);
    List<SkinsNode> resourceLoaderSkinsNodes =
      _getSkinsNodesFromSkinResourceLoaderServices(extCtxt);

    List<SkinsNode> skinsNodes = new ArrayList<SkinsNode>(20);

    if (metaInfSkinsNode != null)
      skinsNodes.addAll(metaInfSkinsNode);

    if (webInfSkinsNode != null)
      skinsNodes.add(webInfSkinsNode);

    if (resourceLoaderSkinsNodes != null)
      skinsNodes.addAll(resourceLoaderSkinsNodes);

    return skinsNodes;
  }


  /**
   * Returns the actual Icon referenced by the ReferenceIcon.
   *
   * @param skin    the Skin to use when resolving the ReferenceIcon
   * @param refIcon a ReferenceIcon instance
   * @return icon which is resolved. i.e., it is not a ReferenceIcon.
   */
  static public Icon resolveReferenceIcon(
   Skin          skin,
   ReferenceIcon refIcon)
  {
    return _resolveReferenceIcon(skin, refIcon, null);
  }

  /**
   * Returns skin debug information of DebugInfoLevel.NORMAL.
   * 
   * @param skin
   * @return
   */
  public static String getSkinDebugInfo(Skin skin)
  {
    return getSkinDebugInfo(skin, DebugInfoLevel.NORMAL);
  }
  
  /**
   * Returns a JSON object containing debug information for a skin depending on
   * the DebugInfoLevel.  This will return a information on the supplied skin and
   * any skin it extends so that information on the skin heirarcy can be 
   * obtained.  If DebugInfoLevel.VERBOSE is specified, this will return a class
   * containing the following information:
   * 
   * id: the skin id
   * family: the skin family
   * version: the skin version
   * renderkit: the skin render kit
   * documentId: the current document checksum
   * document: the full path to the css document
   * features: a map of name/value pairs for the skinning features
   * additions: a list of skinning additions (see below) for this skin
   * parent: the json object for the stylesheet this skin extends (if any)
   * 
   * Skinning additions consist of the following properties:
   * 
   * document: the full path to the css document
   * documentId: the skin addition checksum
   * 
   * The 'parent' property is only provided if the skin is a skin extension
   * and the documentId in both the skin additions as well as the main skin object
   * is provided only if we have a valid RenderingContext.  
   * 
   * If DebugInfoLevel is NORMAL, all of the 'document' properties will contain a
   * location WITHIN the current classpath as opposed to the full resource path.
   * 
   * If DebugInfoLevel is TERSE, then information on the document, features, and
   * additions will not be provided for any object.
   * 
   * This information is in JSON format so that it can be quickly parsed and compared
   * to other skin information.
   * 
   * @param skin
   * @param level
   * @return
   */
  public static String getSkinDebugInfo(Skin skin, DebugInfoLevel level)
  {
    assert (null != skin);
    StringBuilder sb = new StringBuilder();
    
    try
    {
      JsonUtils.writeObject(sb, _getDebugInfoMap(skin, level), false);
    }
    catch (IOException e)
    {
    //We should never hit this because we control the object and the socket, but
    //if we do, we should wrap it in a runtime exception.
    throw new RuntimeException(e);
    }
      
    return sb.toString();
  }
  
  static private Map<String, Object> _getDebugInfoMap(Skin skin, DebugInfoLevel level)
  {
    assert (null != skin);
    RenderingContext rc = RenderingContext.getCurrentInstance();
    StringBuilder sb = new StringBuilder();
      
    //The Map which will be converted into JSON
    Map<String, Object> m = new LinkedHashMap<String, Object>();
    
    m.put("id", skin.getId());
    m.put("family", skin.getFamily());
    m.put("version", skin.getVersion().getName());
    m.put("renderkit", skin.getRenderKitId());
    
    if(null != rc)
    {
      m.put("documentId", skin.getStyleSheetDocumentId(rc));
    }

    //Skip all of this information for terse logging
    if(!DebugInfoLevel.TERSE.equals(level))
    {
      m.put("document", _getStyleSheetLocation(skin.getStyleSheetName(), level));
      m.put("features", skin.getSkinFeatures());

      StyleContext sctx =(null != rc && rc instanceof CoreRenderingContext)?((CoreRenderingContext)rc).getStyleContext():null;  
      List<SkinAddition> additions = skin.getSkinAdditions();

      List<Map<String,String>> additionList = new LinkedList<Map<String,String>>();
      m.put("additions",additionList);
        
      for (SkinAddition addition : additions)
      {
        Map<String,String> additionMap = new LinkedHashMap<String,String>();
        additionList.add(additionMap);
        
        String styleSheetName = addition.getStyleSheetName();
        additionMap.put("document", _getStyleSheetLocation(styleSheetName, level));
        if(null != sctx)
        {
          StyleSheetEntry entry = StyleSheetEntry.createEntry(sctx, styleSheetName);
          
          String documentId="Unknown - Entry could not be created";
          if(null != entry)
        {
            StyleSheetDocument document = entry.getDocument();
            
            documentId = (null == document)?"Unknown - Document could not be created":document.getDocumentId(sctx);
          }
          
          additionMap.put("documentId", documentId);
        }
      }
        }
    
    skin = _findParentSkin(skin);
    if(null != skin)
        {
      m.put("parent", _getDebugInfoMap(skin, level));
        }

    return m;
    }

  static private String _getStyleSheetLocation(String location, DebugInfoLevel level)
  {
    URL url = ClassLoaderUtils.getResource(location);
    String loc = (null == url)?location:url.getPath();
    
    return DebugInfoLevel.VERBOSE.equals(level)? loc:location;
  }

  /**
   * Get all the META-INF/trinidad-skins.xml files, parse them, and from each file we get a
   * SkinsNode object -- the information inside the &lt;skins&gt; element -- each skin and each
   * skin-addition.
   *
   * @return Each SkinsNode object we get from each META-INF/trinidad-skins.xml file, in a
   * List<SkinsNode>.
   */
  private static List<SkinsNode> _getMetaInfSkinsNodeList()
  {
    List<SkinsNode> allSkinsNodes = new ArrayList<SkinsNode>();
    ClassLoader loader = Thread.currentThread().getContextClassLoader();

    try
    {
      Enumeration<URL> urls = loader.getResources(_META_INF_CONFIG_FILE);
      Set<String> urlPaths = new HashSet<String>(16);

      while (urls.hasMoreElements())
      {
        URL url = urls.nextElement();

        // if url matches one we've already processed, skip it
        boolean successfullyAdded = urlPaths.add(url.getPath());
        // _processTrinidadSkinsURL logs the url we are processing
        _processTrinidadSkinsURL(allSkinsNodes, url, successfullyAdded);
      }
    }
    catch (IOException e)
    {
      _LOG.severe("ERR_LOADING_FILE", _META_INF_CONFIG_FILE);
      _LOG.severe(e);
    }

    return allSkinsNodes;
  }


  /**
   * Create a SkinExtension off a generic SAX input source, using
   * a custom parsing manager.
   * <p>
   * @param provider
   * @param resolver A NameResolver that can be used to locate
   *                 resources, such as source images for colorized
   *                 icons.
   * @param parserManager the ParserManager to use for parsing
   *                Must  be non-null.
   * @throws NullPointerException when inputStream or parserManager
   *         is null.
   */
  /**
   * @param provider      an XMLProvider implementation.
   * @param resolver      A NameResolver that can be used to locate resources, such as source images
   *                      for colorized icons.
   * @param inputStream   the inputStream. Must be non-null
   * @param parserManager the ParserManager to use for parsing Must  be non-null.
   * @param configFile    The name of the config file we are parsing.
   * @return A SkinsNode object (contains a List of SkinMetadata and a List of SkinAddition)
   */
  static private SkinsNode _getSkinsNodeFromInputStream(
    XMLProvider        provider,
    NameResolver       resolver,
    InputStream        inputStream,
    ParserManager      parserManager,
    String             configFile,
    boolean            isMetaInf
    )
  {

    if (inputStream == null)
      throw new NullPointerException(_LOG.getMessage("NO_INPUTSTREAM"));
    if (parserManager == null)
      throw new NullPointerException(_LOG.getMessage("NULL_PARSEMANAGER"));
    SkinsNode skinsNode = null;
    try
    {
      InputSource input = new InputSource();
      input.setByteStream(inputStream);
      input.setPublicId(configFile);

      ParseContextImpl context = new ParseContextImpl();

      // set a property on the parse context so that we can modify the stylesheet name
      // while creating SkinMetadata or SkinAddition
      if (isMetaInf)
        context.setProperty(XMLConstants.SKIN_NAMESPACE, XMLConstants.META_INF, isMetaInf);

      // Set up the NameResolver if we have one
      if (resolver != null)
        XMLUtils.setResolver(context, resolver);

      // Create the TreeBuilder
      TreeBuilder builder = new TreeBuilder(parserManager,
                                            SkinsNode.class);
      skinsNode = ((SkinsNode) builder.parse(provider, input, context));

      if (isMetaInf)
        context.setProperty(XMLConstants.SKIN_NAMESPACE, XMLConstants.META_INF, null);
    }
    catch (Exception e)
    {
      _LOG.warning("SKIN_CONFIG_PROCESS_FAILURE", configFile);
      _LOG.warning(e);
    }
    finally
    {
      try
      {
        inputStream.close();
      }
      catch (IOException ioe)
      {
        // Ignore
        ;
      }
    }
    return skinsNode;
  }

  /**
   * Creates a ParserManager pre-registered witih all the default ParserFactories needed to create
   * SkinExtensions.
   */
  static public ParserManager createDefaultManager()
  {
    ParserManager manager = new ParserManager();

    // Register top-level factory
    _registerFactory(manager, SkinsNode.class, "SkinsNode");

    // Register skin node factory and skin addition node factory
    _registerFactory(manager, SkinMetadata.class, "SkinMetadata");
    _registerFactory(manager, SkinAddition.class, "SkinAddition");
    // TODO: change the parser names
    _registerFactory(manager, SkinVersion.class, "SkinVersion");
    _registerFactory(manager, SkinFeatures.class, "SkinFeatures");
    _registerFactory(manager, CustomMetadata.class, "CustomMetadata");

    return manager;
  }

  //This will unwrap the skin of any RequestSkinWrappers and look for a SkinExtension.
  //If one is found, it will return the parent skin.
  static private Skin _findParentSkin(Skin skin)
  {
    //First lets unwrap
    while(skin instanceof RequestSkinWrapper)
    {
      skin = ((RequestSkinWrapper)skin).getWrappedSkin();
    }
    
    if(skin instanceof SkinExtension)
    {
      return ((SkinExtension)skin).getBaseSkin();
    }
    
    return null;
  }

  // Returns a singleton instance of the default ParserManager
  static private ParserManager _getDefaultManager()
  {
    if (_sManager == null)
      _sManager = createDefaultManager();

    return _sManager;
  }

  // Registers a ParserFactory for the LAF namespace
  static private void _registerFactory(
    ParserManager manager,
    Class<?> expectedType,
    String baseName
    )
  {
    String className = _LAF_PARSE_PACKAGE + baseName + "Parser";
    ParserFactory factory = new ClassParserFactory(className);

    manager.registerFactory(expectedType,
                            XMLConstants.SKIN_NAMESPACE,
                            factory);
  }

  /**
   * Get the WEB-INF/trinidad-skins.xml file, parse it, and return a List of SkinsNode objects.
   *
   * @param context ServletContext used to getResourceAsStream
   * @return List of SkinNodes (skin elements) found in trinidad-skins.xml
   */
  private static SkinsNode _getWebInfSkinsNode(
    ExternalContext context)
  {
    InputStream in = context.getResourceAsStream(_CONFIG_FILE);
    if (in != null)
    {
      SkinsNode webInfSkinsNode =
        _getSkinsNodeFromInputStream(null, null, in, _getDefaultManager(), _CONFIG_FILE, false);
      if (_LOG.isFine())
      {
        for (SkinMetadata node : webInfSkinsNode.getSkinNodes())
        {
          _LOG.fine("Skin {0} with stylesheet {1}",
                    new Object[]{node.getId(), node.getStyleSheetName()});
        }
        for (SkinAddition node : webInfSkinsNode.getSkinAdditionNodes())
        {
          _LOG.fine("SkinAddition {0} with stylesheet {1}",
                    new Object[]{node.getSkinId(), node.getStyleSheetName()});

        }
      }
      return webInfSkinsNode;
    }
    else
    {
      return null;
    }
  }

  /**
   * Looks for SPIs registered in META-INF/services folder with file name
   * "org.apache.myfaces.trinidad.resource.SkinResourceLoader". Loads the trinidad-skins.xml exposed
   * by these resource loades by calling findResource(). Creates SkinsNodes and returns
   *
   * @param context
   * @return list of SkinsNode representing trinidad-skins.xml loaded using SkinResourceLoader SPI
   */
  private static List<SkinsNode> _getSkinsNodesFromSkinResourceLoaderServices(
    ExternalContext context)
  {
    if (_LOG.isFine()) _LOG.fine("Parse SkinResourceLoader trinidad-skins.xml");
    // register skins found in DT using the META-INF/services
    List<SkinResourceLoader> urlProviders = ClassLoaderUtils.getServices(
                                      "org.apache.myfaces.trinidad.resource.SkinResourceLoader");
    if (urlProviders != null && !urlProviders.isEmpty())
    {
      return _getResourceLoadedTrinidadSkins(context, urlProviders);
    }

    return Collections.emptyList();
  }

  /**
   * Given the list of SkinResourceLoaders, load the trindiad-skins This is used by DT to load
   * trinidad-skins.xml not in META-INF or WEB-INF
   *
   * @param context
   * @param providers
   * @return
   */
  private static List<SkinsNode> _getResourceLoadedTrinidadSkins(
    ExternalContext context,
    List<SkinResourceLoader> providers)
  {
    Set<String> urlPaths = new HashSet<String>(16);
    List<SkinsNode> allSkinsNodes = new ArrayList<SkinsNode>();


    for (SkinResourceLoader urlProvider : providers)
    {
      Iterator<URL> urlIterator = urlProvider.findResources(context, _TRINIDAD_SKINS_XML);

      if (urlIterator != null)
      {
        try
        {
          while (urlIterator.hasNext())
          {
            URL url = urlIterator.next();
            // if url matches one we've already processed, skip it
            boolean successfullyAdded = urlPaths.add(url.getPath());
            // _processTrinidadSkinsURL logs the url we are processing
            _processTrinidadSkinsURL(allSkinsNodes, url, successfullyAdded);
          }

        }
        catch (IOException e)
        {
          _LOG.severe("ERR_LOADING_FILE", _META_INF_CONFIG_FILE);
          _LOG.severe(e);
        }
      }
    }


    return allSkinsNodes;
  }

  private static void _processTrinidadSkinsURL(
    List<SkinsNode> allSkinsNodes,
    URL url,
    boolean successfullyAdded)
    throws IOException
  {
    if (!successfullyAdded)
    {
      if (_LOG.isFine())
      {
        _LOG.fine("Skipping skin URL:{0} because it was already processed. " +
                    "It was on the classpath more than once.",
                  url);
      }
      // continue to the next url
    }
    else
    {
      if (_LOG.isFine()) _LOG.fine("Processing skin URL:{0}", url);

      URLConnection urlConnection = url.openConnection();
      // the url here points to the trinidad-skins.xml, we do not expect this to change at run time
      // so we can use caching for the URL in runtime.
      // prevent caching during DT where the source may change...
      if (Beans.isDesignTime())
      {
        urlConnection.setUseCaches(false);
      }
      InputStream in = urlConnection.getInputStream();
      try
      {
        // parse the config file and register the skin's additional stylesheets.

        if (in != null)
        {
          SkinsNode metaInfSkinsNode =
            _getSkinsNodeFromInputStream(null, null, in,
                                         _getDefaultManager(),
                                         url.toString(), true);

          if (metaInfSkinsNode != null)
          {
            // for debug only.
            if (_LOG.isFine())
            {
              for (SkinMetadata node : metaInfSkinsNode.getSkinNodes())
                _LOG.fine("Skin {0} with stylesheet {1}",
                          new Object[]{node.getId(), node.getStyleSheetName()});
              for (SkinAddition node : metaInfSkinsNode.getSkinAdditionNodes())
                _LOG.fine("SkinAddition {0} with stylesheet {1}",
                          new Object[]{node.getSkinId(), node.getStyleSheetName()});
            }

            allSkinsNodes.add(metaInfSkinsNode);
          }
          else
          {
            if (_LOG.isFine()) _LOG.fine("No skins found in the URL.");
          }
        }
      }
      catch (Exception e)
      {
        _LOG.warning("ERR_PARSING", url);
        _LOG.warning(e);
      }
      finally
      {
        in.close();
      }
    }
  }

  /**
   * Helper for resolveReferenceIcon which uses a Stack of icon names to detect circular
   * dependencies.
   *
   * @param skin                the Skin to use when resolving the ReferenceIcon
   * @param refIcon             a ReferenceIcon instance
   * @param referencedIconStack The stack of reference icon names which have already been visited.
   *                            Used to detect circular dependencies.
   * @return icon which is resolved. i.e., it is not a ReferenceIcon.
   */
  static private Icon _resolveReferenceIcon(
    Skin          skin,
    ReferenceIcon refIcon,
    Stack<String> referencedIconStack)
  {
    String refName = refIcon.getName();

    // make sure we don't have a circular dependency
    if ((referencedIconStack != null) && referencedIconStack.contains(refName))
    {
      if (_LOG.isWarning())
        _LOG.warning("SKIN_CIRCULAR_INCLUDE_ERROR", refName);
      return null;
    }

    if (referencedIconStack == null)
    {
      referencedIconStack = new Stack<String>();
    }

    referencedIconStack.push(refName);

    Icon icon = skin.getIcon(refName, false);

    if ((icon instanceof ReferenceIcon) && (icon != null))
    {

      return _resolveReferenceIcon(skin,
                                   (ReferenceIcon) icon,
                                   referencedIconStack);

    }

    return icon;
  }


  private SkinUtils()
  {
  }

  // The default ParserManager
  static private ParserManager _sManager;

  // Constants

  // Prefix of LAf parsing package
  static private final String         _LAF_PARSE_PACKAGE    =
    "org.apache.myfaces.trinidadinternal.skin.parse.";
  static private final String         _CONFIG_FILE          = "/WEB-INF/trinidad-skins.xml";
  static private final String         _META_INF_CONFIG_FILE = "META-INF/trinidad-skins.xml";
  static private final String         _TRINIDAD_SKINS_XML   = "trinidad-skins.xml";
  static private final TrinidadLogger _LOG                  =
    TrinidadLogger.createTrinidadLogger(SkinUtils.class);
}