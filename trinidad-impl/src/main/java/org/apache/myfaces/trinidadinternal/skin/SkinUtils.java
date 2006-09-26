
/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.skin;

import java.io.IOException;

import java.io.InputStream;
import javax.servlet.ServletContext;
import javax.xml.parsers.SAXParserFactory;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;



import org.apache.myfaces.trinidadinternal.renderkit.core.skin.MinimalDesktopSkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.MinimalPdaSkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.SimpleDesktopSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.SimplePdaSkin;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.SkinPropertyNode;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.apache.myfaces.trinidadinternal.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.xml.ClassParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.share.xml.ParserFactory;
import org.apache.myfaces.trinidadinternal.share.xml.ParserManager;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import org.apache.myfaces.trinidadinternal.share.xml.XMLProvider;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.apache.myfaces.trinidadinternal.style.Style;

import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.IconParserFactory;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * Utility functions for creating Skins from the trinidad-skins.xml file
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinUtils.java#0 $) $Date: 10-nov-2005.18:59:00 $
 * @author The Oracle ADF Faces Team
 */
public class SkinUtils
{

  /**
   * Register the base skins with the SkinFactory.
   * Make sure the SkinFactory.getFactory() does not return null before
   * calling this method.
   */
  static public void registerBaseSkins()
  {

    SkinFactory skinFactory = SkinFactory.getFactory();

    // skinFactory should be non-null when this is called since it is
    // initiated in the AdfFacesFilterImpl, but in case it isn't do this
    if (skinFactory == null)
    {
      SkinFactory.setFactory(new SkinFactoryImpl());
      skinFactory = SkinFactory.getFactory();
    }

    _registerAdfFacesSkins(skinFactory);
  }
  
  /**
   * Register any custom skin extensions found in the
   * trinidad-skins.xml file with the SkinFactory.
   * 
   * Make sure the SkinFactory.getFactory() does not return null before
   * calling this method.
   * You should call registerBaseSkins() before calling this method.
   * @param context ServletContext, used to get the trinidad-skins.xml file.
   */
  static public void registerSkinExtensions(
    ServletContext context)
  {

    SkinFactory skinFactory = SkinFactory.getFactory();

    // skinFactory should be non-null when this is called since it is
    // initiated in the AdfFacesFilterImpl, but in case it isn't do this
    if (skinFactory == null)
    {
      SkinFactory.setFactory(new SkinFactoryImpl());
      skinFactory = SkinFactory.getFactory();
    }

    _registerSkinExtensions(context, skinFactory);

  }

  /**
   * Create a SkinExtension[] off a generic SAX input source, using
   * the default parsing manager.
   * <p>
   * @param skinFactory The SkinFactory that will be used
   *        to obtain references to Skin instances that this
   *        SkinExtension depends on, such as the base Skin.
   * @param provider an XMLProvider implementation
   * @param resolver A NameResolver that can be used to locate
   *                 resources, such as source images for colorized
   *                 icons.
   * @param source the SAX input source to load the Skin contents from
   */
  static private SkinExtension[] _createSkinExtensions(
    SkinFactory        skinFactory,
    XMLProvider        provider,
    NameResolver       resolver,
    InputSource        source
    ) throws IOException, SAXException
  {
    return _createSkinExtensions(skinFactory,
                                provider,
                                resolver,
                                source,
                                _getDefaultManager());
  }

  /**
   * Create a SkinExtension off a generic SAX input source, using
   * a custom parsing manager.
   * <p>
   * @param skinFactory The SkinFactory that will be used
   *        to obtain references to Skin instances that this
   *        SkinExtension depends on, such as the base Skin. Must  be non-null.
   * @param provider an XMLProvider implementation.
   * @param resolver A NameResolver that can be used to locate
   *                 resources, such as source images for colorized
   *                 icons.
   * @param source the SAX input source to load the LookAndFeel contents from
   *               Must  be non-null.
   * @param manager the ParserManager to use for parsing
   *                Must  be non-null.
   * @throws NullPointerException when skinFactory, source, or parserManager
   *         is null.
   */
  static private SkinExtension[] _createSkinExtensions(
    SkinFactory        skinFactory,
    XMLProvider        provider,
    NameResolver       resolver,
    InputSource        source,
    ParserManager      parserManager
    ) throws IOException, SAXException
  {
    if (skinFactory == null)
      throw new NullPointerException("Null skinFactory");
    if (source == null)
      throw new NullPointerException("Null source");
    if (parserManager == null)
      throw new NullPointerException("Null parserManager");


    ParseContextImpl context = new ParseContextImpl();

    // Set up the SkinFactory on the ParseContext
    context.setProperty(XMLConstants.SKIN_NAMESPACE,
                        _SKIN_FACTORY_PROPERTY,
                        skinFactory);

    // Set up the NameResolver if we have one
    if (resolver != null)
      XMLUtils.setResolver(context, resolver);

    // Create the TreeBuilder
    TreeBuilder builder = new TreeBuilder(parserManager,
                                          SkinExtension[].class);

    return ((SkinExtension[])builder.parse(provider, source, context));

  }

  /**
   * Creates a ParserManager pre-registered witih all
   * the default ParserFactories needed to create SkinExtensions.
   */
  static public ParserManager createDefaultManager()
  {
    ParserManager manager = new ParserManager();

    // Register top-level factory
    _registerFactory(manager,
                     SkinExtension[].class,
                     "SkinsNode");
    _registerFactory(manager,
                     SkinExtension.class,
                     "SkinExtension");

    // Icon-related factories
    _registerFactory(manager, IconNode[].class, "IconsNode");
    _registerFactory(manager, IconNode.class, "IconNode");
    manager.registerFactory(Icon.class,
                            XMLConstants.SKIN_NAMESPACE,
                            new IconParserFactory());

    // Property-related factories
    _registerFactory(manager, SkinPropertyNode[].class, "SkinPropertiesNode");
    _registerFactory(manager, SkinPropertyNode.class, "SkinPropertyNode");

    // Parser for inline styles ????
    ClassParserFactory styleFactory = new ClassParserFactory(
        "org.apache.myfaces.trinidadinternal.style.xml.parse.CSSStyleParser");
    manager.registerFactory(Style.class,
                            XMLConstants.SKIN_NAMESPACE,
                            styleFactory);

    return manager;
  }

  /**
   * Returns the Skin with the specified id for
   * the current SkinFactory. Used to get the base skin while parsing
   * SkinExtensions.
   * @param context The current ParseContext
   * @param id The id of the Skin to retrieve.
   */
  static public Skin getSkin(
    ParseContext context,
    String       id
    )
  {
    // get the SkinFactory from the ParseContext.
    SkinFactory factory = getSkinFactory(context);
    if (factory == null)
     throw new NullPointerException("Null skin factory");

    return factory.getSkin(null, id);
  }

  /**
   * Returns the current SkinFactory.
   * @param context The current ParseContext
   */
  static public SkinFactory getSkinFactory(ParseContext context)
  {
    return (SkinFactory)context.getProperty(
                                 XMLConstants.SKIN_NAMESPACE,
                                 _SKIN_FACTORY_PROPERTY);
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
   * register the AdfFaces skins: simpleDesktopSkin, simplePdaSkin,
   * and minimalDesktopSkin, minimalPdaSkin, and blafPlusDesktopSkin.
   * @param skinFactory
   */
  private static void _registerAdfFacesSkins(
    SkinFactory skinFactory)
  {
    // SimpleDesktopSkin is the BASE skin for org.apache.myfaces.trinidad.desktop renderKit
    // SimplePdaSkin is the BASE skin for org.apache.myfaces.trinidad.pda renderKit. By
    // BASE skin, I mean, this is the skin that all SkinExtensions extend
    // from.
    SimpleDesktopSkin simpleDesktopSkin = new SimpleDesktopSkin();
    skinFactory.addSkin(simpleDesktopSkin.getId(), simpleDesktopSkin);

    SimplePdaSkin simplePdaSkin = new SimplePdaSkin();
    skinFactory.addSkin(simplePdaSkin.getId(), simplePdaSkin);

    MinimalDesktopSkinExtension minimalDesktopSkin =
      new MinimalDesktopSkinExtension(simpleDesktopSkin);
    skinFactory.addSkin(minimalDesktopSkin.getId(), minimalDesktopSkin);

    MinimalPdaSkinExtension minimalPdaSkin =
      new MinimalPdaSkinExtension(simplePdaSkin);
    skinFactory.addSkin(minimalPdaSkin.getId(), minimalPdaSkin);
  }

  /**
   * Parse the trinidad-skins.xml file for SkinExtensions and add each
   * SkinExtension to the skinFactory.
   * @param context
   * @param skinFactory
   */
  private static void _registerSkinExtensions(
    ServletContext context,
    SkinFactory skinFactory)
  {
    if (context == null)
      return;

    // =-=jmw @todo find trinidad-skins.xml in jar
    InputStream in = context.getResourceAsStream(_CONFIG_FILE);
    if (in != null)
    {
      try
      {
        InputSource input = new InputSource();
        input.setByteStream(in);
        input.setPublicId(_CONFIG_FILE);

        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);

        // as we parse each skin, we register with the skin factory.
        // this way we can extend any skin we want.
        _createSkinExtensions(skinFactory, null, null, input);

      }
      catch (IOException ioe)
      {
        _LOG.warning(ioe);
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
  }

  private SkinUtils() {}

  // The default ParserManager
  static private ParserManager _sManager;

  // Constants

  // Prefix of LAf parsing package
  static private final String _LAF_PARSE_PACKAGE =
    "org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.";

  // Property for storing/retrieving the SkinFactory
  static private final String _SKIN_FACTORY_PROPERTY = "_skinFactory";

  static private final String _CONFIG_FILE = "/WEB-INF/trinidad-skins.xml";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinUtils.class);

}
