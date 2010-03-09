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
package org.apache.myfaces.trinidadinternal.skin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import java.net.URL;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.share.io.NameResolver;

import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.util.URLUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.URIImageIcon;
import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludeStyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.SkinPropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * Utility class for creating a StyleSheetDocument.
 * The main method is parseCSSSource which creates a StyleSheetEntry.
 * The interim object is SkinStyleSheetNode
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleSheetParserUtils.java#0 $) $Date: 10-nov-2005.18:59:00 $
 */
class SkinStyleSheetParserUtils
{
  /**
   * Parses a Skin style-sheet that is in the CSS-3 format.
   * @param context the current ParseContext
   * @param resolver a NameResolver to locate the target
   *                ( Given a name, returns an InputStreamProvider.)
   * @param sourceName the name of the target, relative to the current file
   * @param expectedType the expected Java type of the target.
   */
  static public StyleSheetEntry parseCSSSource(
    ParseContext  context,
    NameResolver  resolver,
    String        sourceName,
    Class<?>      expectedType) throws IOException
  {

    if (expectedType == null)
      throw new NullPointerException();
    if (resolver == null)
      throw new NullPointerException();
    if (sourceName == null)
      throw new NullPointerException();
    if (context == null)
      throw new NullPointerException();

    InputStreamProvider provider = resolver.getProvider(sourceName);
    Object cached = provider.getCachedResult();
    if ((cached != null) && expectedType.isInstance(cached))
      return (StyleSheetEntry)cached;

    InputStream stream = provider.openInputStream();

    try
    {
      // Store a resolver relative to the file we're about to parse
      // Store the inputStreamProvider on the context;
      // this will be used to get the document's timestamp later on
      XMLUtils.setResolver(context, resolver.getResolver(sourceName));
      XMLUtils.setInputStreamProvider(context, provider);

      // PARSE!
      // create a SkinStyleSheetNode
      // (contains a namespaceMap and a List of SkinSelectorPropertiesNodes
      // and additional information like direction, locale, etc.)
      // (selectorName + a css propertyList))
      BufferedReader in = new BufferedReader(new InputStreamReader(stream));
      List <SkinStyleSheetNode> skinSSNodeList = _parseCSSStyleSheet(in);
      in.close();

      // process the SkinStyleSheetNodes to create a StyleSheetEntry object
      StyleSheetEntry styleSheetEntry =
        _createStyleSheetEntry(context, sourceName, skinSSNodeList);

      // Store the cached result (if successful)
      // otherwise, if we don't do this, we will keep reparsing. Somehow
      // this affects whether the file has been modified. STRANGE!
      //     if (value != null)
      //    provider.setCachedResult(value);

      //    return value;
      provider.setCachedResult(styleSheetEntry);

      return styleSheetEntry;
    }
    finally
    {
      stream.close();
    }
  }

  /**
   * Trim the leading/ending quotes, if any.
   */
  public static String trimQuotes(String in)
  {
    int length = in.length();
    if (length <= 1)
      return in;
    // strip off the starting/ending quotes if there are any
    char firstChar = in.charAt(0);
    int firstCharIndex = 0;
    if ((firstChar == '\'') || (firstChar == '"'))
      firstCharIndex = 1;

    char lastChar = in.charAt(length-1);
    if ((lastChar == '\'') || (lastChar == '"'))
      length--;

    return in.substring(firstCharIndex, length);
  }

  /**
   * Given a List of SkinStyleSheetNode, create StyleSheetEntry.
   * A StyleSheetEntry is an object that contains:
   * styleSheetName, StyleSheetDocument
   * A StyleSheetDocument contains StyleSheetNodes. A StyleSheetNode contains
   * a list style selectors and their properties and additional info like
   * the direction, locale, etc. for this list of selectors.
   * @param context
   * @param sourceName
   * @param skinSSNodeList
   * @return
   */
  private static StyleSheetEntry _createStyleSheetEntry(
    ParseContext  context,
    String        sourceName,
    List <SkinStyleSheetNode> skinSSNodeList
    )
  {


    // Get each SkinStyleSheetNode, and for each SkinStyleSheetNode get a
    // styleNodeList. Also, build one iconNodeList and one skinPropertyNodeList.

    // initialize
    List<StyleSheetNode> ssNodeList = new ArrayList<StyleSheetNode>();
    String baseSourceURI = CSSUtils.getBaseSkinStyleSheetURI(sourceName);

    // loop through the selectors and its properties
    for (SkinStyleSheetNode skinSSNode : skinSSNodeList)
    {

      // selector and its properties
      List <SkinSelectorPropertiesNode> selectorNodeList =
        skinSSNode.getSelectorNodeList();
      //Map namespaceMap = styleSheetNode.getNamespaceMap();

      // initialize
      List <StyleNode> styleNodeList = new ArrayList<StyleNode>();
      List<IconNode> iconNodeList = new ArrayList<IconNode>();
      // trSkinPropertyNodeList, e.g., af|foo {-tr-show-last-item: true}
      List<SkinPropertyNode> trSkinPropertyNodeList = new ArrayList<SkinPropertyNode>();

      // process each selector and all its name+values
      for (SkinSelectorPropertiesNode cssSelector : selectorNodeList)
      {

        String selectorName = cssSelector.getSelectorName();
        // PropertyNode is the name+value, like font-size: 8px
        List<PropertyNode> propertyList = cssSelector.getPropertyNodes();
        int direction     = skinSSNode.getDirection();

        ResolvedSkinProperties resolvedProperties =
          _resolveProperties(selectorName,
                             propertyList);


        trSkinPropertyNodeList.addAll(resolvedProperties.getSkinPropertyNodeList());

        List<PropertyNode> noTrPropertyList =
          resolvedProperties.getNoTrPropertyList();

        if (_isIcon(selectorName))
        {
          // knock off the '.' if it is the first character.
          if (selectorName.charAt(0) == '.')
            selectorName = selectorName.substring(1);
          // strip out :alias
          selectorName = selectorName.replaceFirst(":alias", "");
          // add :rtl if the direction is rtl
          if (direction == LocaleUtils.DIRECTION_RIGHTTOLEFT)
            selectorName = selectorName.concat(StyleUtils.RTL_CSS_SUFFIX);

          // create an IconNode object and add it ot the iconNodeList
          boolean addStyleNode = _addIconNode(sourceName,
                                              baseSourceURI,
                                              selectorName,
                                              noTrPropertyList,
                                              iconNodeList);
          if (addStyleNode)
          {
            _addStyleNode(selectorName,
                          noTrPropertyList,
                          resolvedProperties.getTrRuleRefList(),
                          resolvedProperties.getInhibitedProperties(),
                          resolvedProperties.isTrTextAntialias(),
                          styleNodeList);
          }
        }
        else
        {
          // create a StyleNode object and add it to the styleNodeList.
          _addStyleNode(selectorName,
                        noTrPropertyList,
                        resolvedProperties.getTrRuleRefList(),
                        resolvedProperties.getInhibitedProperties(),
                        resolvedProperties.isTrTextAntialias(),
                        styleNodeList);

        }
      }

      if ((styleNodeList.size() > 0) || (iconNodeList.size() > 0)
          || (trSkinPropertyNodeList.size() > 0))
      {
        // we need to deal with the styleNodeList by building a StyleSheetNode
        // with this information.
        // create a StyleSheetNode, add to the ssNodeList
        StyleNode[] styleNodeArray = styleNodeList.toArray(new StyleNode[0]);
        StyleSheetNode ssNode =
          new StyleSheetNode(styleNodeArray,
                             iconNodeList,
                             trSkinPropertyNodeList,
                             skinSSNode.getLocales(),
                             skinSSNode.getDirection(),
                             skinSSNode.getAgentMatcher(),
                             skinSSNode.getPlatforms(),
                             0,
                             skinSSNode.getAcessibilityProperties());
        ssNodeList.add(ssNode);
      }

    } // end for each SkinStyleSheetNode


    // StyleSheetDocument contains StyleSheetNode[] styleSheets
    StyleSheetDocument ssDocument =
      _createStyleSheetDocument(context, ssNodeList);

    return new StyleSheetEntry(sourceName,
                               ssDocument);



  }

  /**
   * Loop thru every property in the propertyList and store them in
   * the ResolvedSkinProperties inner class.
   * @param selectorName
   * @param propertyNodeList
   * @return
   */
  private static ResolvedSkinProperties _resolveProperties(
    String selectorName,
    List<PropertyNode> propertyNodeList)
  {

    List<PropertyNode> noTrPropertyList = new ArrayList<PropertyNode>();
    List<String> trRuleRefList = new ArrayList<String>();
    Set<String> inhibitedPropertySet = new TreeSet<String>();
    List<SkinPropertyNode> skinPropertyNodeList =
      new ArrayList<SkinPropertyNode>();

    boolean trTextAntialias = false;

    // loop through each property in the propertyList
    // and resolve into
    // noTrPropertyList (properties that do not start with -tr-.
    //                  (or -ora- for backwards compatibility))
    // trRuleRefList (properties that start with -tr-rule-ref
    //                (or -ora-rule-ref for backwards compatibility))
    // boolean trTextAntialias (property value for -tr-text-antialias
    //                      (or -ora-text-antialias for backwards compatibility)
    // skinPropertyNodeList (all other properties that start with -tr-
    //                       (or -ora- for backwards compatibility))
    // These properties are stored in the ResolvedSkinProperties inner class.

    for(PropertyNode propertyNode : propertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();

      if(propertyName != null && propertyValue != null)
      {
        boolean oraProperty = propertyName.startsWith(_ORA_PROPERTY_PREFIX);
        boolean trProperty = propertyName.startsWith(_TR_PROPERTY_PREFIX);
        if( oraProperty || trProperty)
        {
          int suffixIndex = (oraProperty) ?
                              _ORA_PROPERTY_PREFIX.length() :
                              _TR_PROPERTY_PREFIX.length();
          String propertyNameSuffix = propertyName.substring(suffixIndex);
          if (propertyNameSuffix.equals(_PROPERTY_RULE_REF))
          {
            // add the rule ref value to the list
            trRuleRefList.add(propertyValue);
          }
          else if (propertyNameSuffix.equals(_PROPERTY_TEXT_ANTIALIAS))
          {
            if ("true".equals(propertyValue))
              trTextAntialias = true;

          }
          else if (propertyNameSuffix.equals(_PROPERTY_INHIBIT))
          {
            for (String value : _SPACE_PATTERN.split(propertyValue))
            {
              inhibitedPropertySet.add(value);
            }
          }
          else
          {
            // create the SkinPropertyNode
            SkinPropertyNode node =
              _createSkinPropertyNode(selectorName, propertyName, propertyValue);

            skinPropertyNodeList.add(node);
          }
        }
        else
        {
          noTrPropertyList.add(propertyNode);
        }
      }
    }

    return new ResolvedSkinProperties(
      noTrPropertyList,
      trRuleRefList,
      inhibitedPropertySet,
      skinPropertyNodeList,
      trTextAntialias);
  }

  /**
   * Create an IconNode and add it to the iconNodeList.
   * @param sourceName
   * @param baseSourceURI
   * @param selectorName
   * @param noTrPropertyNodeList
   * @param iconNodeList
   * @return boolean true if this "icon" does not contain an image url or text icon as the
   * property value of 'content:'. That means it is only css styles.
   */
  private static boolean _addIconNode(
    String             sourceName,
    String             baseSourceURI,
    String             selectorName,
    List<PropertyNode> noTrPropertyNodeList,
    List<IconNode>     iconNodeList)
  {

    // these are icon properties.
    // create an IconNode.
    // get content property value. This is how i decide if it is an url or a text icon.
    //
    // loop through all the properties
    // TextIcons take text, rtlText, inlineStyle, styleClass
    // url icons take uri, rtluri, width, height, styleClass, inlineStyle
    // Icon selectors that end with :rtl will be a separate Icon object.
    // I won't combine :rtl icons with regular icons into the same object
    // like we did in 2.2.
    // af|breadCrumbs::separatorIcon {content: ">"}
    // af|breadCrumbs::separatorIcon:rtl {content: "<"}
    // this will create
    // key=af|breadCrumbs::separatorIcon with TextIcon(">", ">", style, inlineStyle)
    // and
    // key=af|breadCrumbs::separatorIcon:rtl with TextIcon("<", "<", rtlstyle, rtlinlineStyle)
    // then when I go to get the icon af|breadCrumbs::separatorIcon, the skin
    // will know to ask for af|breadCrumbs::separatorIcon:rtl or af|breadCrumbs::separatorIcon
    // depending upon the DIRECTION that is set on the context.
    // The current Icon classes code will not have to change.


    Integer width = null;
    String  widthValue = null;
    Integer height = null;
    String  heightValue = null;
    //String  styleClass = null;
    String  uri = null;
    String  text = null;
    boolean isNullIcon = false;
    boolean createStyleNode = false;
    // append all the styles that are not content, width or height into
    // inline style
    CSSStyle inlineStyle = null;

    for(PropertyNode propertyNode : noTrPropertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();
      if (propertyName.equals("width"))
      {
        // save original width value
        // strip off px from the string and return an Integer
        if (_INTEGER_PATTERN.matcher(propertyValue).matches())
        {
          widthValue = propertyValue;
          width = _convertPxDimensionStringToInteger(widthValue);
        }
        else
        {
          widthValue = null;
          // use inlineStyle for non-integer width values;
          if (inlineStyle == null)
          {
            inlineStyle = new CSSStyle();
          }
          inlineStyle.setProperty(propertyName, propertyValue);
        }
      }
      else if (propertyName.equals("height"))
      {
        // save original height value
        // strip off px from the string and return an Integer
        if (_INTEGER_PATTERN.matcher(propertyValue).matches())
        {
          heightValue = propertyValue;
          height = _convertPxDimensionStringToInteger(heightValue);
        }
        else
        {
          // use inlineStyle for non-integer height values;
          heightValue = null;
          if (inlineStyle == null)
          {
            inlineStyle = new CSSStyle();
          }
          inlineStyle.setProperty(propertyName, propertyValue);
        }
      }
      else if (propertyName.equals("content"))
      {
        // is it a text or uri
        if (_isURLValue(propertyValue))
        {
          uri = _getURIString(propertyValue);
        }
        else if (propertyValue.startsWith("inhibit"))
        {
          isNullIcon = true;
        }
        else
        {
          text = trimQuotes(propertyValue);
        }

      }
      else
      {
        // create an inlineStyle with all the extraneous style properties
        if (inlineStyle == null)
          inlineStyle = new CSSStyle();
        inlineStyle.setProperty(propertyName, propertyValue);
      }

    }
    // now I need to create the icon.
    // do not create an icon if isNullIcon is true.
    Icon icon = null;

    if (!isNullIcon)
    {
      if (text != null)
      {
        // don't allow styleClass from the css parsing file. We can handle
        // this when we have style includes
        // put back the width/height properties if there were some
        if ((heightValue != null || widthValue != null) && inlineStyle == null)
          inlineStyle = new CSSStyle();
        if (heightValue != null)
         inlineStyle.setProperty("height", heightValue);
        if (widthValue != null)
          inlineStyle.setProperty("width", widthValue);
        icon = new TextIcon(text, text, null, inlineStyle);
      }
      else if (uri != null)
      {


        // a leading / indicates context-relative
        //      (auto-prefix the servlet context)
        // a leading // indicates server-relative
        //      (don't auto-prefix the servlet context).

        boolean startsWithTwoSlashes = uri.startsWith("//");
        if (!startsWithTwoSlashes && uri.startsWith("/"))
        {

          uri = uri.substring(1);

          icon =
            new ContextImageIcon(uri, uri, width, height, null, inlineStyle);
        }
        else
        {
          // a. if it has two slashes, strip off one.
          // b. if it starts with http: don't do anything to the uri
          // c. if it an absolute url, then it should be relative to
          // the skin file since they wrote the absolute url in the skin file.
          if (startsWithTwoSlashes)
            uri = uri.substring(1);
          else if (!(uri.startsWith("http:")))
            uri = CSSUtils.getAbsoluteURIValue(sourceName, baseSourceURI, uri);
          icon =
            new URIImageIcon(uri, uri, width, height, null, inlineStyle);
        }
      }
      else
      {
        /// neither text or image icon.
        if (inlineStyle != null)
        {
         // create a styleNode, too with the inlineStyles.
         createStyleNode = true;
        }
      }
    }
    else
    {
      icon = NullIcon.sharedInstance();
    }

    // if icon is not null, create an IconNode

    if (icon != null)
      iconNodeList.add(new IconNode(selectorName, icon));

    return createStyleNode;

  }

  /**
   * Creates a StyleNode object and adds it to the styleNodeList
   * @param selectorName
   * @param propertyNodeList
   * @param trRuleRefList
   * @param styleNodeList
   */
  private static void _addStyleNode(
    String             selectorName,
    List<PropertyNode> propertyNodeList,
    List<String>       trRuleRefList,
    Set<String>        inhibitedProperties,
    boolean            trTextAntialias,
    List<StyleNode>    styleNodeList)
  {

    // these are the styles.
    // At this point I have a selector name and the properties.
    // create a StyleNode based on this information.

    String name = null;
    String selector = null;
    int aliasIndex = selectorName.indexOf(":alias");
    if (aliasIndex > -1)
    {
      // :alias means do not output style; it is a namedStyle, so we set
      // the name and not the selector.
      // first, strip off the '.' at the beginning and the :alias bit.
      name = selectorName.substring(1, aliasIndex);
    }
    else
      selector = selectorName;

    // add text-antialias if it is set
    if (trTextAntialias)
    {
      propertyNodeList.add(new PropertyNode("text-antialias", "true"));
    }
    // convert to a PropertyNode[], because StyleNode takes this type.
    PropertyNode[] propertyArray =
      propertyNodeList.toArray(new PropertyNode[propertyNodeList.size()]);

    // if the trRuleRefList is not empty, create IncludeStyleNodes.
    int length = trRuleRefList.size();
    List<IncludeStyleNode> includeStyleNodes = new ArrayList<IncludeStyleNode>();

    if (length > 0)
    {
      for(String value : trRuleRefList)
      {
        // parse the value, which will be of this form:
        // -tr-rule-ref: selector(".AFBaseFont:alias") selector(".Foo")
        // where you have more than one selector in an -tr-rule-ref definition
        // or -tr-rule-ref: selector(".AFBaseFont:alias")
        // where you have only one selector in an -tr-rule-ref definition.
        // I want each selector value to be an IncludeStyleNode.

        _addIncludeStyleNodes(value, includeStyleNodes);

      }
    }

    // create a StyleNode
    StyleNode styleNode =
      new StyleNode(name,
                    selector,
                    propertyArray,
                    includeStyleNodes.toArray(new IncludeStyleNode[0]),
                    null,
                    inhibitedProperties);

    styleNodeList.add(styleNode);

  }

  private static SkinPropertyNode _createSkinPropertyNode(
    String selector,
    String name,
    String value)
  {
    // Store the property selector + property Name as the Skin Property Key.
    // e.g., use af|breadCrumbs-tr-show-last-item
    StringBuilder keyBuilder = new StringBuilder(selector.length() + name.length());
    keyBuilder.append(selector);
    keyBuilder.append(name);
    String key = keyBuilder.toString();

    // look up in map to get conversion
    Class<?> type = SkinProperties.PROPERTY_CLASS_TYPE_MAP.get(key);
    Object propValueObj = null;
    if (type != null)
    {
      try
      {
        // coerce the value to the type
        propValueObj = Coercions.coerce(null, value, type);
      }
      catch (IllegalArgumentException ex)
      {
        if (_LOG.isWarning())
          _LOG.warning(ex);
      }
    }

    SkinPropertyNode node = new SkinPropertyNode(key,
                                                 propValueObj != null ? propValueObj : value);
    return node;
  }

  // This is for -tr-rule-ref properties on styles.
  private static void _addIncludeStyleNodes(
    String value,
    List <IncludeStyleNode> includeStyleNodes )
  {

    if (value != null)
    {
      // parse string and create styleNode for each selector value
      // the string will be of this form:
      // selector(".AFBaseFont:alias") selector(".MyDarkBackground")
      // or a single selector:
      // selector(".AFBaseFont:alias")
      // if it ends with :alias, it is a namedstyle.

      List<String> selectors = new ArrayList<String>();

      String[] test = _SELECTOR_PATTERN.split(value);
      for (int i=0; i < test.length; i++)
      {
        int endIndex = test[i].indexOf(")");
        if (endIndex > -1)
        {
          String selectorValue = test[i].substring(0, endIndex);
          selectorValue = trimQuotes(selectorValue);
          selectors.add(selectorValue);
        }
      }

      // now take the selector List and convert it to IncludeStyleNodes.
      int size = selectors.size();

      for (int i=0; i < size; i++)
      {
        String includeStyle = selectors.get(i);
        // if it has :alias at the end it is a named style
        if (includeStyle.endsWith(":alias"))
        {
          // strip off :alias first and the . at the beginning
          
          int endIndex = includeStyle.indexOf(":alias");
          int startIndex = 0;
          if (includeStyle.charAt(0) == '.')
            startIndex = 1;
          includeStyleNodes.add(new IncludeStyleNode(
                                includeStyle.substring(startIndex, endIndex),
                                null));
        }
        else
          includeStyleNodes.add(new IncludeStyleNode(null, includeStyle));
      }

    }
  }

  private static StyleSheetDocument _createStyleSheetDocument(
    ParseContext       context,
    List <StyleSheetNode> ssNodeList)
  {

    long timestamp = _getDocumentTimestamp(context);

    return new StyleSheetDocument(ssNodeList.toArray(new StyleSheetNode[0]),
                                    null,
                                    timestamp);
  }

  // Returns the document timestamp for the style sheet that
  // is currently being parsed, taking into account timestamps
  // of any imported style sheets. (copied from StyleSheetDocumentParser)
  private static long _getDocumentTimestamp(ParseContext parseContext)
  {

    long timestamp = StyleSheetDocument.UNKNOWN_TIMESTAMP;

    // The only way to get the timestamp is through the
    // InputStreamProvider.
    InputStreamProvider provider = XMLUtils.getInputStreamProvider(parseContext);

    if (provider != null)
    {
      // And this only works if we are using a File-based or URL-based InputStream
      Object identifier = provider.getIdentifier();
      if (identifier instanceof File)
        timestamp = ((File)identifier).lastModified();
      else if (identifier instanceof URL)
      {
        try
        {
          timestamp = URLUtils.getLastModified((URL) identifier);
        }
        catch (IOException io)
        {
          _LOG.warning("CANNOT_GET_STYLESHEET_DOCUMENT_TIMESTAMP");
        }

      }
    }

    return timestamp;
  }

/** unused for now. we want to do this for icons, properties and styles at once
  // substitute the prefix (the part that comes before the |) with
  // its namespace
  // e.g., selectorName = af|breadCrumbs
  // af maps to http://myfaces.apache.org/adf/faces
  // return
  // http://myfaces.apache.org/adf/faces|navigationPath
  private static String _getNamespacedSelector(
    Map    namespaceMap,
    String selectorName)
  {

    int barIndex = selectorName.indexOf("|");

    if (barIndex <= 0)
      return selectorName;
    else
    {
      String namespace =
        (String)namespaceMap.get(selectorName.substring(0, barIndex));
      if (namespace == null)
        return selectorName;
      return namespace.concat(selectorName.substring(barIndex));
    }
  }
  **/

  private static List <SkinStyleSheetNode> _parseCSSStyleSheet(Reader reader)
  {
    SkinCSSParser parser = new SkinCSSParser();
    SkinCSSDocumentHandler documentHandler = new SkinCSSDocumentHandler();
    parser.parseCSSDocument(reader, documentHandler);
    return documentHandler.getSkinStyleSheetNodes();
  }


  // Tests whether the specified property value is an "url" property.
  private static boolean _isURLValue(String propertyValue)
  {
    // URL property values start with "url("
    return propertyValue.startsWith("url(");
  }

  // Returns the uri portion of the url property value
  private static String _getURIString(String propertyValue)
  {
    assert(_isURLValue(propertyValue));

    int uriEnd = propertyValue.indexOf(')');
    String uri = propertyValue.substring(4, uriEnd);

    return trimQuotes(uri);
  }


  // returns true if the selectorName indicates that it is an icon.
  private static boolean _isIcon(String selectorName)
  {
    // =-=jmw There is no good way to tell if this is an icon.
    // for now, I look at the selector name.
    // we do have some styles that have -icon- in the name, but it's
    // not at the end which is how icons are determined.
    // our icon names look like .AFWarningIcon:alias
    // AFErrorIconStyle is a style.
    // This supports pseudo-classes on icon definitions (e.g.,
    // foo-icon:hover- or FooIcon:alias:hover)
    // -icon: is a condition because it could be -icon:hover.
    return  (selectorName.endsWith("-icon")  ||
            (selectorName.indexOf("-icon:") > -1) ||
            selectorName.indexOf("Icon:alias") > -1);
  }

  /**
   * Given a String that denotes a width or height css style
   * property, return an Integer. This will strip off 'px' from
   * the string if there is one.
   * e.g., if propertyValue is '7px', the Integer 7 will be returned.
   * @param propertyValue - this is a string that indicates width
   * or height.
   * @return Integer
   */
  private static Integer _convertPxDimensionStringToInteger(
    String propertyValue)
  {
    int pxPosition = propertyValue.indexOf("px");
    if (pxPosition > -1)
      propertyValue = propertyValue.substring(0, pxPosition);
    return Integer.valueOf(propertyValue);
  }
  private static class ResolvedSkinProperties
  {


    ResolvedSkinProperties(
      List<PropertyNode> noTrPropertyList,
      List<String> trRuleRefList,
      Set<String> inhibitedPropertySet,
      List<SkinPropertyNode> skinPropertyNodeList,
      boolean trTextAntialias)
    {
      _noTrPropertyList = noTrPropertyList;
      _trRuleRefList = trRuleRefList;
      _inhibitedPropertySet = inhibitedPropertySet;
      _skinPropertyNodeList = skinPropertyNodeList;
      _trTextAntialias = trTextAntialias;
    }

    public List<PropertyNode> getNoTrPropertyList()
    {
      return _noTrPropertyList;
    }

    public List<String> getTrRuleRefList()
    {
      return _trRuleRefList;
    }

    public List<SkinPropertyNode> getSkinPropertyNodeList()
    {
      return _skinPropertyNodeList;
    }

    public Set<String> getInhibitedProperties()
    {
      return _inhibitedPropertySet;
    }

    public boolean isTrTextAntialias()
    {
      return _trTextAntialias;
    }

    private Set<String>            _inhibitedPropertySet;
    private List<PropertyNode>     _noTrPropertyList;
    private List<String>           _trRuleRefList;
    private List<SkinPropertyNode> _skinPropertyNodeList;
    private boolean                _trTextAntialias;
  }

  // Custom Trinidad css properties:
  //-tr-rule-ref, -tr-inhibit, -tr-text-antialias
  private static final String _TR_PROPERTY_PREFIX = "-tr-";
  // For backwards compatibility, keep the -ora- css properties in
  // addition to the -tr- css properties.
  private static final String _ORA_PROPERTY_PREFIX = "-ora-";
  private static final String _PROPERTY_RULE_REF = "rule-ref";
  private static final String _PROPERTY_INHIBIT = "inhibit";
  private static final String _PROPERTY_TEXT_ANTIALIAS = "text-antialias";
  private static final Pattern _INTEGER_PATTERN = Pattern.compile("\\d+(px)?");

  private static final Pattern _SPACE_PATTERN = Pattern.compile("\\s");
  private static final Pattern _SELECTOR_PATTERN = Pattern.compile("selector\\(");

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SkinStyleSheetParserUtils.class);
}
