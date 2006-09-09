/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.io.InputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.style.CSSStyle;
import org.apache.myfaces.trinidadinternal.style.xml.parse.IncludeStyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;
import org.apache.myfaces.trinidadinternal.skin.icon.TextIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.URIImageIcon;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.SkinPropertyNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleSheetParserUtils.java#0 $) $Date: 10-nov-2005.18:59:00 $
 * @author The Oracle ADF Faces Team
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
  static public Object parseCSSSource(
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
      return cached;


    InputStream stream = provider.openInputStream();

    try
    {

      // Store a resolver relative to the file we're about to parse
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


    /** trim the leading/ending quotes, if any.
   * */
  public static String trimQuotes(String in)
  {
    // strip off the starting/ending quotes if there are any
    char firstChar = in.charAt(0);
    int firstCharIndex = 0;
    if ((firstChar == '\'') || (firstChar == '"'))
      firstCharIndex = 1;

    int length = in.length();
    char lastChar = in.charAt(length-1);
    if ((lastChar == '\'') || (lastChar == '"'))
      length--;

    return in.substring(firstCharIndex, length);
  }

  /**
   * Given a List of StyleSheetNodes, create StyleSheetEntry.
   * A StyleSheetEntry is an object that contains:
   * styleSheetName, StyleSheetDocument, List<IconNode>, List<SkinPropertyNode>
   * A StyleSheetDocument contains StyleSheetNodes. A StyleSheetNode contains
   * a list css style selectors and their properties and additional info like
   * the direction, locale, etc. for this list of selectors.
   * @param context
   * @param sourceName
   * @param styleSheetNodes
   * @return
   */
  private static StyleSheetEntry _createStyleSheetEntry(
    ParseContext  context,
    String        sourceName,
    List <SkinStyleSheetNode> skinSSNodeList
    )
  {

    // Retrieve the base URI for the source style sheet.
    // We need this information so that we can convert relative
    // URLs in the source style sheet (eg. background-image URLs)
    // to absolute URLs.  Otherwise, the relative URLs would
    // be resolved relative to the generated style sheet,
    // which is not the desired behavior.
    String baseURI = _getBaseURI(sourceName);    
    
    // Get each SkinStyleSheetNode, and for each SkinStyleSheetNode get a
    // styleNodeList. Also, build one iconNodeList and one skinPropertyNodeList.
    
    // initialize
    List<IconNode> iconNodeList = new ArrayList<IconNode>();
    List<SkinPropertyNode> skinPropertyNodeList = new ArrayList<SkinPropertyNode>();
    List<StyleSheetNode> ssNodeList = new ArrayList<StyleSheetNode>();
    
    for (SkinStyleSheetNode skinSSNode : skinSSNodeList) 
    {
  
      // selector and its properties
      List <SkinSelectorPropertiesNode> selectorNodeList =
        skinSSNode.getSelectorNodeList();
      //Map namespaceMap = styleSheetNode.getNamespaceMap();
     
      // initialize
      List <StyleNode> styleNodeList = new ArrayList<StyleNode>();
  
      for (SkinSelectorPropertiesNode cssSelector : selectorNodeList) 
      {

        String selectorName = cssSelector.getSelectorName();
        List<PropertyNode> propertyList = cssSelector.getPropertyNodes();
        int direction     = skinSSNode.getDirection();
  
        ResolvedSkinProperties resolvedProperties =
          _resolveProperties(selectorName,
                             propertyList,
                             baseURI,
                             sourceName);
  
        skinPropertyNodeList.addAll(resolvedProperties.getSkinPropertyNodeList());
  
        List<PropertyNode> noOraPropertyList = 
          resolvedProperties.getNoOraPropertyList();
  
        // =-=jmw There is no good way to tell if this is an icon.
        // for now, I look at the selector name.
        // we do have some styles that have -icon- in the name, but it's
        // not at the end which is how icons are determined.
        // our icon names look like .AFWarningIcon:alias
        // AFErrorIconStyle is a style.
        // This supports pseudo-classes on icon definitions (e.g.,
        // foo-icon:hover- or FooIcon:alias:hover)
        // -icon: is a condition because it could be -icon:hover.
        if (selectorName.endsWith("-icon")  ||
            (selectorName.indexOf("-icon:") > -1) ||
            selectorName.indexOf("Icon:alias") > -1)
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
          _addIconNode(selectorName,
                       noOraPropertyList,
                       iconNodeList);
          // log warning if the icon is defined within an @agent or @platform
          // block that tells the user that this icon will be used 
          // for all agents and platforms.
          // TODO add agent and platform support for icons.
          // This means that if an icon is defined within the @agent and/or
          // the @platform keys, then that icon should be rendered when
          // the rendering context matches the agent/platform.
          if (skinSSNode.getAgents() != null ||
              skinSSNode.getPlatforms() != null)
          {
            _LOG.warning("Icon '" +
                         selectorName +
                         "' is defined for agents and/or platforms in the skinning file." +
                         " However that feature is not implemented yet for icons, only styles. "+ " " +
                         "Therefore, this icon will be used " +
                         "regardless of the request's agent or platform.");
          }
        }
        else
        {
          // create a StyleNode object and add it to the styleNodeList.
          _addStyleNode(selectorName,
                        noOraPropertyList,
                        resolvedProperties.getOraRuleRefList(),
                        resolvedProperties.getInhibitedProperties(),
                        resolvedProperties.isOraTextAntialias(),
                        styleNodeList);
  
        }
      }
      
      if (styleNodeList.size() > 0)
      {
        // we need to deal with the styleNodeList by building a StyleSheetNode
        // with this information.
        // create a StyleSheetNode, add to the ssNodeList
        StyleNode[] styleNodeArray = styleNodeList.toArray(new StyleNode[0]);
        StyleSheetNode ssNode = 
          new StyleSheetNode(styleNodeArray,
                             null,/*locales, not yet supported*/
                             skinSSNode.getDirection(),
                             skinSSNode.getAgents(),
                             null,/*versions, not supported*/
                             skinSSNode.getPlatforms(),
                             0);
        ssNodeList.add(ssNode);
      }
 
    } // end for each SkinStyleSheetNode
    
    
    // StyleSheetDocument contains StyleSheetNode[] styleSheets
    StyleSheetDocument ssDocument = 
      _createStyleSheetDocument(context, ssNodeList);

    return new StyleSheetEntry(sourceName,
                               ssDocument,
                               iconNodeList,
                               skinPropertyNodeList);



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
    List<PropertyNode> propertyNodeList,
    String baseURI,
    String sourceName)
  {

    List<PropertyNode> noOraPropertyList = new ArrayList<PropertyNode>();
    List<String> oraRuleRefList = new ArrayList<String>();
    Set<String> inhibitedPropertySet = new TreeSet<String>();
    List<SkinPropertyNode> skinPropertyNodeList = 
      new ArrayList<SkinPropertyNode>();
    
    boolean oraTextAntialias = false;

    // loop through each property in the propertyList
    // and resolve into
    // noOraPropertyList (properties that do not start with -ora-)
    // oraRuleRefList (properties that start with -ora-rule-ref)
    // boolean oraTextAntialias (property value for -ora-text-antialias
    // skinPropertyNodeList (all other properties that start with -ora-)
    // These properties are all stored in th ResolvedSkinProperties inner class.

    for(PropertyNode propertyNode : propertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();
      
      if(propertyName != null && propertyValue != null)
      {
        if(propertyName.startsWith(_ORA_PROPERTY_PREFIX))
        {
          if (propertyName.equals(_ORA_RULE_REF))
          {
            // add the rule ref value to the list
            oraRuleRefList.add(propertyValue);
          }
          else if (propertyName.equals(_ORA_TEXT_ANTIALIAS))
          {
            if ("true".equals(propertyValue))
              oraTextAntialias = true;

          }
          else if (propertyName.equals(_ORA_INHIBIT))
          {
            for (String value : propertyValue.split("\\s"))
            {
              inhibitedPropertySet.add(value);
            }
          }
          else
          {
            SkinPropertyNode node =
              new SkinPropertyNode(selectorName,
                                   propertyName,
                                   propertyValue);
  
            skinPropertyNodeList.add(node);
          }
        }
        else
        {
          if (_containsURL(propertyValue))
          {
            String resolvedUrl = _resolveURL(baseURI,
                                             propertyValue,
                                             sourceName,
                                             selectorName);
            
            propertyNode = new PropertyNode(propertyName, resolvedUrl);
          }
          else if (_URI_PROPERTIES.contains(propertyName))
          { 
            // Make sure it's a legit value for an URL
            if (!_SPECIAL_URI_VALUES.contains(propertyValue))
            {
              // TODO: Add a list of property names expecting an URL here, 
              // "content" maybe?
              _LOG.warning("An url value delimited by url() is expected for " +
                           "the property '" +
                           propertyName + 
                           "' in selector '" +
                           selectorName +
                           "' in style sheet '" +
                           sourceName + 
                           "'. Found: '" + 
                           propertyValue + "'.");
            }
          }

          noOraPropertyList.add(propertyNode);
        }
      }
    }

    return new ResolvedSkinProperties(
      noOraPropertyList,
      oraRuleRefList,
      inhibitedPropertySet,
      skinPropertyNodeList,
      oraTextAntialias);
  }
  
  private static String _resolveURL(
      String baseUrl,
      String url,
      String sourceName,
      String selectorName)
  {
    int endIndex = -1;
    int index = url.indexOf("url(");
    StringBuilder builder = new StringBuilder();
    builder.append(url, 0 , index);
    while(index >= 0)
    {
      // Appends values before url()
      builder.append(url, endIndex + 1, index);
      
      endIndex = url.indexOf(')', index + 3);
      String uri = url.substring(index + 4, endIndex);

      // Trim off 
      int uriLength = uri.length();
      if (uriLength > 0)
      {
        if ((uri.charAt(0) == '\'' && uri.charAt(uriLength - 1) == '\'') ||
            (uri.charAt(0) == '"' && uri.charAt(uriLength - 1) == '"'))
        {
          uri = uri.substring(1, uriLength - 1);
          uriLength = uriLength - 2;
        }
      }


      if(uriLength == 0)
      {
        // url() or url('') found, should not happen.
        _LOG.warning("An empty URL was found in selector '" +
                     selectorName +
                     "' in style sheet '" +
                     sourceName + "'.");
      }
      
      if(uri.charAt(0) == '/')
      {
        // A transformation is required
        if(uriLength > 1 && uri.charAt(1) == '/')
        {
          // Double slashes, trim one and do not add context root before
          builder.append("url(");
          builder.append(uri, 1, uriLength);
          builder.append(')');
        }
        else
        {
          // Single slash, add context path.
          FacesContext facesContext = FacesContext.getCurrentInstance();
          assert(facesContext != null);

          ExternalContext externalContext = facesContext.getExternalContext();
          String contextPath = externalContext.getRequestContextPath();
          builder.append("url(");
          
          assert contextPath.charAt(0) == '/';
          //if(contextPath.charAt(0) != '/')
          //{
          //  // Should not happen, but never too prudent
          //  builder.append('/');
          //}
          
          assert contextPath.charAt(contextPath.length() - 1) != '/';
          //if(contextPath.charAt(contextPath.length() - 1) == '/')
          //{
          //  // Should not happen, but better safe than sorry.
          //  builder.append(contextPath, 0, contextPath.length() - 1);
          //}
          //else
          //{
          builder.append(contextPath);
          //}
          
          builder.append(uri);
          builder.append(')');
        }
      }
      else if(_isRelativeURI(uri))
      {
        // Convert relative URL values to absolute, since
        // relative values will be resolved relative to the
        // generated style sheet, not the source CSS file.
        builder.append(_getAbsoluteURLValue(baseUrl, uri, sourceName, selectorName));
      }
      
      index = url.indexOf("url(", endIndex);
    }
    
    builder.append(url, endIndex + 1, url.length());

    // Don't change anything
    return builder.toString();
  }

  /**
   * Create an IconNode and add it to the iconNodeList.
   * @param selectorName
   * @param propertyNodeList
   * @param oraTextAntialias
   * @param iconNodeList
   */
  private static void _addIconNode(
    String             selectorName,
    List<PropertyNode> noOraPropertyNodeList,
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
    // append all the styles that are not content, width or height into
    // inline style
    CSSStyle inlineStyle = new CSSStyle();

    for(PropertyNode propertyNode : noOraPropertyNodeList)
    {
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();
      if (propertyName.equals("width"))
      {
        // save original width value
        // strip off px from the string and return an Integer
        widthValue = propertyValue;
        width = _convertPxDimensionStringToInteger(widthValue);
      }
      else if (propertyName.equals("height"))
      {
        // save original height value
        // strip off px from the string and return an Integer
        heightValue = propertyValue;
        height = _convertPxDimensionStringToInteger(heightValue);
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
          // -= Simon Lessard =-
          // Hack: URL at this point might already have been resolved.
          //       It will be resolved if the content was specified using 
          //       url(). If so, it need to be unresolved.
          FacesContext    context     = FacesContext.getCurrentInstance();
          ExternalContext eContext    = context.getExternalContext();
          String          contextPath = eContext.getRequestContextPath();
          assert contextPath.charAt(0) == '/';
          assert contextPath.charAt(contextPath.length() - 1) != '/';
          if(uri.startsWith(contextPath))
          {
            uri = uri.substring(contextPath.length() + 1);
          }
          else
          {
            uri = uri.substring(1);
          }
          
          icon =
            new ContextImageIcon(uri, uri, width, height, null, inlineStyle);
        }
        else
        {
          if (startsWithTwoSlashes)
            uri = uri.substring(1);
          icon =
            new URIImageIcon(uri, uri, width, height, null, inlineStyle);
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

  }

  /**
   * Creates a StyleNode object and adds it to the styleNodeList
   * @param selectorName
   * @param propertyNodeList
   * @param oraRuleRefList
   * @param styleNodeList
   */
  private static void _addStyleNode(
    String             selectorName,
    List<PropertyNode> propertyNodeList,
    List<String>       oraRuleRefList,
    Set<String>        inhibitedProperties,
    boolean            oraTextAntialias,
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
    if (oraTextAntialias)
    {
      propertyNodeList.add(new PropertyNode("text-antialias", "true"));
    }
    // convert to a PropertyNode[], because StyleNode takes this type.
    PropertyNode[] propertyArray =
      propertyNodeList.toArray(new PropertyNode[propertyNodeList.size()]);

    // if the oraRuleRefList is not empty, create IncludeStyleNodes.
    int length = oraRuleRefList.size();
    List<IncludeStyleNode> includeStyleNodes = new ArrayList<IncludeStyleNode>();

    if (length > 0)
    {
      for(String value : oraRuleRefList)
      {
        // parse the value, which will be of this form:
        // -ora-rule-ref: selector(".AFBaseFont:alias") selector(".Foo")
        // where you have more than one selector in an -ora-rule-ref definition
        // or -ora-rule-ref: selector(".AFBaseFont:alias")
        // where you have only one selector in an -ora-rule-ref definition.
        // I want each selector value to be an IncludeStyleNode.

        _addIncludeStyleNodes(value, includeStyleNodes);

      }
    }

    // create a StyleNode
    StyleNode styleNode =
      new StyleNode(name,
                    selector,
                    propertyArray,
                    null,
                    includeStyleNodes.toArray(new IncludeStyleNode[0]),
                    null,
                    inhibitedProperties);

    styleNodeList.add(styleNode);

  }

  // This is for -ora-rule-ref properties on styles.
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
      String[] test = value.split("selector\\(");
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
      // And this only works if we are using a File-based InputStream
      Object identifier = provider.getIdentifier();
      if (identifier instanceof File)
        timestamp = ((File)identifier).lastModified();
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

  // Given the sourceName of the style sheet that is being
  // parsed, return an absolute base URI pointing to the
  // directory which contains the style sheet.  We will use this
  // base URI to ensure that URLs specified in the style sheet
  // (eg. background-image URLs) are resolved appropriately
  // (ie. not relative to the generated style sheet).
  private static String _getBaseURI(String sourceName)
  {
    // The sourceName is actually a context-relative URI.
    // We need to strip off the file name and prepend the
    // context path.

    // First, get the context path.
    // Note that our options for obtaining the context path at
    // this point are somewhat limited.  We could require that
    // the caller pass this in, though this would require
    // passing this information through many layers for non-obvious
    // reasons.  Instead, we rely on the fact that we can obtain
    // this information through the FacesContext (actually,
    // through the ExternalContext), which we have access to here.
    // This is slightly ugly, since this introduces a dependency
    // from our CSS parsing code on JavaServer Faces, but until
    // we find use cases where our skinning architecture will be
    // applied in a non-Faces environment, this dependency seems
    // accpetable.
    FacesContext facesContext = FacesContext.getCurrentInstance();
    assert(facesContext != null);

    ExternalContext externalContext = facesContext.getExternalContext();
    String contextPath = externalContext.getRequestContextPath();
    assert(contextPath != null);

    int contextPathLength = contextPath.length();

    // Before we combine the context path and source name to
    // produce the base URI, make sure that these values are
    // in the expected form
    assert(contextPathLength > 0);
    assert(contextPath.charAt(0) == '/');
    assert(contextPath.charAt(contextPathLength - 1) != '/');
    assert(sourceName.length() > 0);
    assert(sourceName.charAt(0) != '/');

    // Our internal css files are under /META-INF/adf/styles, though
    // images are accessed via <contextPath>/adf/images.  As such,
    // we also need to strip off the bonus "/META_INF" prefix from
    // the source name.  Otherwise, image requests won't be
    // resovled since /META-INF is not exposed via HTTP.
    if (sourceName.startsWith("META-INF/"))
      sourceName = sourceName.substring(9);

    // Find the start of the file name part of the source name - we don't
    // need this as part of the base URI
    int lastSepIndex = sourceName.lastIndexOf('/');
    
    if (lastSepIndex == -1)
      return contextPath;
    else
    {
      StringBuilder buffer = new StringBuilder(
                                    contextPathLength + lastSepIndex + 1);
      buffer.append(contextPath);
      buffer.append("/");
      buffer.append(sourceName.substring(0, lastSepIndex));
      return buffer.toString();
    }
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


  // Tests whether the specified uri is relative
  private static boolean _isRelativeURI(String uri)
  {
    return ((uri.charAt(0) != '/') && (uri.indexOf(':') < 0));
  }

  // Returns an absolute "url()" property value for the specified uri
  private static String _getAbsoluteURLValue(
    String baseURI,
    String uri,
    String sourceName,
    String selectorName)
  {
    String strippedURI = uri;
    String strippedBaseURI = baseURI;

    // Strip off leading "../" segments from the uri
    while (strippedURI.startsWith("../"))
    {
      int lastSepIndex = strippedBaseURI.lastIndexOf('/');
      if (lastSepIndex < 0)
      {
        _LOG.warning("Invalid image uri '" +
                     uri +
                     "' in selector '" +
                     selectorName +
                     "' in style sheet '" +
                     sourceName);

        break;
      }

      strippedURI = strippedURI.substring(3);
      strippedBaseURI = strippedBaseURI.substring(0, lastSepIndex);
    }

    StringBuilder buffer = new StringBuilder(strippedBaseURI.length() +
                                             strippedURI.length() +
                                             8);

    buffer.append("url('");
    buffer.append(strippedBaseURI);
    buffer.append("/");
    buffer.append(strippedURI);
    buffer.append("')");

    return buffer.toString();
  }
  
  /**
   * Determines if the specified value contains a CSS url. The URLs are
   * detected but finding usage of url() function.
   * 
   * @param value
   * 
   * @return <code>true</code> if the specified value contains an URL, 
   *         <code>false</code> otherwise.
   */
  private static boolean _containsURL(String value)
  {
    if(value == null)
    {
      return false;
    }
    
    return value.indexOf("url(") >= 0;
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
      List<PropertyNode> noOraPropertyList,
      List<String> oraRuleRefList,
      Set<String> inhibitedPropertySet,
      List<SkinPropertyNode> skinPropertyNodeList,
      boolean oraTextAntialias)
    {
      _noOraPropertyList = noOraPropertyList;
      _oraRuleRefList = oraRuleRefList;
      _inhibitedPropertySet = inhibitedPropertySet;
      _skinPropertyNodeList = skinPropertyNodeList;
      _oraTextAntialias = oraTextAntialias;
    }

    public List<PropertyNode> getNoOraPropertyList()
    {
      return _noOraPropertyList;
    }

    public List<String> getOraRuleRefList()
    {
      return _oraRuleRefList;
    }

    public List<SkinPropertyNode> getSkinPropertyNodeList()
    {
      return _skinPropertyNodeList;
    }
    
    public Set<String> getInhibitedProperties()
    {
      return _inhibitedPropertySet;
    }

    public boolean isOraTextAntialias()
    {
      return _oraTextAntialias;
    }
    
    private Set<String>            _inhibitedPropertySet;
    private List<PropertyNode>     _noOraPropertyList;
    private List<String>           _oraRuleRefList;
    private List<SkinPropertyNode> _skinPropertyNodeList;
    private boolean                _oraTextAntialias;
  }


  private static final String _ORA_PROPERTY_PREFIX = "-ora-";
  private static final String _ORA_RULE_REF = "-ora-rule-ref";
  private static final String _ORA_INHIBIT = "-ora-inhibit";
  private static final String _ORA_TEXT_ANTIALIAS = "-ora-text-antialias";

  // Set of values that are legal for url() values
  private static final Set<String> _URI_PROPERTIES = new HashSet<String>();
  static
  {
    _URI_PROPERTIES.add("background-image");
    _URI_PROPERTIES.add("cue-after");
    _URI_PROPERTIES.add("cue-before");
    _URI_PROPERTIES.add("list-style-image");
  }

  // Set of values that are legal for url() values
  private static final Set<String> _SPECIAL_URI_VALUES = new HashSet<String>();
  static
  {
    _SPECIAL_URI_VALUES.add("none");
    _SPECIAL_URI_VALUES.add("inherit");
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    SkinStyleSheetParserUtils.class);
}
