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

package org.apache.myfaces.adfinternal.skin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.share.io.InputStreamProvider;
import org.apache.myfaces.adfinternal.share.io.NameResolver;
import org.apache.myfaces.adfinternal.share.xml.ParseContext;
import org.apache.myfaces.adfinternal.share.xml.XMLUtils;

import org.apache.myfaces.adfinternal.skin.icon.NullIcon;
import org.apache.myfaces.adfinternal.style.CSSStyle;
import org.apache.myfaces.adfinternal.style.xml.parse.IncludeStyleNode;
import org.apache.myfaces.adfinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.adfinternal.style.xml.parse.StyleNode;
import org.apache.myfaces.adfinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.adfinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.adfinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.adfinternal.skin.icon.Icon;
import org.apache.myfaces.adfinternal.skin.icon.TextIcon;
import org.apache.myfaces.adfinternal.skin.icon.URIImageIcon;
import org.apache.myfaces.adfinternal.style.util.StyleUtils;
import org.apache.myfaces.adfinternal.ui.laf.xml.parse.IconNode;
import org.apache.myfaces.adfinternal.ui.laf.xml.parse.SkinPropertyNode;
import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;

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
    Class         expectedType) throws IOException
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
    List <IconNode> iconNodeList = new ArrayList();
    List skinPropertyNodeList = new ArrayList();
    List <StyleSheetNode> ssNodeList = new ArrayList();
    
    for (SkinStyleSheetNode skinSSNode : skinSSNodeList) 
    {
  
      // selector and its properties
      List <SkinSelectorPropertiesNode> selectorNodeList =
        skinSSNode.getSelectorNodeList();
      //Map namespaceMap = styleSheetNode.getNamespaceMap();
     
      // initialize
      List <StyleNode> styleNodeList = new ArrayList();
  
      for (SkinSelectorPropertiesNode cssSelector : selectorNodeList) 
      {

        String selectorName = cssSelector.getSelectorName();
        List propertyList = cssSelector.getPropertyNodes();
        int direction     = skinSSNode.getDirection();
  
        ResolvedSkinProperties resolvedProperties =
          _resolveProperties(selectorName,
                             propertyList,
                             baseURI,
                             sourceName);
  
        skinPropertyNodeList.addAll(resolvedProperties.getSkinPropertyNodeList());
  
        List noOraPropertyList = resolvedProperties.getNoOraPropertyList();
  
        // =-=jmw There is no good way to tell if this is an icon.
        // for now, I look at the selector name.
        // we do have some styles that have -icon- in the name, but it's
        // not at the end which is how icons are determined.
        // our icon names look like .AFWarningIcon:alias
        // AFErrorIconStyle is a style.
        if (selectorName.endsWith("-icon")  ||
            selectorName.endsWith("Icon:alias"))
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
        }
        else
        {
          // create a StyleNode object and add it to the styleNodeList.
          _addStyleNode(selectorName,
                        noOraPropertyList,
                        resolvedProperties.getOraRuleRefList(),
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
                             null,
                             skinSSNode.getDirection(),
                             null,
                             null,
                             null,
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
    List   propertyNodeList,
    String baseURI,
    String sourceName)
  {

    List   noOraPropertyList = new ArrayList();
    List   oraRuleRefList = new ArrayList();
    List   skinPropertyNodeList = new ArrayList();
    boolean oraTextAntialias = false;

    // loop through each property in the propertyList
    // and resolve into
    // noOraPropertyList (properties that do not start with -ora-)
    // oraRuleRefList (properties that start with -ora-rule-ref)
    // boolean oraTextAntialias (property value for -ora-text-antialias
    // skinPropertyNodeList (all other properties that start with -ora-)
    // These properties are all stored in th ResolvedSkinProperties inner class.

    Iterator propertyIter = propertyNodeList.listIterator();

    while (propertyIter.hasNext())
    {
      PropertyNode propertyNode = (PropertyNode)propertyIter.next();
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();

      if ((propertyName != null) &&
          (propertyName.startsWith(_ORA_PROPERTY_PREFIX)))
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
        if ("background-image".equals(propertyName) &&
              _isURLValue(propertyValue))
        {
          // Convert relative URL values to absolute, since
          // relative values will be resolved relative to the
          // generated style sheet, not the source CSS file.
          String uri = _getURIString(propertyValue);
          if (_isRelativeURI(uri))
          {
            String absoluteURI = _getAbsoluteURLValue(baseURI,
                                                      uri,
                                                      sourceName,
                                                      selectorName);
            propertyNode = new PropertyNode("background-image", absoluteURI);
          }
        }

        noOraPropertyList.add(propertyNode);
      }

    }

    return new ResolvedSkinProperties(
      noOraPropertyList,
      oraRuleRefList,
      skinPropertyNodeList,
      oraTextAntialias);
  }

  /**
   * Create an IconNode and add it to the iconNodeList.
   * @param selectorName
   * @param propertyNodeList
   * @param oraTextAntialias
   * @param iconNodeList
   */
  private static void _addIconNode(
    String  selectorName,
    List    noOraPropertyNodeList,
    List <IconNode>    iconNodeList)
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
    // af|navigationPath::separatorIcon {content: ">"}
    // af|navigationPath::separatorIcon:rtl {content: "<"}
    // this will create
    // key=af|navigationPath::separatorIcon with TextIcon(">", ">", style, inlineStyle)
    // and
    // key=af|navigationPath::separatorIcon:rtl with TextIcon("<", "<", rtlstyle, rtlinlineStyle)
    // then when I go to get the icon af|navigationPath::separatorIcon, the skin
    // will know to ask for af|navigationPath::separatorIcon:rtl or af|navigationPath::separatorIcon
    // depending upon the DIRECTION that is set on the context.
    // The current Icon classes code will not have to change.


    Integer width = null;
    Integer height = null;
    //String  styleClass = null;
    String  uri = null;
    String  text = null;
    boolean isNullIcon = false;
    // append all the styles that are not content, width or height into
    // inline style
    CSSStyle inlineStyle = new CSSStyle();

    Iterator propertyIter = noOraPropertyNodeList.listIterator();
    while (propertyIter.hasNext())
    {
      PropertyNode propertyNode = (PropertyNode)propertyIter.next();
      String propertyName = propertyNode.getName();
      String propertyValue = propertyNode.getValue();
      if (propertyName.equals("width"))
      {
        int pxPosition = propertyValue.indexOf("px");
        if (pxPosition > -1)
          propertyValue = propertyValue.substring(0, pxPosition);
        width = Integer.valueOf(propertyValue);
      }
      else if (propertyName.equals("height"))
      {
        int pxPosition = propertyValue.indexOf("px");
        if (pxPosition > -1)
          propertyValue = propertyValue.substring(0, pxPosition);
        height = Integer.valueOf(propertyValue);
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
    String  selectorName,
    List    propertyNodeList,
    List    oraRuleRefList,
    boolean oraTextAntialias,
    List    <StyleNode> styleNodeList)
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
      (PropertyNode[])(propertyNodeList.toArray(
                        new PropertyNode[propertyNodeList.size()]));

    // if the oraRuleRefList is not empty, create IncludeStyleNodes.
    int length = oraRuleRefList.size();
    List <IncludeStyleNode> includeStyleNodes = new ArrayList();

    if (length > 0)
    {
      Iterator oraRuleRefIter = oraRuleRefList.listIterator();

      while (oraRuleRefIter.hasNext())
      {
        String value = (String)oraRuleRefIter.next();

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
                    null);

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


      List <String> selectors = new ArrayList();
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
        String includeStyle = (String)selectors.get(i);
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
  // e.g., selectorName = af|navigationPath
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

    StringBuffer buffer = new StringBuffer(contextPathLength + lastSepIndex + 1);
    buffer.append(contextPath);
    buffer.append("/");
    buffer.append(sourceName.substring(0, lastSepIndex));

    return buffer.toString();
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

    StringBuffer buffer = new StringBuffer(strippedBaseURI.length() +
                                           strippedURI.length() +
                                           6);

    buffer.append("url(");
    buffer.append(strippedBaseURI);
    buffer.append("/");
    buffer.append(strippedURI);
    buffer.append(")");

    return buffer.toString();
  }

  private static class ResolvedSkinProperties
  {


    ResolvedSkinProperties(
      List noOraPropertyList,
      List oraRuleRefList,
      List skinPropertyNodeList,
      boolean oraTextAntialias)
    {
      _noOraPropertyList = noOraPropertyList;
      _oraRuleRefList = oraRuleRefList;
      _skinPropertyNodeList = skinPropertyNodeList;
      _oraTextAntialias = oraTextAntialias;
    }

    public List getNoOraPropertyList()
    {
      return _noOraPropertyList;
    }

    public List getOraRuleRefList()
    {
      return _oraRuleRefList;
    }

    public List getSkinPropertyNodeList()
    {
      return _skinPropertyNodeList;
    }

    public boolean isOraTextAntialias()
    {
      return _oraTextAntialias;
    }

    private List    _noOraPropertyList;
    private List    _oraRuleRefList;
    private List    _skinPropertyNodeList;
    private boolean _oraTextAntialias;
  }


  private static final String _ORA_PROPERTY_PREFIX = "-ora-";
  private static final String _ORA_RULE_REF = "-ora-rule-ref";
  private static final String _ORA_TEXT_ANTIALIAS = "-ora-text-antialias";

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(
    SkinStyleSheetParserUtils.class);
}
