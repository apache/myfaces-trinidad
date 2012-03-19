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

package org.apache.myfaces.trinidadinternal.style.util;

import java.util.Arrays;
import java.util.Collection;

import java.util.Collections;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;

import java.util.TreeSet;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.util.Range;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent.Application;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils.StyleSheetVisitor;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * This class serves one purpose: it provides a replacement for NameUtils.getContextName()
 * that produces "stable" file names.  This ensures that generated style sheets will be
 * named in a consistent manner that is not dependent on the order in which requests arrives.
 * 
 * With NameUtils.getContextName(), a skin that contains the following locale-specific
 * definition:
 * 
 * @locale ja, cz, ko { ... }
 * 
 * May share a generated .css file across Japanese, Chinese and Korean end users.  The name
 * of this generated style sheet depends on which locale is specified when the style sheet is
 * first generated.  For example, if the style sheet is generated in response from a request
 * for a Korean-locale user, the file name will include the "ko" locale token.  This file
 * will then be shared by Japanese, Chinese and Korean end users.  However, if the server is
 * subsequently bounced and the first post-bounce request is from a Japanese end user, the file
 * will be re-generated with the "ja" token.
 * 
 * As a result of this behavior, file names are not predictable across runs.  The lack of
 * stable file names makes it difficult/impossible to support use cases where we want to
 * share generated style sheets across applications - eg. by pregenerating style sheets
 * and deploying them to some common server (or a CDN).
 * 
 * StableNameUtils.getContextName() solves this problem by producing names for generated
 * style sheet files that are not dependent on request ordering.
 * 
 * The file names produced by StableNameUtils.getContextName() are broken up into three
 * major sections:
 * 
 * 1. The skin identifier.
 * 2. Variant identifiers (eg. agent, locale, etc...)
 * 3. Contextual identifiers (eg. portlet vs. servlet)
 * 
 * These major sections are separated by double dashes ("--").
 * 
 * Within each major section, minor subsections appear in a predictable order, separated
 * by single dashs ("-").
 * 
 * The skin identifier section (section #1) contains the following subsections:
 * 
 *   <id>-<hash>
 *   
 * Where:
 * 
 * - id: the skin id.  Note that the skin id itself may include dash characters
 *     (eg. "simple-desktop").
 * - hash: a hash of the skin contents
 * 
 * The variant identifiers section (section #2) contains the following subsections:
 * 
 *   <platform>-<agent>-<version>-<locale>-<direction>-<accessibility>
 * 
 * Where:
 * 
 * - platform: the platform name (eg. linux, windows, iphone)
 * - agent: the agent name (eg. ie, gecko, safari)
 * - version: the agent version (eg. 7, 1.9.2, 534)
 * - locale: the locale (eg. en, en_US, ja).
 * - direction: the reading direction (ltr|rtl)
 * - accessibility: accessibility profile preferences (hc|lf|hclf for high contrast|
 *     large fonts|high contrast + large fonts).
 *     
 * In the event that no @-rule variant is expicitly matched, only default styles
 * (ie. styles that apply to all requests) are included in the generated style sheet.
 * The token "d" is used to identify this case.  For example, if the skin does not
 * define any @locale rules, the locale portion of the file name will be "d".
 * 
 * In some cases it is not possible to determine a unique value
 * for a particular variant, because only @-rules that specify multiple values
 * are matched.  In this case, a combination of the matched values will be used
 * for the segment.  For example, if the
 * skin defines a single @locale rule matching the ja, cz, and ko locales, the 
 * locale portion of the file name for generated style sheets corresponding to these
 * locales will be "ca_ja_ko".
 * 
 * The contextual identifiers section (section #3) contains the following subsections:
 * 
 * - container type: identifies whether the application is hosted within a servlet or
 *     portlet (s|p).
 * - request type: identifies whether the style sheet is used for a secure or non-secure
 *     request (s|n).
 * - style class type: identifies whether the style sheet is generated with compressed or
 *     uncompressed style classes (c|u).
 * 
 * In case where skins are pregenerated and shared across applications, it may be necessary
 * to write regular expressions against the above format in order to rewrite style sheet
 * uris to point to some other host.  When doing so, the following recommendations should
 * be observed:
 * 
 * - The identifier section contains a variable number of subsection separators: skin
 *   ids may contain dashes.  As such, when writing regular expressions that target items
 *   in the subsequent major sections, it is best to anchor these regular expressions
 *   to the major section separators (double-dash).
 *   
 * - In the future it is possible that new subsections may need to be added to the
 *   variant or contextual identifier sections.  If the need for additional
 *   information arises, new subsections will be added at the end of the appropriate
 *   major section.  As such, regular expressions should be written in a way that they
 *   can tolerate new subsections later appearing at the end of each major section.
 */
public final class StableNameUtils
{
  /**
   * Similar to NameUtils.getContextName(), but produces stable names.
   * 
   * @param context the current style context
   * @param document the skin document
   * @return a name suitable for inclusion in a generated style sheet file name.
   */
  public static String getContextName(
    StyleContext       context,
    StyleSheetDocument document
    )
  {
    return new ContextNameProducer(context, document).getName();
  }
  
  // Helper class that produces context-specific file names.
  private static final class ContextNameProducer
  {
    // The ContextNameProducer implementation is designed for modularity/code 
    // cleanliness more than performance.  Each name that is produced generates
    // a fairly large number of temporary objects (most iterators).  This seems
    // acceptable as we only generate each name once per generated file, when the
    // file is initially written.  If performance/temporary object creation
    // ever become an issue, we can optimize/refactor.

    public ContextNameProducer(
      StyleContext       context,
      StyleSheetDocument document
      )
    {
      _context = context;
      _documentId = document.getDocumentId(context);
      _styleSheets = document.getStyleSheetsAsCollection(context);
    }

    /**
     * Returns the context-specific file name.
     */
    public String getName()
    {
      // We're going to collect all required info (eg. platform, agent,
      // locale, etc...) in a single StyleSheetVisitUtils.visit(), during
      // which we delegate to a number of "naming" StyleSheetVisitors
      // to collect the info that we need to produce a name.
      Collection<NamingStyleSheetVisitor> visitors = _getVisitors();

      _visitStyleSheets(visitors);

      return _toName(visitors);
    }

    private Collection<NamingStyleSheetVisitor> _getVisitors()
    {
      return Arrays.<NamingStyleSheetVisitor>asList(
               new PlatformNameExtractor(),
               new ApplicationNameExtractor(_context.getAgent().getVersion()),
               new LocaleNameExtractor(),
               new DirectionNameExtractor(),
               new AccessibilityNameExtractor());
    }

    private void _visitStyleSheets(Collection<? extends StyleSheetVisitor> visitors)
    {
      StyleSheetVisitor compoundVisitor = StyleSheetVisitUtils.compoundStyleSheetVisitor(visitors);
      StyleSheetVisitUtils.visitStyleSheets(_styleSheets, compoundVisitor);      
    }

    private String _toName(Collection<NamingStyleSheetVisitor> visitors)
    {
      // Picking a slightly large initial size in the hopes that this will
      // avoid reallocations.
      StringBuilder builder = new StringBuilder(100);
      
      _appendDocumentName(builder);
      _appendVisitNames(builder, visitors);
      _appendNonVisitNames(builder);
      _appendSuffix(builder);
      
      return builder.toString();
    }

    private void _appendDocumentName(StringBuilder builder)
    {
      builder.append(_documentId);
      
      // Double-separator to make it easier to write regular
      // expressions against the interesting portion of the name
      // (eg. for uri rewriting purposes).
      builder.append(_SEPARATOR);
      builder.append(_SEPARATOR);      
    }
    
    private void _appendVisitNames(
      StringBuilder builder,
      Collection<NamingStyleSheetVisitor> visitors
      )
    {
      for (NamingStyleSheetVisitor visitor : visitors)
      {
        visitor.appendName(builder);
        builder.append(_SEPARATOR);
      }
      
      // Another double-dash separator to aid regular expression
      // targeting.  This may come in handy if we ever end up
      // introducing new variants that we want to add to the
      // visit-based names section.
      builder.append(_SEPARATOR);
    }

    private void _appendNonVisitNames(StringBuilder builder)
    {
      builder.append(_context.isPortletMode() ? "p" : "s");
      builder.append(_SEPARATOR);
      builder.append(_context.isRequestSecure() ? "s" : "n");
      builder.append(_SEPARATOR);
      builder.append(_context.isDisableStyleCompression() ? "u" : "c");
    }
    
    private void _appendSuffix(StringBuilder builder)
    {
      builder.append(".css");
    }

    private final StyleContext               _context;
    private final String                     _documentId;
    private final Collection<StyleSheetNode> _styleSheets;    
  }
  
  // Extension of StyleSheetVisitor that contributes to name generation
  private interface NamingStyleSheetVisitor extends StyleSheetVisitor
  {
    /**
     * Appends the the file name that is being built up in the specified
     * StringBuilder.
     */
    public void appendName(StringBuilder builder);
  }

  // Abstract base class for NamingStyleSheetVisitors that operate on the 
  // collections coughed up by StyleSheetNode.  Handles three possible
  // scenarios:
  //
  // 1. There is no explicit match - eg. the skin does not define any @locale
  //    rules, so StyleSheetNode.getLocales() is always empty.  In this case,
  //    we always append the default match path token.
  // 2. There is a single match - eg. the skin defines "@locale ja" and this
  //    StyleSheetNode is visited.  In this case, we append the string representation
  //    of the matched value.
  // 3. There is a multiple match (and no single match) - eg. the skin defines
  //    "@locale ja, cz, ko" and this StyleSheetNode is visited.  In this case,
  //    we append a concatenation of the string representation of each value,
  //    separated by the underscore character.
  private abstract static class CollectionNameExtractor<E> implements NamingStyleSheetVisitor
  {
    /**
     * Returns the collection of values that contribute to the name.
     */
    abstract protected Collection<E> getCollectionValues(StyleSheetNode styleSheet);
    
    @Override
    public void visit(StyleSheetNode styleSheet)
    {
      Collection<E> newValues = getCollectionValues(styleSheet);
      if (!newValues.isEmpty())
      {
        if (_values == null)
        {
          // Need to make a copy since we'll be modifying this collection.
          _values = new HashSet<E>(newValues);
        }
        else
        {
          mergeValues(styleSheet, _values, newValues);

          // We should never reach a state where the collected values
          // is empty.  This would indicate that our style sheet node
          // matching logic is wrong - eg. we matched a style sheet
          // node with "@locale ja" and a second style sheet node with
          // "@locale ko".  Make some noise if we hit this.
          if (_values.isEmpty())
          {
            _fail();
          }
        }
      }
    }

    /**
     * Merges previously collected values with a new collection of values,
     * storing the result in oldValues.
     * 
     * By default, the intersection of the two collections is retained.
     */
    protected void mergeValues(
      StyleSheetNode styleSheet,
      Collection<E>  oldValues,
      Collection<E>  newValues
      )
    {
      oldValues.retainAll(newValues);
    }

    @Override
    public void appendName(StringBuilder builder)
    {
      if (_values == null)
      {
        builder.append(_DEFAULT_PATH_TOKEN);
      }
      else
      {
        assert(_values != null);
        assert(!_values.isEmpty());
        
        Collection<String> names = _toNames(_values);
        _appendNames(builder, names);
      }
    }
    
    private Collection<String> _toNames(Collection<E> values)
    {
      Collection<String> names = new TreeSet<String>();
      
      for (E value : values)
      {
        String name = toName(value);
        assert(name != null);
        
        names.add(name);
      }
      
      return names;
    }

    /**
     * Converts the specified value to a name to include in the 
     * file name.
     */
    protected String toName(E value)
    {
      return value.toString();
    }

    private void _appendNames(StringBuilder builder, Collection<String> names)
    {
      assert(names != null);
      assert(!names.isEmpty());
      
      Iterator<String> iter = names.iterator();
      while (iter.hasNext())
      {
        builder.append(iter.next());
        
        if (iter.hasNext())
        {
          builder.append("_");
        }
      }
    }
    
    private Collection<E> _values = null;
  }

  // NamingStyleSheetVisitor that extracts the platform name.
  private static final class PlatformNameExtractor extends CollectionNameExtractor<Integer>
  {
    @Override
    protected Collection<Integer> getCollectionValues(StyleSheetNode styleSheet)
    {
      return styleSheet.getPlatforms();
    }
    
    @Override
    protected String toName(Integer platform)
    {
      return NameUtils.getPlatformName(platform);
    }
  }

  // NamingStyleSheetVisitor that extracts the agent application name
  private static class ApplicationNameExtractor extends CollectionNameExtractor<Application>
  {
    public ApplicationNameExtractor(Version agentVersion)
    {
      assert(agentVersion != null);

      _agentVersion = agentVersion;
      _matchedVersions = Version.ALL_VERSIONS;
    }

    @Override
    protected String toName(Application agentApplication)
    {
      return agentApplication.getAgentName();
    }

    @Override
    protected Collection<Application> getCollectionValues(StyleSheetNode styleSheet)
    {
      AgentAtRuleMatcher agentMatcher = styleSheet.getAgentMatcher();
      if (agentMatcher == null)
      {
        return Collections.emptySet();
      }
      
      return agentMatcher.getAllApplications();
    }

    @Override
    protected void mergeValues(
      StyleSheetNode          styleSheet,
      Collection<Application> oldAgentApplications,
      Collection<Application> newAgentApplications
      )
    {
      super.mergeValues(styleSheet, oldAgentApplications, newAgentApplications);

      // In addition to merging application values, we also need to keep track of
      // versions that we have seen, since this may be included in the generated
      // file name.  Note that we only care about versions for cases where we've
      // got an exact agent application match (ie. in cases where we actually know
      // which application we are targeted.)
      Application newAgentApplication = _getSingleAgentApplication(newAgentApplications);
      if (newAgentApplication != null)
      {
        _mergeVersions(styleSheet.getAgentMatcher(), newAgentApplication);
      }
    }

    // If the collection contains a single entry, returns the single value.
    // Otherwise, returns null;
    private Application _getSingleAgentApplication(Collection<Application> agentApplications)
    {
      if (agentApplications.size() == 1)
      {
         Iterator<Application> iter = agentApplications.iterator();
         if (iter.hasNext())
         {
           return iter.next();
         }
      }
      
      return null;
    }

    private void _mergeVersions(
      AgentAtRuleMatcher agentMatcher,
      Application        agentApplication
      )
    {
      assert(agentMatcher != null);
      assert(agentApplication != null);

      Range<Version> matchedVersions =
        agentMatcher.getMatchedVersionsForApplication(agentApplication, _agentVersion);
      _matchedVersions = _matchedVersions.intersect(matchedVersions);

      // We should never see an empty range at this point since the two
      // ranges that we intersected both contains _agentVersion.
      if (_matchedVersions.isEmpty())
      {
        _fail();
      }
    }

    @Override
    public void appendName(StringBuilder builder)
    {
      // This super call will handle appending the agent name segment.
      // However, we also need to ensure that the agent version is added.
      super.appendName(builder);
      
      // Tack on a value for the version field.  (All stable names need to
      // have the same # of fields in order to support allow uris/names to
      // be easily targeted by uri rewriting regular expressions.)
      builder.append(_SEPARATOR);
      _appendVersionName(builder);      
    }

    private void _appendVersionName(StringBuilder builder)
    {
      Version startVersion = _matchedVersions.getStart();
      Version endVersion = _matchedVersions.getEnd();
      boolean startMin = startVersion.equals(Version.MIN_VERSION);
      boolean endMax = endVersion.equals(Version.MAX_VERSION);
      
      if (startMin && endMax)
      {
        builder.append(_DEFAULT_PATH_TOKEN);
      }
      else if (startMin)
      {
        builder.append(endVersion);
        builder.append("m");
      }
      else if (endMax)
      {
        builder.append(startVersion);
        builder.append("p");
      }
      else
      {
        builder.append(startVersion);
      }
    }

    private final Version  _agentVersion;
    private Range<Version> _matchedVersions;
  }

  // NamingStyleSheetVisitor that extracts the locale name.
  private static final class LocaleNameExtractor extends CollectionNameExtractor<Locale>
  {
    @Override
    protected Collection<Locale> getCollectionValues(StyleSheetNode styleSheet)
    {
      return styleSheet.getLocales();
    }

    @Override
    protected void mergeValues(
      StyleSheetNode     styleSheet,
      Collection<Locale> oldLocales,
      Collection<Locale> newLocales
      )
    {
      assert(oldLocales != null);
      assert(newLocales != null);

      // We can't simply use Collection.retainAll() because we need to
      // compensate for partial locales - ie. if "ja" is present in
      // oldLocales and we encounter "ja_JP", Collection.retainAll()
      // would result in the empty set, where as we want to replace "ja"
      // with "ja_JP".
      
      for (Locale newLocale : newLocales)
      {
        _retainLocale(oldLocales, newLocale); 
      }
    }
    
    // We only want to retain the new locale if:
    //
    // a) it exist in the old locales, or...
    // b) one of its "super" locales exists in the old locales.
    //
    // In the case of b), we want to remove the super-locale and
    // repalce it with the new, more-specific locale.
    private void _retainLocale(Collection<Locale> oldLocales, Locale newLocale)
    {
      // We could optimize by explicitly checking whether the new locale actually
      // specifies a country/variant, but prefer to keep the code simple.
      Locale langOnlyLocale = _toLanguageOnlyLocale(newLocale);
      Locale langAndCountryLocale = _toLanguageAndCountryLocale(newLocale);
      
      if (oldLocales.contains(langOnlyLocale) || oldLocales.contains(langAndCountryLocale))
      {
        oldLocales.remove(langOnlyLocale);
        oldLocales.remove(langAndCountryLocale);
        oldLocales.add(newLocale);
      }
    }
    
    private Locale _toLanguageOnlyLocale(Locale locale)
    {
      return new Locale(locale.getLanguage());
    }

    private Locale _toLanguageAndCountryLocale(Locale locale)
    {
      return new Locale(locale.getLanguage(), locale.getCountry());
    }
  }
  
  // NamingStyleSheetVisitor that extracts the direction name
  private static class DirectionNameExtractor implements NamingStyleSheetVisitor
  {
    @Override
    public void visit(StyleSheetNode styleSheet)
    {
      int direction = styleSheet.getReadingDirection();   
      if (direction != LocaleUtils.DIRECTION_DEFAULT)
      {
        assert((_direction == LocaleUtils.DIRECTION_DEFAULT) || (_direction == direction));
        _direction = direction;
      }
    }

    @Override
    public void appendName(StringBuilder builder)
    {
      String name = (_direction == LocaleUtils.DIRECTION_DEFAULT) ? 
                      _DEFAULT_PATH_TOKEN : 
                      NameUtils.getDirectionName(_direction);
      
      builder.append(name);
    }
    
    private int _direction = LocaleUtils.DIRECTION_DEFAULT;
  }

  // NamingStyleSheetVisitor that extracts the accessibility name
  private static final class AccessibilityNameExtractor extends CollectionNameExtractor<String>
  {
    @Override
    protected Collection<String> getCollectionValues(StyleSheetNode styleSheet)
    {
      return styleSheet.getAccessibilityProperties();
    }
    
    protected void mergeValues(
      StyleSheetNode styleSheet,
      Collection<String> oldAccessibilityProperties,
      Collection<String> newAccessibilityProperties
      )
    {
      oldAccessibilityProperties.addAll(newAccessibilityProperties);
    }
    
    @Override
    protected String toName(String accessibilityProperty)
    {
      String name = null;
      
      if (XMLConstants.ACC_HIGH_CONTRAST.equals(accessibilityProperty))
      {
        name = "hc";
      }
      
      if (XMLConstants.ACC_LARGE_FONTS.equals(accessibilityProperty))
      {
        name = "lf";
      }
      
      assert(name != null);

      return name;
    }
  }
  
  private static void _fail()
  {
    String message = _LOG.getMessage("SKIN_GENERATION_ERROR");
    throw new IllegalStateException(message);
  }

  private StableNameUtils()
  {
  }
  
  private static final char _SEPARATOR = '-';

  private static final String _DEFAULT_PATH_TOKEN = "d";
  
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StableNameUtils.class);  
  
}
