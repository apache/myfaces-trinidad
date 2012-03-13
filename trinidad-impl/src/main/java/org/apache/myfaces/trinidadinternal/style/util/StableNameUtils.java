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

import java.util.Iterator;
import java.util.Locale;

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
 * These fields all support two additional values:
 * 
 * - Default: in the event that no @-rule variant is expicitly matched, only default styles
 *    (ie. styles that apply to all requests) are included in the generated style sheet.
 *   The token "d" is used to identify this case.  For example, if the skin does not
 *   define any @locale rules, the locale portion of the file name will be "d".
 * 
 * - Multiple: In some cases it is not possible to determine a unique value
 *   for a particular variant, because only @-rules that specify multiple values
 *   are matched.  The token "x" is used to identify such cases.  For example, if the
 *   skin defines a single @locale rule matching the ja, cz, and ko locales, the 
 *   locale portion of the file name for generated style sheets corresponding to these
 *   locales will be "x".
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
        visitor.apppendName(builder);
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
    public void apppendName(StringBuilder builder);
  }

  // Abstract base class for NamingStyleSheetVisitors that operate on the 
  // collections coughed up by StyleSheetNode.  Handles three possible
  // scenarios:
  //
  // 1. There is no explicit match - eg. the skin does not define any @locale
  //    rules, so StyleSheetNode.getLocales() is always empty.  In this case,
  //    we always append the default match path token.
  // 2. There is a single match - eg. the skin defines "@locale ja" and this
  //    StyleSheetNode is visited.  In this case, we call appendSingleValueName()
  //    to get the variant-specific name.
  // 3. There is a multiple match (and no single match) - eg. the skin defines
  //    "@locale ja, cz, ko" and this StyleSheetNode is visited.  In this case,
  //    we always append the multiple match path token.
  private abstract static class CollectionNameExtractor<E> implements NamingStyleSheetVisitor
  {
    @Override
    public final void visit(StyleSheetNode styleSheet)
    {
      Collection<E> values = getCollectionValues(styleSheet);
      _match(styleSheet, values);
    }

    @Override
    public final void apppendName(StringBuilder builder)
    {
      if (_match == Match.SINGLE)
      {
        appendSingleValueName(builder, _singleValue);
      }
      else
      {
        appendNonSingleValueName(builder, _match);
      }
    }
    
    /**
     * Returns the collection that this NamingStyleSheetVisitor will operate on.
     * 
     * For example, a subclass that wants to extract locale-specific names would
     * override this to call styleSheet.getLocales().
     * 
     * @param styleSheet the style sheet that provides the collection
     */
    abstract protected Collection<E> getCollectionValues(StyleSheetNode styleSheet);
    
    /**
     * Called to append a name for single-value matches.
     * 
     * The subclass will translate the singly-matched value into a name.
     * 
     * (CollectionNameExtractor handles the default/multiple match cases automatically.)
     * 
     * @param builder the (non-null) StringBuilder to append to
     * @param singleValue the most recent value returned from matchSingleValue().
     */
    abstract protected void appendSingleValueName(StringBuilder builder, E singleValue);

    /**
     * Appends a name for a non-single match.
     *
     * @param builder the (non-null) StringBuilder to append to
     * @param match the type of match - either Match.DEFAULT or Match.MULTIPLE.
     */
    protected void appendNonSingleValueName(StringBuilder builder, Match match)
    {
      builder.append(_match.pathToken());      
    }

    /**
     * Called to notify the subclass that a collection with a single value has been
     * matched.
     * 
     * @param styleSheet the style sheet that is being processed
     * @param oldValue the previously matched single value, or null if
     *                 no single value has been matched.
     * @param newValue the (non-null) newly matched single value
     * @return the new single value to use for name generation. By default
     *          this returns the newValue.  However, in some
     *           cases, such as locale matching, the old value may be
     *           more specific (eg. "en_US" vs "en") and thus preferred.
     */
    protected E matchSingleValue(StyleSheetNode styleSheet, E oldValue, E newValue)
    {
      return newValue;
    }

    private void _match(StyleSheetNode styleSheet, Collection<E> values)
    {
      int size = values.size();
      
      if (size == 1)
      {
        _matchSingle(styleSheet, values);
      }
      else if (size > 1)
      {
        _matchMultiple();
      }      
    }
    
    private void _matchSingle(StyleSheetNode styleSheet, Collection<E> values)
    {
      assert(values.size() == 1);
      
      Iterator<E> iter = values.iterator();
      if (iter.hasNext())
      {
        E newValue = iter.next();
        assert(newValue != null);

        _singleValue = matchSingleValue(styleSheet, _singleValue, newValue);
        _match = Match.SINGLE;            
      }
    }
    
    private void _matchMultiple()
    {
      if (_match == Match.DEFAULT)
      {
        _match = Match.MULTIPLE;
      }
    }

    private Match _match = Match.DEFAULT;
    private E     _singleValue = null;
  }

  // Enum used by name extraction classes to track match state.
  private static enum Match
  {
    // Indicates that we have not matched any @-rule
    DEFAULT("d"),
    
    // Indicates that we have matched a single-valued @-rule 
    SINGLE("s"),
    
    // Indicates that we have matched a multi-valued @-rule
    MULTIPLE("x");
    
    Match(String pathToken)
    {
      _pathToken = pathToken;
    }
    
    // Returns a token suitable for inclusion in a file name/path.
    public String pathToken()
    {
      return _pathToken;      
    }

    private final String _pathToken;
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
    protected void appendSingleValueName(StringBuilder builder, Integer platform)
    {
      builder.append(NameUtils.getPlatformName(platform));
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
    protected Application  matchSingleValue(
      StyleSheetNode styleSheet,
      Application oldAgentApplication,
      Application newAgentApplication
      )
    {
      _matchVersion(styleSheet.getAgentMatcher(), newAgentApplication);
      
      return newAgentApplication;
    }
    
    private void _matchVersion(
      AgentAtRuleMatcher agentMatcher,
      Application        agentApplication
      )
    {
      assert(agentMatcher != null);
      assert(agentApplication != null);

      Range<Version> matchedVersions =
        agentMatcher.getMatchedVersionsForApplication(agentApplication, _agentVersion);
      _matchedVersions = _matchedVersions.intersect(matchedVersions);

      if (_matchedVersions.isEmpty())
      {
        _LOG.warning("SKIN_EMPTY_VERSION_RANGE");
      }
    }

    @Override
    protected void appendNonSingleValueName(StringBuilder builder, Match match)
    {
      super.appendNonSingleValueName(builder, match);
      
      // Tack on a value for the version field.  (All stable names need to
      // have the same # of fields in order to support allow uris/names to
      // be easily targeted by uri rewriting regular expressions.)
      builder.append(_SEPARATOR);
      builder.append(match.pathToken());
    }

    @Override
    protected void appendSingleValueName(StringBuilder builder, Application agentApplication)
    {
      builder.append(agentApplication.getAgentName());
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
        builder.append(Match.DEFAULT.pathToken());
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
    protected Locale matchSingleValue(
      StyleSheetNode styleSheet,
      Locale         oldLocale,
      Locale         newLocale
      )
    {
      return _getMoreSpecificLocale(oldLocale, newLocale);
    }

   private static Locale _getMoreSpecificLocale(
      Locale oldLocale,
      Locale newLocale
      )
    {
      assert(newLocale != null);

      if (oldLocale == null)
      {
        return newLocale;
      }

      assert(oldLocale.getLanguage().equals(newLocale.getLanguage()));

      return (oldLocale.getCountry() == null ? newLocale : oldLocale);
    }

    @Override
    protected void appendSingleValueName(
      StringBuilder builder,
      Locale        locale
      )
    {
      builder.append(locale.toString());
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
    public void apppendName(StringBuilder builder)
    {
      String name = (_direction == LocaleUtils.DIRECTION_DEFAULT) ? 
                      Match.DEFAULT.pathToken() : 
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

    @Override
    protected String matchSingleValue(
      StyleSheetNode styleSheet,
      String oldAccessibilityProperty,
      String newAccessibilityProperty
      )
    {
      // We use contains() instead of equals() as the accessibility property might be a
      // compund property (eg. "high-contrast&large-fonts").  We could attempt to do a
      // more formal parse, but not much point.
      if (newAccessibilityProperty.contains(XMLConstants.ACC_HIGH_CONTRAST))
      {
        _highContrastMatch = Match.SINGLE;
      }
        
      if (newAccessibilityProperty.contains(XMLConstants.ACC_LARGE_FONTS))
      {
        _largeFontsMatch = Match.SINGLE;
      }
      
      // We track the single value ourselves
      return null;
    }

    @Override
    protected void appendSingleValueName(
      StringBuilder builder,
      String        accessibilityProperty
      )
    {
      assert((_highContrastMatch == Match.SINGLE) ||
             (_largeFontsMatch == Match.SINGLE));
      
      if (_highContrastMatch == Match.SINGLE)
      {
        builder.append("hc");
      }
      
      if (_largeFontsMatch == Match.SINGLE)
      {
        builder.append("lf");
      }
    }
    
    private Match _highContrastMatch = Match.DEFAULT;
    private Match _largeFontsMatch = Match.DEFAULT;
  }

  private StableNameUtils()
  {
  }
  
  private static final char _SEPARATOR = '-';

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StableNameUtils.class);  
  
}
