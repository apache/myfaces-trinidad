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
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

import java.util.List;

import java.util.NoSuchElementException;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent.Application;
import org.apache.myfaces.trinidadinternal.share.nls.NullLocaleContext;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.ContainerType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.RequestType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.StyleClassType;
import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils.StyleSheetVisitor;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * Produces the unique combinations of variants for a skin.  These variants
 * are exposed via iteration - ie. clients interact with this class as an 
 * Iterable.
 */
public final class SkinVariants implements Iterable<SkinVariant>
{
  public SkinVariants(
    StyleSheetDocument document,
    PregenConfig       config
    )
  {
    SkinVariantExtractor<Integer> platformExtractor = _getPlatformExtractor(config);
    SkinVariantExtractor<ApplicationAndVersion> agentExtractor = _getAgentExtractor(config);
    SkinVariantExtractor<LocaleContext> localeExtractor = _getLocaleExtractor(config);    
    SkinVariantExtractor<AccessibilityProfile> accExtractor = _getAccessibilityExtractor(config);
    
    _extractVariants(document,
                    localeExtractor,
                    platformExtractor,
                    agentExtractor,
                    accExtractor);
    
    _localeContexts = localeExtractor.getVariants();
    _platforms = platformExtractor.getVariants();
    _appAndVersions = agentExtractor.getVariants();
    _accProfiles = accExtractor.getVariants();

    _containerTypes = new ArrayList<ContainerType>(config.getContainerTypes());
    _requestTypes = new ArrayList<RequestType>(config.getRequestTypes());
    _styleClassTypes = new ArrayList<StyleClassType>(config.getStyleClassTypes());
  }

  public Iterator<SkinVariant> iterator()
  {
    return new VariantsIterator();
  }

  private SkinVariantExtractor<Integer> _getPlatformExtractor(PregenConfig config)
  {
    Collection<Integer> platforms = config.getPlatformVariants();
    
    if (platforms == PregenConfig.ALL_VARIANTS)
    {
      return new PlatformVariantExtractor();
    }

    return (platforms.isEmpty()) ?
      FixedVariantExtractor.extractor(TrinidadAgent.OS_UNKNOWN) :
      FixedVariantExtractor.extractor(platforms);
  }
  
  private SkinVariantExtractor<ApplicationAndVersion> _getAgentExtractor(PregenConfig config)
  {
    Collection<Application> agentApplications = config.getAgentApplicationVariants();
    
    if (agentApplications.isEmpty())
    {
      return FixedVariantExtractor.extractor(ApplicationAndVersion.UNKNOWN);
    }
    
    if (agentApplications == PregenConfig.ALL_VARIANTS)
    {
      agentApplications = null;
    }

    // We need to go through AgentVariantExtractor even in the case where
    // PregenConfig provides a fixed set of agents to pregenerate since
    // wee need to visit style sheets to determine agent versions.
    return new AgentVariantExtractor(agentApplications);
  }

  private SkinVariantExtractor<LocaleContext> _getLocaleExtractor(PregenConfig config)
  {
    SkinVariantExtractor<LocaleContext> localeExtractor;
    Collection<LocaleContext> locales = config.getLocaleVariants();
    
    if (locales == PregenConfig.ALL_VARIANTS)
    {
      localeExtractor = new LocaleVariantExtractor();
    }
    else
    {
      if (locales.isEmpty())
      {
        locales = Arrays.asList(NullLocaleContext.getLeftToRightContext(),
                                NullLocaleContext.getRightToLeftContext());
      }
      
      localeExtractor = FixedVariantExtractor.extractor(locales);
    }

    return _filterDirections(localeExtractor, config);
  }

  private SkinVariantExtractor<LocaleContext> _filterDirections(
    SkinVariantExtractor<LocaleContext> localeExtractor,
    PregenConfig                        config
    )
  {
    Collection<Integer> directions = config.getReadingDirectionVariants();
    
    if ((directions == PregenConfig.ALL_VARIANTS) || (directions.size() > 1))
    {
      return localeExtractor;
    }
    
    boolean rtlOnly = directions.contains(LocaleUtils.DIRECTION_RIGHTTOLEFT);

    return new DirectionFilteringVariantExtractor(localeExtractor, rtlOnly);
  }
  
  private SkinVariantExtractor<AccessibilityProfile> _getAccessibilityExtractor(
    PregenConfig config
    )
  {
    Collection<AccessibilityProfile> accessibilityProfiles =
      config.getAccessibilityVariants();
    
    if (accessibilityProfiles == PregenConfig.ALL_VARIANTS)
    {
      return new AccessibilityVariantExtractor();
    }
    
    return (accessibilityProfiles.isEmpty() ?
      FixedVariantExtractor.extractor(AccessibilityProfile.getDefaultInstance()) :
      FixedVariantExtractor.extractor(accessibilityProfiles));
      
  }

  private void _extractVariants(
    StyleSheetDocument      document,
    StyleSheetVisitor...    visitors
    )
  {
    Collection<StyleSheetNode> styleSheets = document.getStyleSheetsAsCollection();
    StyleSheetVisitor compoundVisitor = 
      StyleSheetVisitUtils.compoundStyleSheetVisitor(Arrays.asList(visitors));

    StyleSheetVisitUtils.visitStyleSheets(styleSheets, compoundVisitor);    
  }

  private class VariantsIterator implements Iterator<SkinVariant>
  {
    @Override
    public boolean hasNext()
    {
      return ((_localeIndex < _localeContexts.size())         &&
               (_platformIndex < _platforms.size())           &&
               (_appAndVersionIndex < _appAndVersions.size()) &&
               (_accProfileIndex < _accProfiles.size())       &&
               (_containerTypeIndex < _containerTypes.size()) &&
               (_requestTypeIndex < _requestTypes.size())     &&
               (_styleClassTypeIndex < _styleClassTypes.size()));
    }

    @Override
    public SkinVariant next()
    {
      SkinVariant variant = _getNextVariant();
      _nextIndex();
      
      return variant;
    }

    @Override
    public void remove()
    {
      throw new UnsupportedOperationException();
    }
    
    private SkinVariant _getNextVariant()
    {
      if (!hasNext())
      {
        throw new NoSuchElementException();
      }
      
      return new SkinVariant(_localeContexts.get(_localeIndex),
                             _platforms.get(_platformIndex),
                             _appAndVersions.get(_appAndVersionIndex),
                             _accProfiles.get(_accProfileIndex),
                             _containerTypes.get(_containerTypeIndex),
                             _requestTypes.get(_requestTypeIndex),
                             _styleClassTypes.get(_styleClassTypeIndex));
    }
    
    private void _nextIndex()
    {
      _localeIndex++;
        
      if (_localeIndex == _localeContexts.size())
      {
        _localeIndex = 0;
        _accProfileIndex++;
      }
      
      if (_accProfileIndex == _accProfiles.size())
      {
        _accProfileIndex = 0;
        _containerTypeIndex++;
      }
      
      if (_containerTypeIndex == _containerTypes.size())
      {
        _containerTypeIndex = 0;
        _requestTypeIndex++;
      }
      
      if (_requestTypeIndex == _requestTypes.size())
      {
        _requestTypeIndex = 0;
        _styleClassTypeIndex++;
      }

      if (_styleClassTypeIndex == _styleClassTypes.size())
      {
        _styleClassTypeIndex = 0;
        _appAndVersionIndex++;        
      }

      if (_appAndVersionIndex == _appAndVersions.size())
      {
        _appAndVersionIndex = 0;
        _platformIndex++;
      }
    }

    private int _localeIndex = 0;
    private int _platformIndex = 0;
    private int _appAndVersionIndex = 0;
    private int _accProfileIndex = 0;
    private int _containerTypeIndex = 0;
    private int _requestTypeIndex = 0;
    private int _styleClassTypeIndex = 0;
  }

  private final List<LocaleContext>         _localeContexts;
  private final List<Integer>               _platforms;
  private final List<ApplicationAndVersion> _appAndVersions;
  private final List<AccessibilityProfile>  _accProfiles;
  private final List<ContainerType>         _containerTypes;
  private final List<RequestType>           _requestTypes;
  private final List<StyleClassType>        _styleClassTypes;
}
