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

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;

import java.util.HashMap;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidad.util.Range;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent.Application;
import org.apache.myfaces.trinidadinternal.skin.AgentAtRuleMatcher;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * An @-rule processor for extracting @agent rule metadata.
 */
final class AgentVariantExtractor implements SkinVariantExtractor<ApplicationAndVersion>
{
  /**
   * Creates an AgentVariantExtractor for a specified set of supported
   * agent applications.  AgentVariantExtractor.getVariants() will only
   * ApplicationAndVersion instances corresponding to these agent
   * applications.  If no supported agent applications are specified,
   * all agent applications found in the style sheet nodes will be 
   * treated as supported.
   */
  public AgentVariantExtractor(
    Collection<Application> supportedApplications
    )
  {
    _appVersionsMap =
      new HashMap<Application, Set<Version>>();
    
    _supportedApplications = _initSupportedApplications(supportedApplications);
    
    // Seed the map with unknown agent.  This won't appear
    // in the skin definition, but we need to cover this case
    // during pregeneration.
    _addApplicationIfSupported(Application.UNKNOWN);  
  }
  
  private Collection<Application> _initSupportedApplications(
    Collection<Application> supportedApplications
    )
  {
    if ((supportedApplications == null) || supportedApplications.isEmpty())
    {
      return new AbstractCollection<Application>() {
        
          @Override
          public boolean contains(Object o)
          {
            return true;  
          }
          
          @Override
          public Iterator<Application> iterator()
          {
            throw new UnsupportedOperationException();
          }

          @Override
          public int size()
          {
            throw new UnsupportedOperationException();
          }
        };
    }

    return new HashSet<Application>(supportedApplications);
  }

  @Override
  public void visit(StyleSheetNode node)
  {
    AgentAtRuleMatcher agentMatcher = node.getAgentMatcher();
    
    if (agentMatcher != null)
    {
      _addApplicationVersions(agentMatcher);    
    }
  }

  /**
   * Returns alist containing ApplicationAndVersions
   * corresponding to all processed @agent rules.
   */
  public List<ApplicationAndVersion> getVariants()
  {
    List<ApplicationAndVersion> appAndVersionsList =
      _toAppAndVersionsList(_appVersionsMap);

    return appAndVersionsList;
  }
  
  private void _addApplicationVersions(AgentAtRuleMatcher agentMatcher)
  {
    assert(agentMatcher != null);
    
    Collection<Application> nodeApplications =
      agentMatcher.getAllApplications();
      
    for (Application application : nodeApplications)
    {

      boolean supported = _addApplicationIfSupported(application);
      
      if (supported)
      {
        Collection<Range<Version>> versionRanges = agentMatcher.getAllVersionsForApplication(application);
        _addVersions(application, versionRanges);        
      }

    }    
  }
  
  private boolean _addApplicationIfSupported(Application application)
  {
    if (!_supportedApplications.contains(application))
    {
      return false;
    }

    if (!_appVersionsMap.containsKey(application))
    {
      Set<Version> versions = new TreeSet<Version>();
      
      // Minimally, every application needs to be able
      // to pregenerate for the unknown version case.
      versions.add(_UNKNOWN_VERSION);
      
      _appVersionsMap.put(application, versions);
    }
    
    return true;
  }
  
  private void _addVersions(
    Application  application,
    Collection<Range<Version>> versionRanges
    )
  {
    Collection<Version> versions = _appVersionsMap.get(application);
    assert(versions != null);
    
    for (Range<Version> versionRange : versionRanges)
    {
      // We add the start/end points of the range to our
      // set of versions to pregenerate.
      
      // Also note that from here on out we only want to
      // deal with "concrete" versions.  If we leave version
      // wildcards in place, we can lose information due to
      // Version's wildcard-sensitive natural ordering (ie.
      // a Set.add() might fail because a matching/wildcarded
      // version is already present.)
      versions.add(versionRange.getStart().toMinimumVersion());
      versions.add(versionRange.getEnd().toMaximumVersion());
    }
  }

  private static List<ApplicationAndVersion> _toAppAndVersionsList(
    Map<Application, Set<Version>> appVersionsMap
    )
  {
    ArrayList<ApplicationAndVersion> appAndVersions = 
      new ArrayList<ApplicationAndVersion>();
    
    for (Map.Entry<Application, Set<Version>> entry : appVersionsMap.entrySet())
    {
      for (Version version : entry.getValue())
      {
        appAndVersions.add(new ApplicationAndVersion(entry.getKey(), version));
      }
    }
    
    return appAndVersions;
  }

  // Map of application to versions that have been encountered 
  // during processing
  private final Map<Application, Set<Version>> _appVersionsMap;
  
  // Only extract version information for these applications.
  private final Collection<Application> _supportedApplications;

  // A Version that we use to ensure that we pregenerate style sheets
  // for the case where the agent version does not match any version
  // specified in the skin.
  private static final Version _UNKNOWN_VERSION = new Version("unknown");
}
