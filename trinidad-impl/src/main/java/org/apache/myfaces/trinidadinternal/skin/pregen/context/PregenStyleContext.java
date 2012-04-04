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
package org.apache.myfaces.trinidadinternal.skin.pregen.context;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.style.Styles;
import org.apache.myfaces.trinidadinternal.agent.AgentNameUtil;
import org.apache.myfaces.trinidadinternal.agent.DefaultAgent;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgentImpl;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.ContainerType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.RequestType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.StyleClassType;
import org.apache.myfaces.trinidadinternal.skin.pregen.variant.ApplicationAndVersion;
import org.apache.myfaces.trinidadinternal.skin.pregen.variant.SkinVariant;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleSheetNamingStrategy;

/**
 * StyleContext implementation used during skin pregeneration.
 */
public final class PregenStyleContext implements StyleContext
{
  /**
   * Returns a StyleContext instance that can be used to retrieve the
   * StyleSheetDocument for the specified StyleProvider.
   * 
   * @param provider the StyleProvider for which we'll retrieve the StyleSheetDocument
   * @param path the generated files path
   * @return
   */
  public static StyleContext documentContext(
    FacesContext  context,
    StyleProvider provider,
    String        path
    )
  {
    // Note: DocumentProviderSkin.getStyleSheetDocument() minimally requires
    // the StyleContet's StyleProvider and the generated files path, so that is
    // all that we bother with here.
    return new PregenStyleContext(context,
                                  provider, 
                                  path, 
                                  null,
                                  false);
  }

  /**
   * Creates a StyleContext that can be used for pregeneration
   * of a single skin variant.
   * 
   * @param context the FacesContext
   * @param provider the StyleProvider
   * @param generatedFilesPath the root generated files path
   * @param variant the variant of the skin to generate
   */
  public PregenStyleContext(
    FacesContext           context,
    StyleProvider          provider,
    String                 generatedFilesPath,
    SkinVariant            variant,
    boolean                dirty
    )
  {
    _styleProvider = provider;
    _generatedFilesPath = generatedFilesPath;
    _variant = variant;
    _agent = _createTrinidadAgent(context, variant);
    _dirty = dirty;
  }
  
  @Override
  public LocaleContext getLocaleContext()
  {
    return _variant.getLocaleContext();
  }

  @Override
  public TrinidadAgent getAgent()
  {
    return _agent;
  }

  @Override
  public String getGeneratedFilesPath()
  {
    return _generatedFilesPath;
  }
  
  @Override
  public StyleSheetNamingStrategy getNamingStrategy()
  {
    // Stable names are required for pregeneration in order
    // for runtime names to match pregenerated names.
    return StyleSheetNamingStrategy.STABLE;
  }

  @Override
  public boolean checkStylesModified()
  {
    return false;
  }

  @Override
  public boolean disableStandardsMode()
  {
    return false;
  }

  @Override
  public StyleProvider getStyleProvider()
  {
    return getStyleProvider(false);
  }

  @Override
  public StyleProvider getStyleProvider(boolean recompute)
  {
    return _styleProvider;
  }

  @Override
  public Styles getStyles()
  {
    return null;
  }

  @Override
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _variant.getAccessibilityProfile();
  }

  @Override
  public boolean isPortletMode()
  {
    return (_variant.getContainerType() == ContainerType.PORTLET);
  }

  @Override
  public boolean isDisableStyleCompression()
  {
    return (_variant.getStyleClassType() == StyleClassType.UNCOMPRESSED);
  }

  @Override
  public boolean isDirty()
  {
    return _dirty;
  }

  @Override
  public boolean isRequestSecure()
  {
    return (_variant.getRequestType() == RequestType.SECURE);
  }
  
  private static TrinidadAgent _createTrinidadAgent(
    FacesContext          context,
    SkinVariant           variant
    )
  {
    if (variant == null)
    {
      // Null variant == bootstrapping case.  No need for agent access.
      return null;
    }
                            
    // In theory we should be able to create/use our own trivial TrinidadAgent
    // implementation.  However, TrinidadAgentImpl contains all of the magic
    // for figuring out agent capabilities.  Turns out to be easier if we
    // just use this implementation rather than create our own.  Unfortunately,
    // this means that we first need to jump through the hoop of translating
    // the agent information into org.apache.myfaces.trinidad.context.Agent
    // land.
    Agent agent = new PregenAgent(variant.getPlatform(), variant.getApplicationAndVersion());

    return new TrinidadAgentImpl(context, agent);
  }
  
  private static class PregenAgent extends DefaultAgent
  {
    public PregenAgent(
      int                   platform,
      ApplicationAndVersion appAndVersion
      )
    {
      assert(appAndVersion != null);
      
      _platformName = AgentNameUtil.getPlatformName(platform);
      _agentName = appAndVersion.application.getAgentName();
      _agentVersion = appAndVersion.version.toString();
    }
    
    @Override
    public String getAgentName()
    {
      return _agentName;  
    }
    
    @Override
    public String getAgentVersion()
    {
      return _agentVersion;
    }
    
    @Override
    public String getPlatformName()
    {
      return _platformName;
    }
    
    private final String _agentName;
    private final String _agentVersion;
    private final String _platformName;
  }

  private final StyleProvider _styleProvider;
  private final String        _generatedFilesPath;
  private final SkinVariant   _variant;
  private final TrinidadAgent _agent;
  private final boolean       _dirty;
}
