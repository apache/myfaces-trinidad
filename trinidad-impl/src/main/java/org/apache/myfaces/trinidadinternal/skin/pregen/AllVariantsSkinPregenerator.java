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
package org.apache.myfaces.trinidadinternal.skin.pregen;

import java.util.HashSet;
import java.util.List;

import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent.Application;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.skin.DocumentProviderSkin;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;
import org.apache.myfaces.trinidadinternal.skin.pregen.context.PregenStyleContext;
import org.apache.myfaces.trinidadinternal.skin.pregen.context.PregenStyleProvider;
import org.apache.myfaces.trinidadinternal.skin.pregen.variant.SkinVariant;
import org.apache.myfaces.trinidadinternal.skin.pregen.variant.SkinVariants;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * SkinPregenerator implementation that generates all possible style
 * sheet variants for the skin.
 * 
 * This class is not thread safe.
 */
class AllVariantsSkinPregenerator implements SkinPregenerator
{
  public AllVariantsSkinPregenerator(FacesContext context)
  {
    _generatedFilesPath = _getGeneratedFilesPath(context);
    _generatedSkinFilesPath = _getGeneratedSkinFilesPath(_generatedFilesPath);
  }

  @Override
  public void pregenerate(
    FacesContext    context, 
    Skin            skin,
    PregenConfig    config
    )
  {
    _pregenerateWithStats(context, skin, config);
  }
 
  private void _pregenerateWithStats(
    FacesContext   context,
    Skin           skin,
    PregenConfig   config
    )
  {
    Stats stats = new Stats(skin);
    stats.start();
    
    try
    {
      _pregenerate(context, skin, config, stats); 
    }
    finally
    {
      stats.end();
    }
  }
  
  private void _pregenerate(
    FacesContext    context,
    Skin            skin,
    PregenConfig    config,
    Stats           stats
    )
  {
    StyleProvider provider = _getStyleProviderForSkin(skin);
    StyleSheetDocument document = _getDocumentForSkin(context, skin, provider);
    _pregenerateAllVariants(context, provider, document, config, stats);     
  }
  
  private StyleProvider _getStyleProviderForSkin(Skin skin)
  {
    return new PregenStyleProvider(skin, _generatedSkinFilesPath);
  }
  
  private StyleSheetDocument _getDocumentForSkin(
    FacesContext  context,
    Skin          skin,
    StyleProvider provider
    )
  {
    StyleSheetDocument document = null;
    
    if (skin instanceof DocumentProviderSkin)
    {
      StyleContext styleContext = PregenStyleContext.documentContext(context, provider, _generatedFilesPath);
      document = ((DocumentProviderSkin) skin).getStyleSheetDocument(styleContext);
    }
    
    if (document == null)
    {
      _LOG.warning("SKIN_PREGEN_NO_DOCUMENT", skin.getId());
      document = _EMPTY_DOCUMENT;
    }
        
    return document;
  }
  
  private void _pregenerateAllVariants(
    FacesContext       context,
    StyleProvider      provider,
    StyleSheetDocument document,
    PregenConfig       config,
    Stats              stats
    )
  {
    SkinVariants variants = new SkinVariants(document, config);
    
    for (SkinVariant variant : variants)
    {
      _pregenerateVariant(context, provider, variant, stats);
    }
  }

  private void _pregenerateVariant(
    FacesContext           context,
    StyleProvider          provider,
    SkinVariant            variant,
    Stats                  stats
    )
  {
    StyleContext styleContext = _createStyleContext(context,
                                                    provider,
                                                    variant);

    // This, finally, triggers pregeneration for the current variant.
    List<String> uris = provider.getStyleSheetURIs(styleContext);
    
    stats.variant(variant, uris);
    _variant = variant;
  }

  private StyleContext _createStyleContext(
    FacesContext           context,
    StyleProvider          provider,
    SkinVariant            variant
    )
  {
    assert(variant != null);
    
    return new PregenStyleContext(context, 
                                  provider,
                                  _generatedFilesPath,
                                  variant,
                                  _isDirty(variant));
  }

  // Tests whether we should dirty the style provider when pregenerating
  // the specified variant.  FileSystemStyleCache currently does not prune
  // memory used by its entry caches.  During pregeneration, these caches
  // become fully populated, which can put significant pressure on the heap.
  // We keep memory usage in check by allowing the style provider to free
  // up its caches from time to time.
  private boolean _isDirty(SkinVariant variant)
  {
    // Switching between platforms seems like as good a time as any to let
    // the style provider reset its caches.  This promotes cache reuse within
    // each platform variant while not letting cache size get too out of control.
    return ((_variant != null) && (_variant.getPlatform() != variant.getPlatform()));
  }

  // Returns the root directory for generated files.
  private static String _getGeneratedFilesPath(FacesContext context)
  {
    String targetDirectory = System.getProperty(_TARGET_DIRECTORY_PROPERTY);
    
    if ((targetDirectory == null) || targetDirectory.isEmpty())
    {
      // Refactor to avoid dependency on core renderkit-specific API?
      targetDirectory = CoreRenderingContext.getTemporaryDirectory(context, false);
    }
    
    return targetDirectory;
  }

  // Returns the subdirectory for skin-specific generated files.
  private static String _getGeneratedSkinFilesPath(String generatedFilesPath)
  {
    assert(generatedFilesPath != null);
    assert(!generatedFilesPath.isEmpty());
    assert(generatedFilesPath.charAt(generatedFilesPath.length() - 1) != '/');
    
    return generatedFilesPath + TrinidadRenderingConstants.STYLES_CACHE_DIRECTORY;
  }

  // Little utility class for tracking statistics (eg.
  // # of generated style sheets, duration, etc...)
  private class Stats
  {
    public Stats(Skin skin)
    {
      _skin = skin;
    }

    public void start()
    {
      _logStartMessage();
      _startTime = System.currentTimeMillis(); 
    }

    public void variant(SkinVariant variant, List<String> uris)
    {
      _visitedVariantsCount++;
      
      if (_isNewURI(uris))
      {
        _generatingVariantsCount++;
        _uris.addAll(uris);
      }
      
      _logVariant(variant);
    }

    public void end()
    {
      long endTime = System.currentTimeMillis();
      _logEndMessage(endTime);
    }

    private boolean _isNewURI(List<String> uris)
    {
      return ((uris != null) && !uris.isEmpty() && !_uris.contains(uris.get(0)));
    }
    
    private void _logStartMessage()
    {
      _LOG.info("SKIN_PREGEN_STARTING", _skin.getId());
    }
    
    private void _logEndMessage(long endTime)
    {
      long duration = (endTime - _startTime);
      
      _LOG.info("SKIN_PREGEN_COMPLETED",
                new Object[] {
                  _skin.getId(),
                  _uris.size(),
                  _generatingVariantsCount,
                  _visitedVariantsCount,
                  duration });
    }
    
    private void _logVariant(SkinVariant variant)
    {
      assert(variant != null);
      int platform = variant.getPlatform();
      Application agentApplication = variant.getApplicationAndVersion().application;

      if ((_variant == null)                   ||
          (platform != _variant.getPlatform()) ||
          (agentApplication != _variant.getApplicationAndVersion().application))
      {
        _LOG.info("SKIN_PREGEN_VARIANT",
                  new Object[] {
                    NameUtils.getPlatformName(platform),
                    agentApplication.getAgentName(),
                    _skin.getId() });
      }
    }

    private final Skin        _skin;
    private final Set<String> _uris = new HashSet<String>(101);
    private int               _visitedVariantsCount = 0;
    private int               _generatingVariantsCount = 0;
    private long              _startTime = -1;    
  }

  // Root directory for generated files
  private final String _generatedFilesPath;
  
  // Subdirectory for skin-specific generated files.
  private final String _generatedSkinFilesPath;
  
  // The variant that we are currently processing
  private SkinVariant _variant;
  
  private static final StyleSheetDocument _EMPTY_DOCUMENT = 
    new StyleSheetDocument(new StyleSheetNode[0], null, StyleSheetDocument.UNKNOWN_TIMESTAMP);

  private static final String _TARGET_DIRECTORY_PROPERTY =
    "org.apache.myfaces.trinidad.SKIN_PREGENERATION_SERVICE_TARGET_DIRECTORY";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(AllVariantsSkinPregenerator.class);
}
