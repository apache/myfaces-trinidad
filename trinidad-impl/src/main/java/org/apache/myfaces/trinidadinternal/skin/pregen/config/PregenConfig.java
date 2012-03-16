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
package org.apache.myfaces.trinidadinternal.skin.pregen.config;

import java.io.File;
import java.io.IOException;

import java.util.AbstractCollection;
import java.util.Arrays;
import java.util.Collection;

import java.util.Collections;
import java.util.Iterator;

import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.EnumParseException;
import org.apache.myfaces.trinidad.util.Enums;
import org.apache.myfaces.trinidad.util.FileUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent.Application;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.share.nls.NullLocaleContext;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

/**
 * Specifies configuration constraints for skin pregeneration.
 * 
 * For the moment, only two types of configurations are supported:
 * 
 * - all: All possible variants are pregenerated
 * - common: Only the most common variants are pregenerated
 * 
 * The "variants" request parameter specifies which of these two
 * configurations to use.
 * 
 * In the future, we can consider supporting finer-grained configuration
 * specification - eg. we could allow specific platforms/agents/locales
 * to be specified via an XML configuration file.
 */
abstract public class PregenConfig
{
  /**
   * Constant returned from variants accessors to indicate that
   * all possible values of the variant should be included during
   * pregeneration.
   */
  public static final Collection<?> ALL_VARIANTS =
    new AbstractCollection() {
    
      @Override
      public boolean isEmpty()
      {
        return false;
      }
  
      @Override
      public Iterator iterator()
      {
        throw new UnsupportedOperationException();
      }

      @Override
      public int size()
      {
        throw new UnsupportedOperationException();
      }
    };

  /**
   * Returns a null/empty PregenConfig instance.
   */
  public static PregenConfig nullInstance()
  {
    return _NULL_CONFIG;    
  }

  /**
   * Creates a PregenConfig instance that is configured based on request
   * parmeter values.
   * 
   * Request parameters are documented in SkinPregenerationService class doc.
   * 
   * @param context the FacesContext to use for retreving request parameter
   *   values.
   * @return
   */
  public static PregenConfig parse(FacesContext context)
    throws InvalidConfigException
  {
    ExternalContext external = context.getExternalContext();

    return _isAllVariantsRequest(external) ?
      new AllPregenConfig(context) :
      new CommonPregenConfig(context);
  }

  private static boolean _isAllVariantsRequest(ExternalContext external)
    throws InvalidConfigException
  {
    Collection<Variants> variants = _parseDisplayNameParam(external,
                                                           "variants",
                                                           Variants.class,
                                                           Variants.COMMON);
    
    return variants.contains(Variants.ALL);
  }

  private static <E extends Enum> Collection<E> _parseDisplayNameParam(
    ExternalContext external,
    String          paramName,
    Class<E>        enumClass, 
    E               defaultValue
    ) throws InvalidConfigException
  {
    try
    {
      return Enums.parseDisplayNameEnumRequestParameter(external,
                                                        paramName,
                                                        enumClass,
                                                        defaultValue);
    }
    catch (EnumParseException e)
    {
      throw new InvalidConfigException(e.getMessage());
    }
  }

  /**
   * Returns the (non-null) platform variants to pregenerate.
   * 
   * The collection values correspond to TrinidadAgent.OS_* constants.
   * 
   * A return value of PregenConfig.ALL indicates that all platform
   * variants should be pregenerated.
   */
  abstract public Collection<Integer> getPlatformVariants();
  
  /**
   * Returns the (non-null) locale variants to pregenerate.
   * 
   * A return value of PregenConfig.ALL indicates that all locale
   * variants should be pregenerated.
   */
  abstract public Collection<LocaleContext> getLocaleVariants();
  
  /**
   * Returns the (non-null) reading direction variants to pregenerate.
   * 
   * A return value of PregenConfig.ALL indicates that both ltr and
   * rtl variants should be pregenerated.
   */
  abstract public Collection<Integer> getReadingDirectionVariants();
  
  /**
   * Returns the (non-null) agent application variants to pregenerate.
   * 
   * A return value of PregenConfig.ALL indicates that all agent application
   * variants should be pregenerated.
   */
  abstract public Collection<Application> getAgentApplicationVariants();
  
  /**
   * Returns the (non-null) accessibility variants to pregenerate.
   * 
   * A return value of PregenConfig.ALL indicates that all accessibility
   * variants should be pregenerated.
   */
  abstract public Collection<AccessibilityProfile> getAccessibilityVariants();
  
  /**
   * Returns the conatiner types to pregenerate.
   */
  abstract public Collection<ContainerType> getContainerTypes();

  /**
   * Returns the request types to pregenerate.
   */
  abstract public Collection<RequestType> getRequestTypes();
  
  /**
   * Returns the style class types to pregenerate.
   */
  abstract public Collection<StyleClassType> getStyleClassTypes();
  
  /**
   * Returns the path of the target output directory.
   * 
   * This returns a non-null path to an existing, writable directory.
   * 
   * The returned path is guaranteed to not have a trailing file
   * separator.
   */
  abstract public String getTargetDirectoryPath();
  
  // Utility enum class used to identify whether PregenConfig.parse() should
  // return a PregenConfig instance that is configured for generating
  // "common" or "all" variants.
  public enum Variants
  {
    COMMON("common"),
    ALL("all");
    
    Variants(String displayName)
    {
      _displayName = displayName;
    }
    
    public String displayName()
    {
      return _displayName;
    }
    
    public static Variants valueOfDisplayName(String displayName)
    {
      return Enums.stringToEnum(_displayNameMap, displayName, Variants.class);
    }
    
    private final String _displayName;
    private static final Map<String, Variants> _displayNameMap;
    
    static
    {
      _displayNameMap = Enums.createDisplayNameMap(Variants.class);
    }
  }
  
  // Enum that is used to indicate whether pregeneration should target
  // servlet or portlet containers (or both).
  public enum ContainerType
  {
    SERVLET("servlet"),
    PORTLET("portlet");

    ContainerType(String displayName)
    {
      _displayName = displayName;
    }
    
    public String displayName()
    {
      return _displayName;
    }
    
    public static ContainerType valueOfDisplayName(String displayName)
    {
      return Enums.stringToEnum(_displayNameMap, displayName, ContainerType.class);
    }
    
    private final String _displayName;
    private static final Map<String, ContainerType> _displayNameMap;
    
    static
    {
      _displayNameMap = Enums.createDisplayNameMap(ContainerType.class);
    }    
  }
  
  // Enum that is used to indicate whether pregeneration should target  
  // secure or nonsecure request types (or both).
  public enum RequestType
  {
    NONSECURE("nonsecure"),
    SECURE("secure");

    RequestType(String displayName)
    {
      _displayName = displayName;
    }
    
    public String displayName()
    {
      return _displayName;
    }
    
    public static RequestType valueOfDisplayName(String displayName)
    {
      return Enums.stringToEnum(_displayNameMap, displayName, RequestType.class);
    }
    
    private final String _displayName;
    private static final Map<String, RequestType> _displayNameMap;
    
    static
    {
      _displayNameMap = Enums.createDisplayNameMap(RequestType.class);
    } 
  }

  // Enum that is used to indicate whether pregeneration should target
  // compressed or uncompressed style classes (or both).
  public enum StyleClassType
  {
    COMPRESSED("compressed"),
    UNCOMPRESSED("uncompressed");

    StyleClassType(String displayName)
    {
      _displayName = displayName;
    }
    
    public String displayName()
    {
      return _displayName;
    }
    
    public static StyleClassType valueOfDisplayName(String displayName)
    {
      return Enums.stringToEnum(_displayNameMap, displayName, StyleClassType.class);
    }
    
    private final String _displayName;
    private static final Map<String, StyleClassType> _displayNameMap;
    
    static
    {
      _displayNameMap = Enums.createDisplayNameMap(StyleClassType.class);
    } 
  }

  protected PregenConfig()
  {
  }

  // A PregenConfig implementation that derives its contextual configuration 
  // from request parameters.
  private static abstract class ParamPregenConfig extends PregenConfig
  {
    public ParamPregenConfig(FacesContext context)
      throws InvalidConfigException
    {
      ExternalContext external = context.getExternalContext();

      _containerTypes = _parseDisplayNameParam(external,
                                               "containerType",
                                               ContainerType.class,
                                               ContainerType.SERVLET);

      _requestTypes = _parseDisplayNameParam(external,
                                             "requestType",
                                             RequestType.class,
                                             RequestType.NONSECURE);
      
      _styleClassTypes = _parseDisplayNameParam(external,
                                                "styleClassType",
                                                StyleClassType.class,
                                                StyleClassType.COMPRESSED);
      
      _targetDirectoryPath = _getTargetDirectoryPath(context);
    }
    
    @Override
    public Collection<ContainerType> getContainerTypes()
    {
      return _containerTypes;
    }
    
    @Override
    public Collection<RequestType> getRequestTypes()
    {
      return _requestTypes;
    }
    
    @Override
    public Collection<StyleClassType> getStyleClassTypes()
    {
      return _styleClassTypes;
    }

    @Override
    public String getTargetDirectoryPath()
    {
      return _targetDirectoryPath;
    }

    // Returns the target directory path.  Throws an InvalidConfigException
    // if the target directory does not exist/cannot be created or is not
    // writable.
    private static String _getTargetDirectoryPath(FacesContext context)
      throws InvalidConfigException
    {
      String targetDirectoryPath = _getNormalizedTargetDirectoryPath(context);
      
      try
      {
        FileUtils.toWritableDirectory(targetDirectoryPath);
      }
      catch (IOException e)
      {
        // The directory either does not exist/cannot be created or
        // exists but is not writable.  Propagate this failure out.
        throw new InvalidConfigException(e.getMessage());
      }
      
      return targetDirectoryPath;
    }

    // Returns the target directory path in its normalized form.
    private static String _getNormalizedTargetDirectoryPath(FacesContext context)
    {
      String targetDirectoryPath = System.getProperty(_TARGET_DIRECTORY_PROPERTY);
    
      if ((targetDirectoryPath == null) || targetDirectoryPath.isEmpty())
      {
        // Refactor to avoid dependency on core renderkit-specific API?
        targetDirectoryPath = CoreRenderingContext.getTemporaryDirectory(context, false);
      }
    
      return _normalizeDirectoryPath(targetDirectoryPath);
    }
    
    // Normalizes the specified directory path by stripping off the trailing
    // file separator, if present.
    private static String _normalizeDirectoryPath(String path)
      throws InvalidConfigException
    {
      if ((path == null) || path.isEmpty())
      {
        String message = _LOG.getMessage("SKIN_PREGEN_NO_TARGET_DIRECTORY", 
                                         _TARGET_DIRECTORY_PROPERTY);
        throw new InvalidConfigException(message);
      }

      return _hasTrailingSeparator(path) ? 
        path.substring(0, path.length() - 1) :
        path;
    }
    
    private static boolean _hasTrailingSeparator(String path)
    {
      char lastChar = path.charAt(path.length() - 1);
      
      // Explicitly check '/' just in case someone happens to be using
      // this as a separator in their sys property value (and we're on
      // a platform that uses a different separator, eg. Windows).
      return ((lastChar == File.separatorChar) || (lastChar == '/'));
    }

    private final Collection<ContainerType>  _containerTypes;
    private final Collection<RequestType>    _requestTypes;
    private final Collection<StyleClassType> _styleClassTypes;
    private final String                     _targetDirectoryPath;
  }
  
  // PregenConfig implementation that is used for pregeneration of
  // all possible variant values.
  private static class AllPregenConfig extends ParamPregenConfig
  {
    public AllPregenConfig(FacesContext context)
    {
      super(context);
    }

    @Override
    public Collection<Integer> getPlatformVariants()
    {
      return (Collection<Integer>)ALL_VARIANTS;
    }
    
    @Override
    public Collection<LocaleContext> getLocaleVariants()
    {
     return (Collection<LocaleContext>)ALL_VARIANTS;
    }

    @Override
    public Collection<Integer> getReadingDirectionVariants()
    {
      return (Collection<Integer>)ALL_VARIANTS;
    }

    @Override
    public Collection<Application> getAgentApplicationVariants()
    {
      return (Collection<Application>)ALL_VARIANTS;
    }

    @Override
    public Collection<AccessibilityProfile> getAccessibilityVariants()
    {
      return (Collection<AccessibilityProfile>)ALL_VARIANTS;      
    }
  }

  // PregenConfig implementation that is used for pregeneration of
  // only the most common variant values.
  private static class CommonPregenConfig extends ParamPregenConfig
  {
    public CommonPregenConfig(FacesContext context)
    {
      super(context);
    }

    @Override
    public Collection<Integer> getPlatformVariants()
    {
      return Arrays.asList(
               TrinidadAgent.OS_ANDROID,
               TrinidadAgent.OS_IPHONE,
               TrinidadAgent.OS_LINUX,
               TrinidadAgent.OS_MACOS,
               TrinidadAgent.OS_WINDOWS);
    }
    
    @Override
    public Collection<LocaleContext> getLocaleVariants()
    {
     return Arrays.asList(
              NullLocaleContext.getLeftToRightContext(),
              NullLocaleContext.getRightToLeftContext());
    }

    @Override
    public Collection<Integer> getReadingDirectionVariants()
    {
      return Arrays.asList(LocaleUtils.DIRECTION_LEFTTORIGHT);
    }

    @Override
    public Collection<Application> getAgentApplicationVariants()
    {
      return Arrays.asList(Application.GECKO,
                           Application.IEXPLORER,
                           Application.SAFARI);
    }

    @Override
    public Collection<AccessibilityProfile> getAccessibilityVariants()
    {
      return Arrays.asList(AccessibilityProfile.getDefaultInstance());      
    }
  }
  
  // Null PregenConfig implementation. Only used for error cases, to
  // avoid null comparisons.
  private static final class NullPregenConfig extends PregenConfig
  {
    @Override
    public Collection<Integer> getPlatformVariants()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<LocaleContext> getLocaleVariants()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<Integer> getReadingDirectionVariants()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<Application> getAgentApplicationVariants()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<AccessibilityProfile> getAccessibilityVariants()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<PregenConfig.ContainerType> getContainerTypes()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<PregenConfig.RequestType> getRequestTypes()
    {
      return Collections.emptySet();
    }

    @Override
    public Collection<PregenConfig.StyleClassType> getStyleClassTypes()
    {
      return Collections.emptySet();
    }
    
    @Override
    public String getTargetDirectoryPath()
    {
      return null;
    }
  }

  private static final PregenConfig _NULL_CONFIG = new NullPregenConfig();

  private static final String _TARGET_DIRECTORY_PROPERTY =
    "org.apache.myfaces.trinidad.SKIN_PREGENERATION_SERVICE_TARGET_DIRECTORY";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(PregenConfig.class);  
}
