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

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidadinternal.style.xml.XMLConstants;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * Helper class for extracting @accessibility-profile rule metadata.
 */
final class AccessibilityVariantExtractor implements SkinVariantExtractor<AccessibilityProfile>
{
  public AccessibilityVariantExtractor()
  {
    _accProfiles = new HashSet<AccessibilityProfile>();
    
    // Seed with default accessibility profile. This won't show
    // up as at @accessibility rule, but needs to be covered during
    // pregeneration.
    _accProfiles.add(AccessibilityProfile.getDefaultInstance());
  }

  @Override
  public void visit(StyleSheetNode node)
  {
    Collection<String> accProps = node.getAccessibilityProperties();
    
    for (String accProp : accProps)
    {
      _addAccessibilityProfile(accProp);
    }
  }

  /**
   * Returns un unmodifiable list containing AccessibilityProfiles corresponding
   * to all @accesibility-profile rules in the specified document.
   */
  public List<AccessibilityProfile> getVariants()
  {
    if (_accProfiles.contains(_HIGH_CONTRAST_ONLY) &&
        _accProfiles.contains(_LARGE_FONTS_ONLY)   &&
        !_accProfiles.contains(_HIGH_CONTRAST_LARGE_FONTS))
    {
      _accProfiles.add(_HIGH_CONTRAST_LARGE_FONTS);
    }
  
    List<AccessibilityProfile> accProfilesList =
      new ArrayList<AccessibilityProfile>(_accProfiles);
  
    return Collections.unmodifiableList(accProfilesList);
  }
  
  private void _addAccessibilityProfile(String accProp)
  {
    AccessibilityProfile accProfile = _toAccessibilityProfile(accProp);

    _accProfiles.add(accProfile);                                                               
  }
  
  private static AccessibilityProfile _toAccessibilityProfile(String accProp)
  {
    boolean highContrast = false;
    boolean largeFonts = false;
    
    if (_isHighContrast(accProp))
    {
      highContrast = true;
    }
    else if (_isLargeFonts(accProp))
    {
      largeFonts = true;
    }
    else if (_isHighContrastLargeFonts(accProp))
    {
      highContrast = true;
      largeFonts = true;
    }
    
    return _getAccessibilityProfile(highContrast, largeFonts);
  }

  private static boolean _isHighContrast(String accProp)
  {
    return XMLConstants.ACC_HIGH_CONTRAST.equals(accProp);
  }
  
  private static boolean _isLargeFonts(String accProp)
  {
    return XMLConstants.ACC_LARGE_FONTS.equals(accProp);
  }

  private static boolean _isHighContrastLargeFonts(String accProp)
  {
    return ((accProp.indexOf(XMLConstants.ACC_HIGH_CONTRAST) > -1) &&
            (accProp.indexOf(XMLConstants.ACC_LARGE_FONTS) > -1));
  }

  private static AccessibilityProfile _getAccessibilityProfile(
    boolean highContrast,
    boolean largeFonts
    )
  {
    return AccessibilityProfile.getInstance(
             highContrast ?
               AccessibilityProfile.ColorContrast.HIGH :
               AccessibilityProfile.ColorContrast.STANDARD,
             largeFonts ?
               AccessibilityProfile.FontSize.LARGE :
               AccessibilityProfile.FontSize.MEDIUM);
  }

  private final Collection<AccessibilityProfile> _accProfiles;

  private static final AccessibilityProfile _HIGH_CONTRAST_ONLY =
    _getAccessibilityProfile(true, false);  
  private static final AccessibilityProfile _LARGE_FONTS_ONLY =
    _getAccessibilityProfile(false, true);
  private static final AccessibilityProfile _HIGH_CONTRAST_LARGE_FONTS =
    _getAccessibilityProfile(true, true);
}
