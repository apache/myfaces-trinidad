/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import java.util.Collection;

import junit.framework.Test;

import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.AccessibilityProfile.ColorContrast;
import org.apache.myfaces.trinidad.context.AccessibilityProfile.FontSize;
import org.apache.myfaces.trinidadinternal.skin.SkinTestCase;

public class AccessibilityVariantExtractorTest extends SkinTestCase
{
  public static final Test suite()
  {
    return new TestSuite(AccessibilityVariantExtractorTest.class);
  }

  public static void main(String[] args)
    throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }
  
  public AccessibilityVariantExtractorTest(String string)
  {
    super(string);
  }

  public void testEmpty()
  {
    AccessibilityVariantExtractor extractor = new AccessibilityVariantExtractor();
    visitStyleSheets(TestSkin.EMPTY, extractor);
    
    Collection<AccessibilityProfile> profiles = extractor.getVariants();
    assertEquals(profiles.size(), 1);
    
    _assertContainsDefaultProfile(profiles);
  }
  
  public void testLargeFonts()
  {
    AccessibilityVariantExtractor extractor = new AccessibilityVariantExtractor();    
    visitStyleSheets(TestSkin.ACCESSIBILITY_LARGE_FONTS, extractor);

    Collection<AccessibilityProfile> profiles = extractor.getVariants();    
    assertEquals(profiles.size(), 2);
    _assertContainsDefaultProfile(profiles);
    _assertContainsLargeFontsProfile(profiles);
  }
  
  private void _assertContainsDefaultProfile(Collection<AccessibilityProfile> profiles)
  {
    assertTrue("contains default acc profile",
               profiles.contains(AccessibilityProfile.getDefaultInstance()));    
  }

  private void _assertContainsLargeFontsProfile(Collection<AccessibilityProfile> profiles)
  {
    AccessibilityProfile profile = AccessibilityProfile.getInstance(ColorContrast.STANDARD,
                                                                    FontSize.LARGE);
    
    assertTrue("contains large fonts acc profile", profiles.contains(profile));
  }
}
