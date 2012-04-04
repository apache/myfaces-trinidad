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
package org.apache.myfaces.trinidadinternal.skin;

import junit.framework.TestCase;

import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils.StyleSheetVisitor;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

/**
 * Base class for tests that need to work with parsed skins (ie. with
 * StyleSheetDocuments).
 */
public class SkinTestCase extends TestCase
{
  /**
   * Enum for built-in test skins.
   */
  public enum TestSkin
  {
    EMPTY("empty"),
    ACCESSIBILITY_HIGH_CONTRAST("acc-high-contrast"),
    ACCESSIBILITY_LARGE_FONTS("acc-large-fonts");
    
    TestSkin(String skinName)
    {
      _skinName = skinName; 
    }
  
    /**
     * Returns a resource name suitable for use with
     * ClassLoader.getResource().
     */
    public String getResourceName()
    {
      return _RESOURCE_PATH + _skinName + _SUFFIX;
    }

    private final String _skinName;
  }

  public SkinTestCase(String string)
  {
    super(string);
  }
  
  /**
   * Returns the StyleSheetDocument for the specified test skin.
   */
  protected StyleSheetDocument getSkinDocument(TestSkin testSkin)
  {
    return SkinTestUtils.parseSkin(testSkin.getResourceName());
  }

  /**
   * Performs a visit of all style sheet nodes in the specified test
   * skin
   */
  protected void visitStyleSheets(TestSkin testSkin, StyleSheetVisitor visitor)
  {
    StyleSheetDocument document = getSkinDocument(testSkin);
    
    StyleSheetVisitUtils.visitStyleSheets(document.getStyleSheetsAsCollection(), visitor);
  }

  private static final String _RESOURCE_PATH =
    "org/apache/myfaces/trinidadinternal/skin/testSkins/";
  
  private static final String _SUFFIX = ".css";
}
