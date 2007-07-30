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
package org.apache.myfaces.trinidad.skin;

import java.util.Collections;

import org.apache.myfaces.trinidad.context.LocaleContext;

/**
 * SkinAdditions are defined in trinidad-skins.xml file &lt;skin-addition&gt;
 * They are used by custom component developers who have created custom
 * components, and they need a way to 'push' in their own stylesheet and 
 * resource bundle for these components into some skin of their choosing, 
 * most likely the simple skin.
 * Skin objects contain zero or more SkinAdditions. The SkinAdditions' stylesheets
 * are merged into the Skin's own stylesheet. The SkinAdditions' resource
 * bundle is looked at along with the Skin's own resource bundle when Skin's
 * getTranslatedValue is called.
 *
 */
public class SkinAddition
{


  /**
   * Constructor takes a styleSheet name and a resourceBundle name.
   */
  public SkinAddition (
    String styleSheetName,
    String resourceBundleName
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = resourceBundleName;
  }
  
  /**
   * Gets the SkinAddition's style sheet name.
   */
  public String getStyleSheetName()
  {
    return _styleSheetName;
  } 
  
  /**
   * Gets the SkinAddition's resource bundle .     
   */
  public String getResourceBundleName()
  {
    return _resourceBundleName;
  } 
 
  private final String _styleSheetName;
  private final String _resourceBundleName;
  
}
