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
package org.apache.myfaces.trinidaddemo;

import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;

/**
 * Bean for accessibilityProfileDemo.jspx demo.
 */
public class DemoAccessibilityProfileBean
{
  public DemoAccessibilityProfileBean()
  {
  }

  /**
   * Return the accessibility mode preference.
   */
  public String getAccessibilityMode()
  {
    // At the moment, we don't actually surface this preference
    // the demo, so just return the default value.
    return "default";
  }

  /**
   * Return the accessibility profile preference.
   */
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _accProfile;
  }

  /**
   * Return the skin family preference.
   */
  public String getSkinFamily()
  {
    // We force the skin family to the accessibility demo
    // skin, which contains various demonstrations of
    // how to use @accessibility-profile.
    return "accdemo";
  }

  /**
   * Is high contrast enabled?
   */
  public boolean isHighContrast()
  {
    return _highContrast;
  }
  
  /**
   * Are large fonts enabled?
   */
  public boolean isLargeFonts()
  {
    return _largeFonts;
  }

  /**
   * Sets a new high contrast value.
   */  
  public void setHighContrast(boolean highContrast)
  {
    _highContrast = highContrast;
  }
  
  /**
   * Sets a new large fonts value
   */  
  public void setLargeFonts(boolean largeFonts)
  {
    _largeFonts = largeFonts;
  }

  /**
   * ActionListener for handling accessibility profile updates.
   */  
  public void update(ActionEvent event)
  {
    AccessibilityProfile.ColorContrast colorContrast =
      isHighContrast() ?
        AccessibilityProfile.ColorContrast.HIGH : 
        AccessibilityProfile.ColorContrast.STANDARD;

    AccessibilityProfile.FontSize fontSize =
      isLargeFonts() ?
        AccessibilityProfile.FontSize.LARGE : 
        AccessibilityProfile.FontSize.MEDIUM;
    
    _accProfile = AccessibilityProfile.getInstance(colorContrast, fontSize);
  }

  private boolean              _highContrast;
  private boolean              _largeFonts;
  private AccessibilityProfile _accProfile;
}
