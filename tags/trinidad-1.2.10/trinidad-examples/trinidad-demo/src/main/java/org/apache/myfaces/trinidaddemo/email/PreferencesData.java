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
package org.apache.myfaces.trinidaddemo.email;

import java.util.ArrayList;
import java.util.List;

import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;

/**
 * Bean which provides preferences data
 *
 * @version 1.0
 */
public class PreferencesData
{
  /**
   * Branding type for "regular" branding, which includes the
   * corporate brand, a large product brand, and global button
   * icons.
   */
  public static String REGULAR_BRANDING = "regular";

  /**
   * Branding type for "medium" branding, which includes the
   * corporate brand, a smaller product brand, and no global
   * button icons
   */
  public static String MEDIUM_BRANDING = "medium";

  /**
   * Branding type for "small" branding, which includes the
   * corporate brand, no product brand, and no global
   * button icons
   */
  public static String SMALL_BRANDING = "small";

  /**
   * Creates a PreferencesData object with default preferences
   */
  public PreferencesData()
  {
    // need public accesibility mode constants declared in Trinidad!
    _branding = REGULAR_BRANDING;
    _rowsShown = _DEFAULT_ROWS_SHOWN;
    _displayFolderTree = true;
    _displaySenderColumn = true;
    _displayDateColumn = true;
    _displaySizeColumn = true;
    _skinFamily = "suede";
    _accessibilityMode = "default";
  }


  /**
   * @todo Provide internationalized list.
   */
  public List<SelectItem> getSkinFamilyItems()
  {
    return _SKIN_FAMILIES;
  }


  /**
   * @todo Provide internationalized list.
   */
  public List<SelectItem> getAccessibilityModeItems()
  {
    return _ACCESSIBILITY_MODES;
  }

  /**
   * Returns the skin-family for the application.
   */
  public String getSkinFamily()
  {
    return _skinFamily;
  }


  /**
   * Returns the skin-family for the application.
   */
  public void setSkinFamily(String skinFamily)
  {
    _skinFamily = skinFamily;
  }

  /**
   * Returns the AccessibilityMode for this user
   */
  public String getAccessibilityMode()
  {
    return _accessibilityMode;
  }

  /**
   * Returns the AccessibilityProfile for this user.
   */
  public AccessibilityProfile getAccessibilityProfile()
  {
    // For the moment, just use the default profile.
    return AccessibilityProfile.getDefaultInstance();
  }

  /**
   * Returns the branding type: REGULAR_BRANDING,
   * MEDIUM_BRANDING or SMALL_BRANDING
   */
  public String getBranding()
  {
    return _branding;
  }

  /**
   * Returns the numbers of rows to display in the messages table.
   */
  public int getRowsShown()
  {
    return _rowsShown;
  }

  /**
   * Gets whether the SideBar with the folder tree is displayed
   */
  public boolean getDisplayFolderTree()
  {
    return _displayFolderTree;
  }

  /**
   * Sets whether the SideBar with the folder tree is displayed
   */
  public void setDisplayFolderTree(boolean isDisplayed)
  {
    _displayFolderTree = isDisplayed;
  }

  /**
   * Gets whether the sender column is displayed
   */
  public boolean getDisplaySenderColumn()
  {
    return _displaySenderColumn;
  }

  /**
   * Sets whether the sender column is displayed
   */
  public void setDisplaySenderColumn(boolean isDisplayed)
  {
    _displaySenderColumn = isDisplayed;
  }

  /**
   * Gets whether the date column is displayed
   */
  public boolean getDisplayDateColumn()
  {
    return _displayDateColumn;
  }

  /**
   * Sets whether the date column is displayed
   */
  public void setDisplayDateColumn(boolean isDisplayed)
  {
    _displayDateColumn = isDisplayed;
  }

  /**
   * Gets whether the size column is displayed
   */
  public boolean getDisplaySizeColumn()
  {
    return _displaySizeColumn;
  }

  /**
   * Sets whether the size column is displayed
   */
  public void setDisplaySizeColumn(boolean isDisplayed)
  {
    _displaySizeColumn = isDisplayed;
  }

  /**
   * Gets whether the user wants to preview the email message underneath
   * the message list
   */
  public boolean isPreviewMessageMode()
  {
    return _isPreviewMessageMode;
  }

  /**
   * Sets whether the user wants to preview the email message underneath
   * the message list
   */
  public void setPreviewMessageMode(boolean enable)
  {
    _isPreviewMessageMode = enable;
  }

  /**
   * Sets the AccessibilityMode for this user.
   */
  public void setAccessibilityMode(String accessibilityMode)
  {
    _accessibilityMode = accessibilityMode;
  }

  /**
   * Sets the branding.  Must be one of: REGULAR_BRANDING,
   * MEDIUM_BRANDING, or SMALL_BRANDING.
   */
  public void setBranding(String branding)
  {
    // assert (REGULAR_BRANDING.equals(branding) ||
  //    MEDIUM_BRANDING.equals(branding)  ||
  //    SMALL_BRANDING.equals(branding));

    _branding = branding;
  }

  /**
   * Sets the number of rows to show in the messages table.
   */
  public void setRowsShown(int rowsShown)
  {
    // assert rowsShown > 0;

    if (rowsShown > 0)
      _rowsShown = rowsShown;
  }

  private String  _accessibilityMode;  // The accessibility mode
  private String  _branding;          // The branding type
  private String  _skinFamily;          // The skin-family
  private int     _rowsShown;         // How many rows to show
  private boolean _displayFolderTree;
  private boolean _displaySenderColumn;
  private boolean _displayDateColumn;
  private boolean _displaySizeColumn;
  private boolean _isPreviewMessageMode = false;

  private static final int _DEFAULT_ROWS_SHOWN = 25;

  private static final List<SelectItem> _ACCESSIBILITY_MODES = new ArrayList<SelectItem>();
  static
  {
    _ACCESSIBILITY_MODES.add(new SelectItem("default", "Default"));
    _ACCESSIBILITY_MODES.add(new SelectItem("inaccessible", "None"));
    _ACCESSIBILITY_MODES.add(new SelectItem("screenReader", "Screen Readers"));
  }

  private static final List<SelectItem> _SKIN_FAMILIES = new ArrayList<SelectItem>();
  static
  {
    _SKIN_FAMILIES.add(new SelectItem("oracle", "Oracle"));
    _SKIN_FAMILIES.add(new SelectItem("minimal", "Minimal"));
  }
}
