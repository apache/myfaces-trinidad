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
package org.apache.myfaces.trinidadexhibition.webapp;



/**
 *
 * @author Andrew Robinson
 */
public class PreferencesBean
{
  private String _skinFamily = "demo-skin";
  private String _accessibilityMode;
  
  /**
   * @return the skinFamily
   */
  public String getSkinFamily()
  {
    return _skinFamily;
  }
  
  /**
   * @param skinFamily the skinFamily to set
   */
  public void setSkinFamily(String skinFamily)
  {
    _skinFamily = skinFamily;
  }

  /**
   * @return the accessibilityMode
   */
  public String getAccessibilityMode()
  {
    return _accessibilityMode;
  }

  /**
   * @param accessibilityMode the accessibilityMode to set
   */
  public void setAccessibilityMode(String accessibilityMode)
  {
    _accessibilityMode = accessibilityMode;
  }
}
