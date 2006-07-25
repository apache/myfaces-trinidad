/*
 * Copyright  2000-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfinternal.renderkit.core.skin;

import org.apache.myfaces.adfinternal.skin.Skin;

/**
 *  Base Skin implementation
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/BaseSkin.java#0 $) $Date: 10-nov-2005.19:02:50 $
 * @author The Oracle ADF Faces Team
 */
public class BaseSkin extends Skin
{
  public BaseSkin()
  {
  }

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  public String getStyleSheetName()
  {
    return null;
  }

  protected String getBundleName()
  {
    return null;
  }
}
