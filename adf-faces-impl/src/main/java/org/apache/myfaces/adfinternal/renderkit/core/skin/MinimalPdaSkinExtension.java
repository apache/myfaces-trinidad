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
import org.apache.myfaces.adfinternal.skin.SkinExtension;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafConstants;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/MinimalPdaSkinExtension.java#0 $) $Date: 10-nov-2005.19:02:52 $
 * @author The Oracle ADF Faces Team
 */
public class MinimalPdaSkinExtension extends SkinExtension
{

  /**
   * Creates an MinimalPdaSkinExtension instance which extends
   * the specified base Skin. (should be SimplePdaSkin)
   */
  public MinimalPdaSkinExtension(Skin baseSkin)
  {
     // Create a SkinExtension for Minimal
    super(baseSkin,
          _MINIMAL_PDA_ID,
          _MINIMAL_FAMILY,
          XhtmlLafConstants.ORACLE_ADF_PDA);

    // Register our style sheet
    setStyleSheetName(_MINIMAL_STYLE_SHEET_NAME);

  }

  // Minimal skin family name
  private static final String _MINIMAL_FAMILY = "minimal";
  private static final String _MINIMAL_PDA_ID = "minimal.pda";
  // Minimal skin style sheet name
  private static final String _MINIMAL_STYLE_SHEET_NAME =
    "META-INF/adf/styles/minimal-pda.xss";
}
