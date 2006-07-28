/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.skin;


/**
 * Skin implementation for simple pocket pc
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/SimplePdaSkin.java#0 $) $Date: 10-nov-2005.19:02:56 $
 * @author The Oracle ADF Faces Team
 */
public class SimplePdaSkin extends PdaHtmlSkin
{

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  public String getStyleSheetName()
  {
    return "META-INF/adf/styles/simple-pda.xss";
  }

  /**
   * Returns the id for the PDA implementation of the Simple
   * Skin: "simple.pda".
   */
  public String getId()
  {
    return "simple.pda";
  }

  /**
   * Returns the family for the PDA implementation of the Simple
   * Skin: "simple.pda".
   */
  public String getFamily()
  {
    return "simple";
  }

  /**
   * Returns the renderKitId for the PDA implementation of the Simple
   * Skin: "org.apache.myfaces.trinidad.pda".
   */  
  public String getRenderKitId()
  {
    return ORACLE_ADF_PDA;
  }


}
