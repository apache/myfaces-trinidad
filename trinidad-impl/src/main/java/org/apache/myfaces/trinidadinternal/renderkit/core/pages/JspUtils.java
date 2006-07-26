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
package org.apache.myfaces.adfinternal.renderkit.core.pages;

import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.component.core.nav.CoreGoButton;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.util.nls.StringUtils;

/**
 * Utility methods for code shared among JSPs.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/JspUtils.java#0 $) $Date: 10-nov-2005.19:03:35 $
 * @author The Oracle ADF Faces Team
 */
class JspUtils
{
  static public CoreGoButton createGoButton(
    AdfRenderingContext arc,
    String              textKey)
  {
    CoreGoButton button = new CoreGoButton();
    String selectText = arc.getTranslatedString(textKey);
    button.setText(StringUtils.stripMnemonic(selectText));
    char ch = _getMnemonic(selectText);
    if (ch != 0)
      button.setAccessKey(ch);
    return button;
  }

  /**
   * Get the encoding for the page, specifying default if null
   */
  static public String getEncoding(FacesContext context, 
                                   String defaultEncoding)
  {
    String enc = getEncoding(context);
    if (enc == null)
      enc = defaultEncoding;
    return enc;
  }

  /**
   * Get the encoding for the page
   */
  static public String getEncoding(FacesContext context)
  {
    return (String) 
      context.getExternalContext().getRequestParameterMap().get("enc");
  }

  static private char _getMnemonic(String text)
  {
    if (text == null)
      return 0;

    int accessKeyIndex = StringUtils.getMnemonicIndex(text);
    if (accessKeyIndex != StringUtils.MNEMONIC_INDEX_NONE)
    {
      return text.charAt(accessKeyIndex + 1);
    }

    return 0;
  }
}
