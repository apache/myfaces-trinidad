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
package org.apache.myfaces.trinidadinternal.renderkit.core.pages;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.nav.CoreGoButton;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

/**
 * Utility methods for code shared among JSPs.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/JspUtils.java#0 $) $Date: 10-nov-2005.19:03:35 $
 */
class JspUtils
{
  static public CoreGoButton createGoButton(
    RenderingContext arc,
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
    String encoding = (String)context.getExternalContext().getRequestParameterMap().get("enc");
    
    // verify that the encoding doesn't contain CRLF header delimiters as this could
    // allow additional headers to per attached to a request on Servlet Engines
    // that do not correctly validate the input to Response.setContentType();
    if (encoding != null)
    {      
      if ((encoding.indexOf('\r') >= 0) ||
          (encoding.indexOf('\n') >= 0))
        throw new IllegalArgumentException("Encoding parameter contains line/header delimiters");
    }
    
    return encoding;
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
