/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderer;

/**
 * Utilities for working with access keys.
 */
public class AccessKeyUtils
{
  /**
   * Renders the text with the access key highlighted with an underline.
   */
  static public void renderAccessKeyText(
    FacesContext  context,
    Object        textValue,
    int           keyIndex
    ) throws IOException
  {
    renderAccessKeyText(context, textValue, keyIndex, "u");
  }

  /**
   * Renders the text with the access key highlighted as appropriate.
   */
  static public void renderAccessKeyText(
    FacesContext  context,
    Object        textValue,
    int           keyIndex,
    String        highlightElement
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    if ((textValue != null) && (keyIndex != -1))
    {
      String textString = textValue.toString();
      
      char[] textChars = textString.toCharArray();
      
      // write text before the mnemonic
      writer.writeText(textChars, 0, keyIndex);
      
      writer.startElement(highlightElement, null);
      writer.writeText(textChars, keyIndex, 1);
      writer.endElement(highlightElement);
      
      // write text after the mnemonic
      keyIndex++;
      
      int charsLeft = textChars.length - keyIndex;
      
      if (charsLeft > 0)
      {
        writer.writeText(textChars, keyIndex, charsLeft);
      }
    }
    else
    {
      // output the text directly since we have no access key
      writer.writeText(textValue, null);
    }
  }

  /**
   * Renders the text with the access key highlighted as appropriate.
   */
  static public void renderAccessKeyText(
    FacesContext context,
    Object       textValue,
    char         accessKey,
    String        highlightElement
    ) throws IOException
  {
    Object textString = (textValue != null)
                          ? textValue.toString()
                          : null;

    renderAccessKeyText(context,
                        textString,
                        getAccessKeyIndex(textString, accessKey),
                        highlightElement);
  }


  /**
   * Returns the index of the access key in the specified text.
   */
  public static int getAccessKeyIndex(
    Object textValue,
    char   accessChar
    )
  {
    int keyIndex = -1;

    if ((textValue != null) &&
        (accessChar != CoreRenderer.CHAR_UNDEFINED))
    {
      String textString = textValue.toString();
      
      // underline the first instance of the access key in the text
      keyIndex = textString.indexOf(accessChar);
      
      // try the key in the opposite case if there was no match
      if (keyIndex == -1)
      {
        char oppositeChar = Character.toLowerCase(accessChar);
        
        if (oppositeChar == accessChar)
        {
          oppositeChar = Character.toUpperCase(accessChar);
        }
        
        if (oppositeChar != accessChar)
        {
          keyIndex = textString.indexOf(oppositeChar);
        }
      }
    }

    return keyIndex;
  }
}
