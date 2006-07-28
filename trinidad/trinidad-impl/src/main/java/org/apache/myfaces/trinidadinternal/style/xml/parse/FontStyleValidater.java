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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;

/**
 * PropertyValidater for font-style values
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/FontStyleValidater.java#0 $) $Date: 10-nov-2005.18:58:06 $
 * @author The Oracle ADF Faces Team
 */
class FontStyleValidater implements PropertyValidater
{
  /**
   * Tests whether the specified value is valid for the given property 
   * name.  If the property is not valid, an error message is returned.
   * Otherwise, null is returned to indicate that the value is valid.
   */
  public String validateValue(String name, String value)
  {
    try
    {
      CSSUtils.parseFontStyle(value);
    }
    catch (PropertyParseException e)
    {
      return e.getMessage();
    }

    return null;
  }
}
