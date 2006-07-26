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
package org.apache.myfaces.adfinternal.style.util;

import org.apache.myfaces.adfinternal.style.StyleContext;

/**
 * Utils class for styleSheet mode support
 * 
 * @author The Oracle ADF Faces Team
 * 
 */
public class ModeUtils
{
  public static final int MODE_DEFAULT = 0;

  public static final int MODE_STANDARDS = 1;

  public static final int MODE_QUIRKS = 2;

  public static final String MODE_STANDARD_NAME = "s";

  public static final String MODE_QUIRKS_NAME = "q";

  public static final String STANDARD_MODE = "standards";

  public static final String QUIRKS_MODE = "quirks";

  /**
   * returns the current mode,standards or quirks
   * 
   * @param context
   * @return
   */
  public static String getCurrentMode(StyleContext context)
  {
    if (context.disableStandardsMode())
      return QUIRKS_MODE;
    else
      return STANDARD_MODE;
  }
}
