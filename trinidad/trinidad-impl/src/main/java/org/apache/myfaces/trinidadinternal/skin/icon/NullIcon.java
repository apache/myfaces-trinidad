/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.skin.icon;

import java.util.Map;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;

/**
 * Icon class for a completely empty, null icon.
 */
public class NullIcon extends Icon
{
  static public Icon sharedInstance()
  {
    return _ICON;
  }

  public void renderIcon(
    FacesContext        context,
    AdfRenderingContext arc,
    Map              attrs
    )
  {
    // null icons don't render anything
  }

  public boolean isNull()
  {
    return true;
  }

  private NullIcon()
  {
  }

  static private final NullIcon _ICON = new NullIcon();
}

