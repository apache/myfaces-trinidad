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

package org.apache.myfaces.adfinternal.skin.icon;

import java.io.IOException;
import java.util.Map;

import javax.faces.context.FacesContext;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

/**
 * An Icon implementation that references another icon by name.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/ReferenceIcon.java#0 $) $Date: 10-nov-2005.18:59:04 $
 * @author The Oracle ADF Faces Team
 */
public class ReferenceIcon extends Icon
{
  /**
   * 
   */
  public ReferenceIcon(String name)
  {
    _name = name;
  }
  
  public void renderIcon(
    FacesContext        context,
    AdfRenderingContext arc,
    Map              attrs
    ) throws IOException
  {
    // do nothing.
  }
  
  public String getName()
  {
    return _name;
  }  
  private String _name;
}
