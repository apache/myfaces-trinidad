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

package org.apache.myfaces.trinidadinternal.renderkit.core.skin;



import java.io.IOException;
import java.util.Map;

import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;

/**
 * An Icon implementation which switches between a Mac OS-specific
 * Icon and a default Icon.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/MacOSSwitcherIcon.java#0 $) $Date: 10-nov-2005.19:02:51 $
 * @author The Oracle ADF Faces Team
 */
class MacOSSwitcherIcon extends Icon
{
  public MacOSSwitcherIcon(
    Icon icon,
    Icon macOSIcon
    )
  {
    if ((icon == null)||(macOSIcon == null))
    {
      throw new NullPointerException("Null argument");
    }

    _icon = icon;
    _macOSIcon = macOSIcon;
  }

  /**
   * Override of Icon.renderIcon().
   */
  @Override
  public void renderIcon(
    FacesContext        context,
    RenderingContext    arc,
    Map<String, Object> attrs
    ) throws IOException
  {
    Icon icon = _getIcon(arc);

    icon.renderIcon(context, arc, attrs);
  }

  /**
   * Override of Icon.getImageURI().
   */
  @Override
  public Object getImageURI(
    FacesContext        context,
    RenderingContext arc)
  {
    Icon icon = _getIcon(arc);

    return icon.getImageURI(context, arc);
  }

  /**
   * Override of Icon.getImageWidth().
   */
  @Override
  public Integer getImageWidth(RenderingContext arc)
  {
    Icon icon = _getIcon(arc);

    return icon.getImageWidth(arc);
  }

  /**
   * Override of Icon.getImageHeight().
   */
  @Override
  public Integer getImageHeight(RenderingContext arc)
  {
    Icon icon = _getIcon(arc);

    return icon.getImageHeight(arc);
  }

  // Returns the Icon to use
  private Icon _getIcon(RenderingContext arc)
  {
    return (arc.getAgent().getAgentOS() == TrinidadAgent.OS_MACOS) ?
             _macOSIcon :
             _icon;
  }

  private Icon _icon;
  private Icon _macOSIcon;
}
