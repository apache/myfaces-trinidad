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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/HideShowUtils.java#0 $) $Date: 10-nov-2005.18:55:20 $
 * @author The Oracle ADF Faces Team
 */
public class HideShowUtils implements BaseDesktopConstants
{ 
  /**
   * Render the Hide/Show disclosed state symbol. This method also renders the
   * translated alt text 
   * @param context the current context
   * @param renderer the Renderer that is doing the rendering
   * @param disclosed if true the disclosed (hide) symbol is shown. If false
   * the undisclosed (show) symbol is shown.
   */
  public static void renderDisclosedStateSymbol(
    RenderingContext context,
    boolean disclosed,
    String disclosedAltTextKey,
    String undisclosedAltTextKey) throws IOException
  {
    Icon icon = _getHideShowIcon(context, disclosed);

    if (icon != null)
    {
      // Get the alt text
      String key = disclosed ? disclosedAltTextKey : undisclosedAltTextKey;
      Object altText = HtmlLafRenderer.getTranslatedValue(context, key);

      // Get the align
      Object align = BaseDesktopUtils.getMiddleIconAlignment(context);

      // Render the icon with the specified attrs
      BaseDesktopUtils.renderIcon(context, icon, altText, align);
    }
  }


  // =-=gc should this be the way the hideShowHeader works?
  // get property as to whether to render hideShow as inline rather
  // than block level element
  public static Boolean getIsInline(
    RenderingContext context
  )
  {
    return (Boolean)context.getProperty(UIConstants.MARLIN_NAMESPACE, 
                                        _IS_INLINE_PROPERTY);
  }

  // =-=gc should this be the way the hideShowHeader works?
  // Set property to say to render hideShow as inline rather
  // than block level element
  public static void setIsInline(
    RenderingContext context,
    Boolean          isInline
  )
  {    
    context.setProperty( UIConstants.MARLIN_NAMESPACE,
                         _IS_INLINE_PROPERTY,
                         isInline);
  }    

  // Returns the hideShow Icon
  private static Icon _getHideShowIcon(
    RenderingContext context,
    boolean          disclosed
    )
  {
    String iconName = (disclosed) ? AF_SHOW_DETAIL_DISCLOSED_ICON_NAME :
                                    AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME;
    
    return context.getIcon(iconName);
  }

  private static final Object _IS_INLINE_PROPERTY = new Object();  
}
