/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.awt.Color;

import org.apache.myfaces.trinidadinternal.style.util.FontProxy;

import org.apache.myfaces.trinidadinternal.image.ImageProviderRequest;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * ButtonRenderer for Simple Look And Feel.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/ButtonRenderer.java#0 $) $Date: 10-nov-2005.18:51:20 $
 * @author The Oracle ADF Faces Team
 */
public class ButtonRenderer 
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.ButtonRenderer
{
  /**
   * Tests whether the button should be rendered as an image.
   */
  protected boolean doRenderImageContent(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Check with superclass first (screen reader mode).
    if (!super.doRenderImageContent(context, node))
      return false;

    // We only render buttons as images if we have all
    // of the button icons. 
    return SimpleButtonUtils.doRenderImageButton(context);
  }

  /**
   * Creates the ImageProviderRequest to use when looking up the
   * button image.
   */ 
  protected ImageProviderRequest createImageProviderRequest(
    UIXRenderingContext context,
    Object       name,
    Object       text,
    Color        foreground,
    Color        background,
    Color        surroundingColor,
    FontProxy    font,
    boolean      disabled,
    boolean      textAntialias,
    boolean      startRounded,
    boolean      endRounded,
    char         accessKey
    )
  {
    return SimpleButtonUtils.createButtonRequest(
                        context,
                        (name != null)
                          ? name.toString()
                          : null,
                        (text != null)
                          ? text.toString()
                          : null,
                        foreground,
                        background,
                        surroundingColor,
                        font,
                        disabled,
                        textAntialias,
                        accessKey);
  }

  /**
   * Returns the name of the server-side style for styling
   * button text.
   */
  protected String getServerStyleName(
    UIXRenderingContext context,
    UINode           node,
    boolean          disabled
    )
  {
    return SimpleButtonUtils.getButtonStyleName(disabled);
  }
}
  
