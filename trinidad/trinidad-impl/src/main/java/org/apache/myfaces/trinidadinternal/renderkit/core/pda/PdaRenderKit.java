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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import org.apache.myfaces.trinidadinternal.renderkit.RenderKitDecorator;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

public class PdaRenderKit extends RenderKitDecorator
{
  public PdaRenderKit()
  {
    addRenderer("org.apache.myfaces.trinidad.Table",
		"org.apache.myfaces.trinidad.Table",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.PdaTableRenderer");
    addRenderer("org.apache.myfaces.trinidad.Process",
                "org.apache.myfaces.trinidad.Train",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.TrainRenderer");
    addRenderer("org.apache.myfaces.trinidad.FrameBorderLayout",
                "org.apache.myfaces.trinidad.FrameBorderLayout",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.PdaFrameBorderLayoutRenderer");
    addRenderer("org.apache.myfaces.trinidad.Frame",
                "org.apache.myfaces.trinidad.Frame",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.FrameRenderer");
    addRenderer("org.apache.myfaces.trinidad.Panel",
                "org.apache.myfaces.trinidad.ButtonBar",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.PanelButtonBarRenderer");
    addRenderer("org.apache.myfaces.trinidad.NavigationLevel",
                "org.apache.myfaces.trinidad.Pane",
                "org.apache.myfaces.trinidadinternal.renderkit.core.pda.PdaNavigationPaneRenderer");
  }

  @Override
  protected String getDecoratedRenderKitId()
  {
    return CoreRenderKit.BASE_RENDER_KIT_ID;
  }

}