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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.tablet;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.laf.*;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.BaseDesktopUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop.SimpleDesktopConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.simple.pda.SimplePdaLookAndFeel;

/**
 * Utilities for the desktop implementation of the Simple
 * Look And Feel. Currently the Look And Feel is used only for choosing
 * renderers. We stripped out the styles, icons, translations, and properties,
 * since we now use a Skin to customize that. The goal is to get rid of the 
 * LookAndFeel code altogether.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/SimpleDesktopUtils.java#0 $) $Date: 10-nov-2005.18:51:27 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SimpleTabletUtils extends BaseDesktopUtils
  implements SimpleDesktopConstants
{

  /**
   * Registers the desktop implementation of the Simple
   * Look And Feel with the specified LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    LookAndFeel baseLAF = manager.getLookAndFeelById(_SIMPLE_DESKTOP_ID);

    // The Base LAF should always be pre-registered, but just to
    // be extra safe...
    if (baseLAF == null)
    {
      // No need to log an error message - but an assert
      // should catch our attention
      assert false;

      // We can't create SLAF without the Base LAF
      return;
    }

    // Create a LookAndFeelExtension for SLAF
    LookAndFeelExtension simpleLAF =
      new LookAndFeelExtension(baseLAF, SIMPLE_TABLET_ID, _SIMPLE_FAMILY);

    manager.registerLookAndFeel(_TABLET_SCORER, simpleLAF);
  }

  private static final String _SIMPLE_FAMILY = "simple";
  public static final String _SIMPLE_DESKTOP_ID = "simple.desktop";

  private static final NameAndAgentScorer _TABLET_SCORER =
    new NameAndAgentScorer("simple",
                           null,
                           null,
                           null,
                           TrinidadAgent.TYPE_TABLET);
}
