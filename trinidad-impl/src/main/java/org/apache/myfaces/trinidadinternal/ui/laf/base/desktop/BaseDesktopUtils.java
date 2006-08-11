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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.trinidadinternal.ui.laf.NameAndAgentScorer;
import org.apache.myfaces.trinidadinternal.ui.laf.Score;
import org.apache.myfaces.trinidadinternal.ui.laf.ScoreProxy;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/BaseDesktopUtils.java#0 $) $Date: 10-nov-2005.18:55:09 $
 * @author The Oracle ADF Faces Team
 */
public class BaseDesktopUtils extends XhtmlLafUtils
{
  /**
   * Registers the desktop implementation of the Base Look And Feel
   *  with the specified
   * LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    BaseDesktopLookAndFeel laf = new BaseDesktopLookAndFeel();

    // Don't register for netscape 4, as it's not supported
    manager.registerLookAndFeel(_SCORER, laf);
  }

  protected BaseDesktopUtils()
  {
  }

  //
  // "PRIVATE" RENDERER NAMES
  //

  /**
   * Returns true if the agent supports partial rendering of content.
   */
  public static boolean supportsPartialRendering(
    UIXRenderingContext context
    )
  {
    return HtmlLafRenderer.supportsPartialRendering(context);
  }

  // We use a scorer which matches base/desktop - but not Netscape
  private static final NameAndAgentScorer _SCORER =
    new NoNetscapeScorer("base");

  private static class NoNetscapeScorer extends NameAndAgentScorer
  {
    public NoNetscapeScorer(String lafName)
    {
      // Initialize the NameAndAgentScorer for base/desktop
      super(lafName,
            IntegerUtils.getInteger(TrinidadAgent.TYPE_DESKTOP),
            null,
            null,
            null);
    }

    @Override
    public Score scoreLookAndFeel(
      UIXRenderingContext context,
      String           lafName
      )
    {
      Score score = super.scoreLookAndFeel(context, lafName);

      // We don't support Netscape 4.x
      if (TrinidadAgent.APPLICATION_NETSCAPE ==
          context.getAgent().getAgentApplication())
      {
        score = new ScoreProxy(score)
                    {
                      @Override
                      public int getAgentApplicationScore()
                      {
                        return Score.NO_MATCH;
                      }
                    };
      }

      return score;
    }
  }
}
