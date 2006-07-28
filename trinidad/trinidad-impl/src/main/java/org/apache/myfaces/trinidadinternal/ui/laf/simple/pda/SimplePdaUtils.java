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
package org.apache.myfaces.trinidadinternal.ui.laf.simple.pda;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.trinidadinternal.ui.laf.NameAndAgentScorer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.pda.PdaHtmlLafUtils;

import org.apache.myfaces.trinidadinternal.util.IntegerUtils;



/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/pda/SimplePdaUtils.java#0 $) $Date: 10-nov-2005.18:50:47 $
 * @author The Oracle ADF Faces Team
 */
public class SimplePdaUtils extends PdaHtmlLafUtils
{

  private SimplePdaUtils()
  {
  }

  /**
   * Registers the Pocket PC Look And Feel with the specified
   * LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    SimplePdaLookAndFeel laf = new SimplePdaLookAndFeel();
    manager.registerLookAndFeel(_PDA_SCORER, laf);
  }

  private static final NameAndAgentScorer _PDA_SCORER =
    new NameAndAgentScorer("simple",
                           IntegerUtils.getInteger(TrinidadAgent.TYPE_PDA),
                           null,
                           null,
                           null);


}
