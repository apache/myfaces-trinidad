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
package org.apache.myfaces.adfinternal.ui.laf.base.pda;

import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafUtils;

import org.apache.myfaces.adfinternal.util.IntegerUtils;
import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;

import org.apache.myfaces.adfinternal.ui.laf.LookAndFeelManager;
import org.apache.myfaces.adfinternal.ui.laf.NameAndAgentScorer;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PdaHtmlLafUtils.java#0 $) $Date: 10-nov-2005.18:55:02 $
 * @author The Oracle ADF Faces Team
 */
public class PdaHtmlLafUtils extends XhtmlLafUtils
{
  protected PdaHtmlLafUtils()
  {
  }

  /**
   * Registers the pda html PC Look And Feel with the specified
   * LookAndFeelManager.
   */
  public static void registerLookAndFeel(
    LookAndFeelManager manager
    )
  {
    manager.registerLookAndFeel(_SCORER, new PdaHtmlLookAndFeel());
  }

  private static final NameAndAgentScorer _SCORER =
    new NameAndAgentScorer(null,
                           IntegerUtils.getInteger(AdfFacesAgent.TYPE_PDA),
                           null,
                           null,
                           null);
}
