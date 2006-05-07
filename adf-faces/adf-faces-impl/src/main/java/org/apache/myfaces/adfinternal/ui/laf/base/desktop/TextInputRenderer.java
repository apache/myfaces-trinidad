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
package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/TextInputRenderer.java#0 $) $Date: 10-nov-2005.18:56:20 $
 * @author The Oracle ADF Faces Team
 */
public class TextInputRenderer 
                        extends org.apache.myfaces.adfinternal.ui.laf.base.xhtml.TextInputRenderer
{
  protected Object getOnFocus(
    RenderingContext context,
    UINode           node
    )
  {
    if (_shouldScriptReadOnly(context, node) &&
        Boolean.TRUE.equals(getReadOnly(context, node)))

    {
      // chain together the JS we need for readonly 
      // with any JS used by the client
      return BaseDesktopUtils.getChainedJS(_READ_ONLY_SCRIPT,
                                       super.getOnFocus(context, node),
                                       true);
    }
    
    return super.getOnFocus(context, node);
  }
  
  
  /**
   * Returns true if the passed in columns should be shrunken when displaying
   */
  protected boolean shrinkColumns(
    RenderingContext context
    )
  {
    AdfFacesAgent agent = context.getAgent();

    boolean isNetscape = HtmlLafRenderer.isNetscape(context);

    // Internet Explorer has a fairly reasonable size; Netscape is
    // on the big size.  3/5 works well for text inputs,
    // and 3/4 for text areas.  Also, the heuristic only
    // appears to work on Windows.  (Mozilla was once on the large
    // size, but as of 1.0 seems good).
    return (isNetscape && (agent.getAgentOS() == AdfFacesAgent.OS_WINDOWS));
  }

  protected boolean renderReadOnlyAsElement(
    RenderingContext context,
    UINode           node)
  {
    return (super.renderReadOnlyAsElement(context, node) ||
            _shouldScriptReadOnly(context, node));
  }

  private boolean _shouldScriptReadOnly(
    RenderingContext context,
    UINode           node)
  {
    return  (isTextArea(context,node) &&
             !supportsReadOnlyFormElements(context) &&
             supportsScripting(context));
  }

  private static final String _READ_ONLY_SCRIPT = "this.blur()";
}
