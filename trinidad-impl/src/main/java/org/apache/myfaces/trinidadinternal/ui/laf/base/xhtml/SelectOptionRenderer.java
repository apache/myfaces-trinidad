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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;



import org.apache.myfaces.trinidadinternal.agent.AdfFacesAgent;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/SelectOptionRenderer.java#0 $) $Date: 10-nov-2005.18:54:12 $
 * @author The Oracle ADF Faces Team
 */
public class SelectOptionRenderer
       extends OptionContainerRenderer.OptionRenderer
{
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
  

    boolean agentSupportsDisabledOptions = Boolean.TRUE
        .equals(getAgentCapability(context,
            AdfFacesAgent.CAP_SUPPORTS_DISABLED_OPTIONS));
    if (!(agentSupportsDisabledOptions))
    {
      
      boolean isReadOnly = BaseLafUtils.getLocalBooleanAttribute(context, 
                                                  node, 
                                                  READ_ONLY_ATTR, 
                                                  false);
      boolean isDisabled = BaseLafUtils.getLocalBooleanAttribute(context, 
                                                  node, 
                                                  DISABLED_ATTR, 
                                                  false);
                                                  
       if(!render(context, node, isDisabled, isReadOnly))
         return;
    }
    
    if (renderAsElement(context, node))
    {
      super.render(context, node);
    }
    else
    {
      renderAsNonElement(context, node);
    }
  }

  public boolean render(
    UIXRenderingContext context,
    UINode           node,
    boolean          isDisabled,
    boolean          isReadOnly
    ) throws IOException  
  {                                             
      if (isReadOnly || isDisabled)
        return false;
      return true;
  }

  protected void renderAsNonElement(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    if (isOptionSelected(context, node))
    {
      renderStyledText(context, node);
    }
  }

  protected boolean doRenderStyleAttrs(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return false;
  }

  protected void renderContent(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    renderText(context, node);
  }

  protected void renderSelectedAttribute(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderAttribute(context,
                    SELECTED_ATTRIBUTE,
                    Boolean.valueOf(isOptionSelected(context, node)));
  }

  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return OPTION_ELEMENT;
  }
}
