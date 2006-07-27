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

import java.util.Map;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.component.UIXCommand;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/CommandItemRenderer.java#0 $) $Date: 10-nov-2005.18:53:46 $
 * @author The Oracle ADF Faces Team
 */
public class CommandItemRenderer extends SelectOptionRenderer
{
  public boolean render(
    UIXRenderingContext context,
    UINode           node,
    boolean          isDisabled,
    boolean          isReadOnly
    ) throws IOException  
  {                                             
      if (isReadOnly || isDisabled)
      {
        if (isOptionSelected(context, node))
          return true;
        return false;
      }
      return true;
  }


    protected void renderValue(
      UIXRenderingContext context,
      UINode           node,
      Object           value
    )throws IOException
    {         
    

      boolean agentSupportsDisabledOptions = Boolean.TRUE
          .equals(getAgentCapability(context,
              TrinidadAgent.CAP_SUPPORTS_DISABLED_OPTIONS));
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

        if (isReadOnly || isDisabled)
          return;
      }    
    
      UIComponent component = node.getUIComponent() ;
      Map attributes = component.getAttributes();
      Object destination = node.getAttributeValue(context, DESTINATION_ATTR);  
      
      if ( destination != null)
      {
        value = '#' + encodeURL(context, destination);
      }
      else if ( value != null)
      {
        boolean immediate = Boolean.TRUE.equals( 
                                    attributes.get(UIXCommand.IMMEDIATE_KEY)); 
        String validate = immediate?"0":"1";
        value = value.toString() + '[' + validate + ']';
      }
      
      renderAttribute(context, VALUE_ATTRIBUTE, value);
    }
 
}
