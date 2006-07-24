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
package org.apache.myfaces.adfinternal.ui.laf.base.pda;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.component.UIXHierarchy;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.ModelRendererUtils;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.adfinternal.util.IntegerUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/ProcessTrainRenderer.java#0 $) $Date: 10-nov-2005.18:55:03 $
 * @author The Oracle ADF Faces Team
 */
public class ProcessTrainRenderer extends XhtmlLafRenderer
{

 protected UIXHierarchy getHierarchyBase(
    RenderingContext context,
    UINode           node
  )
  {
    return (UIXHierarchy) node.getUIComponent();  
  }


  protected UINode getStamp(
    RenderingContext context,
    UINode           node
    )
  {
    return node.getNamedChild(context, NODE_STAMP_CHILD);
  }
  

  protected boolean setNewPath(
    RenderingContext context,
    UINode           node,
    UIXHierarchy    component
  )
  {
    int startDepth = getIntAttributeValue(context, node, START_DEPTH_ATTR, 0);
    return ModelRendererUtils.setNewPath(component, startDepth, 
                                         component.getFocusRowKey(), false); 
    
   
  }

  /**
   *  Render Step x of z for process Train components.
   */
  protected void renderContent(
          RenderingContext context,
          UINode           node
          ) throws IOException
  {

    UIXHierarchy component = getHierarchyBase(context, node);          
    UINode stamp = getStamp(context, node);
    
    if(stamp != null)
    { 
      Object oldPath = component.getRowKey();      
      boolean isNewPath = setNewPath(context, node, component);
      if (isNewPath)
      {

        int selectedIndex = component.getRowIndex();
        int length = component.getRowCount();      
        String pattern;
        String[] parameters;
        
        selectedIndex++; 
        
        if (length == MAX_VALUE_UNKNOWN)
        {
          pattern = getTranslatedString(context,
                  _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING);
  
          parameters = new String[]
          {
            getTranslatedString(context, _STEP_TEXT_KEY),
            IntegerUtils.getString(selectedIndex)
          };
          
        }
        else
        {
          pattern = getTranslatedString(context,
                  _SINGLE_RANGE_FORMAT_TOTAL_STRING);
  
          parameters = new String[]
          {
            getTranslatedString(context, _STEP_TEXT_KEY),
            IntegerUtils.getString(selectedIndex),
            IntegerUtils.getString(length)
          };
        }

        ResponseWriter writer = context.getResponseWriter();

        writer.writeText(formatString(context, pattern, parameters), null);
        
        component.setRowKey(oldPath);
      }
    }
  }


  protected String getElementName(
          RenderingContext context,
          UINode           node
          )
  {
    return null;
  }


  static private final String _STEP_TEXT_KEY = 
    "af_processTrain.STEP";
  static private final String _SINGLE_RANGE_FORMAT_TOTAL_STRING =
    "af_processTrain.FORMAT_TOTAL";
  static private final String _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING =
    "af_processTrain.FORMAT_NO_TOTAL";
}
