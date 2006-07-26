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
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;

/** 
 * Used to provide contextual information like orientation/selectedIndex/
 *  unvalidated, which is relevant during rendering of subTabBar.
 * Normally used by showOneTab to communicate with subTabBar
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/SubTabBarUtils.java#0 $) $Date: 10-nov-2005.18:56:18 $
 * @author The Oracle ADF Faces Team
 */
public class SubTabBarUtils implements UIConstants
{
  private SubTabBarUtils()
  {
  }

  public static Object getOrientation(
    RenderingContext context
  )
  {
    return context.getProperty( MARLIN_NAMESPACE,
                                _SUB_TAB_BAR_ORIENTATION_PROPERTY);
  }

  public static void setOrientation(
    RenderingContext context,
    Object           orientation
  )
  {    
    context.setProperty( MARLIN_NAMESPACE,
                         _SUB_TAB_BAR_ORIENTATION_PROPERTY,
                         orientation);
  }
  
  public static Object getSelectedIndex(
    RenderingContext context
  )
  {
    return context.getProperty( MARLIN_NAMESPACE,
                                _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY);
  }
  
  public static void setSelectedIndex(
    RenderingContext context,
    Object selectedIndex)
  {
    context.setProperty( MARLIN_NAMESPACE,
                         _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY,
                         selectedIndex);
  }

  public static Object isUnvalidated(
    RenderingContext context
  )
  {
    return context.getProperty( MARLIN_NAMESPACE,
                                _SUB_TAB_BAR_UNVALIDATED_PROPERTY);
  }
  
  public static void setUnvalidated(
    RenderingContext context,
    Object isUnvalidated)
  {
    context.setProperty( MARLIN_NAMESPACE,
                         _SUB_TAB_BAR_UNVALIDATED_PROPERTY,
                         isUnvalidated);
  }

  private static final Object _SUB_TAB_BAR_SELECTED_INDEX_PROPERTY = new Object();
  private static final Object _SUB_TAB_BAR_ORIENTATION_PROPERTY = new Object();  
  private static final Object _SUB_TAB_BAR_UNVALIDATED_PROPERTY = new Object();  
}
