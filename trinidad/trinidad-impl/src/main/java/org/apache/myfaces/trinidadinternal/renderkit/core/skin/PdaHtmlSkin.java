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
package org.apache.myfaces.trinidadinternal.renderkit.core.skin;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.trinidadinternal.skin.icon.NullIcon;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

/**
 * Skin implementation for HTML browsers
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/PdaHtmlSkin.java#0 $) $Date: 10-nov-2005.19:02:55 $
 * @author The Oracle ADF Faces Team
 */
public class PdaHtmlSkin extends XhtmlSkin
{

  public PdaHtmlSkin()
  {
    // Register customizable Icons
    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
    _registerSkinProperties();
  }



  private void _registerSkinProperties()
  {    
    setProperty(SkinProperties.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY, Boolean.FALSE);
    setProperty(SkinProperties.AF_PANELHEADER_INDENT_CONTENT, Boolean.FALSE);
    setProperty(SkinProperties.AF_PANEL_LIST_DEFAULT_COLUMNS, IntegerUtils.getInteger(2));
  }

  // This array contains entries for all of the customizable
  // org.apache.myfaces.trinidadinternal.skin.icon.Icons for PdaHtmlSkin
  // and subclasses of PdaHtmlSkin.
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // null out icons that have a default in base skin
    SkinSelectors.ERROR_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),
    SkinSelectors.WARNING_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),
    SkinSelectors.INFO_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME,
      new TranslatedTextIcon("af_showDetailHeader.DISCLOSED"),
      SkinSelectors.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME,
      new TranslatedTextIcon("af_showDetailHeader.UNDISCLOSED"),
      
   // define icons
    SkinSelectors.AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/dp.gif", 
                           "adf/images/dprtl.gif",
                           IntegerUtils.getInteger(17),
                           IntegerUtils.getInteger(18)),
                            
    SkinSelectors.AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME,
      new ContextImageIcon("adf/images/lovi.gif", 
                           "adf/images/lovirtl.gif",
                           IntegerUtils.getInteger(18),
                           IntegerUtils.getInteger(18)), 
    // @todo these need to be green and in adf/images/lovi (these are for
    // OraclePdaSkin, but we are sharing for now.
    SkinSelectors.AF_COLUMN_UNSORTED_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrt.gif", 
                           null,
                           IntegerUtils.getInteger(16),
                           IntegerUtils.getInteger(7)), 
    SkinSelectors.AF_COLUMN_SORTED_ASCEND_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrta.gif", 
                           null, 
                           IntegerUtils.getInteger(9),
                           IntegerUtils.getInteger(9)),    
    SkinSelectors.AF_COLUMN_SORTED_DESCEND_ICON_NAME,
      new ContextImageIcon("adf/images/oracle/msrtd.gif", 
                           null, 
                           IntegerUtils.getInteger(9),
                           IntegerUtils.getInteger(9)),    

  };

}
