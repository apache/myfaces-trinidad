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
package org.apache.myfaces.adfinternal.renderkit.core.skin;

import org.apache.myfaces.adfinternal.ui.laf.base.desktop.BaseDesktopConstants;
import org.apache.myfaces.adfinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.adfinternal.skin.icon.NullIcon;
import org.apache.myfaces.adfinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.adfinternal.skin.icon.TextIcon;

import org.apache.myfaces.adfinternal.util.IntegerUtils;

/**
 * Skin implementation for HTML browsers
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/BaseDesktopSkin.java#0 $) $Date: 10-nov-2005.19:02:49 $
 * @author The Oracle ADF Faces Team
 */
public class BaseDesktopSkin extends XhtmlSkin
  implements BaseDesktopConstants
{
  /**
   * Constructs a BaseDesktopSkin instance
   */
  public BaseDesktopSkin()
  {
    // Register our icons
    // Register our icons
    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
  }

  /**
   * Returns the id for the desktop implementation of the Base
   * Look And Feel: "base.desktop".
   */
  public String getId()
  {
    return BaseDesktopConstants.BASE_DESKTOP_ID;
  }

  /**
   * Returns the family for the Base
   * Look And Feel: "base".
   */
  public String getFamily()
  {
    return "base";
  }


  /**
   * Returns the renderKitId for the BaseDesktopSkin: "org.apache.myfaces.adf.desktop".
   */
  public String getRenderKitId()
  {
    return ORACLE_ADF_DESKTOP;
  }

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  public String getStyleSheetName()
  {
    return "base-desktop.xss";
  }

  // Customizable LAF Icons
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // navigationPath and treeTable path separator icon.
    PATH_SEPARATOR_ICON_ALIAS_NAME,
      new TextIcon("\u00a0\u00a0>\u00a0\u00a0"),

    // ColorField Icons
    AF_SELECT_INPUT_COLOR_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/cfb.gif",
                          "adf/images/cfbr.gif",
                          IntegerUtils.getInteger(24),
                          IntegerUtils.getInteger(24)),

                         
    AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_NAME,
    new ContextImageIcon("adf/images/cfso.gif", 
                         "adf/images/cfsor.gif", 
                         IntegerUtils.getInteger(12),
                          IntegerUtils.getInteger(12), 
                         AF_SELECT_INPUT_COLOR_SWATCH_OVERLAY_ICON_STYLE_CLASS, 
                         null),  
    // DateField Icons
    AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME,
      new ContextImageIcon("adf/images/dfb.gif",
                          "adf/images/dfbr.gif",
                          IntegerUtils.getInteger(19),
                          IntegerUtils.getInteger(24)),

    // GlobalHeader Icons
    AF_MENU_BAR_SEPARATOR_ICON_NAME,
    new TextIcon("\u00a0| "),

    // HideShow Icons
    // (showDetail) defined in OracleDesktopSkinExtension and BaseDesktopSkin
    // used in base.desktop.HideShowRenderer by simple, and pda
    // and oracle.desktop extends it.

    DETAIL_DISCLOSED_ICON_ALIAS_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BC", 
                                        null, 
                                        HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                        null), 
                            new TextIcon("\u2193", 
                                         null, 
                                         HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null)),

    DETAIL_UNDISCLOSED_ICON_ALIAS_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BA", 
                                         "\u25C4", 
                                         HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null), 
                            new TextIcon("\u2192", 
                                         "\u2190", 
                                         HIDE_SHOW_DISCLOSED_SYMBOL_STYLE_CLASS, 
                                         null)),

    AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),

    // af:table icons
    AF_TABLE_SD_DISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    AF_TABLE_SD_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),

    // af:showDetailHeader icons
    AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),

    // af:navigationTree icons
    AF_NAVIGATION_TREE_DISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_DISCLOSED_ICON_ALIAS_NAME),

    AF_NAVIGATION_TREE_UNDISCLOSED_ICON_NAME,
      new ReferenceIcon(DETAIL_UNDISCLOSED_ICON_ALIAS_NAME),
    // HGrid icons
    // (treeTable)
    AF_TREE_TABLE_EXPANDED_ICON_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BC"), 
                            new TextIcon("\u2193")),

    AF_TREE_TABLE_COLLAPSED_ICON_NAME,
      new MacOSSwitcherIcon(new TextIcon("\u25BA", "\u25C4"), 
                            new TextIcon("\u2192", "\u2190")),

    AF_TREE_TABLE_FOCUS_ICON_NAME,
      new TextIcon("X",
                   null,
                   AF_TREE_TABLE_FOCUS_ICON_STYLE_CLASS,
                   null),

    AF_TREE_TABLE_LOCATOR_ICON_NAME,
      new TextIcon("X",
                   null,
                   AF_TREE_TABLE_LOCATOR_ICON_STYLE_CLASS,
                   null),

    // SelectInputText Icons
    AF_SELECT_INPUT_TEXT_BUTTON_ICON_NAME,
      new ContextImageIcon("adf/images/lvib.gif",
                          "adf/images/lvibr.gif",
                          IntegerUtils.getInteger(24),
                          IntegerUtils.getInteger(24)),

    // shuttle's 'Move' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move icon keys reference.
    // shuttle's icons are in BaseDesktopSkin instead of XhtmlSkin because
    // pda renderers don't support shuttle.
    // establish the icon hierarchy here, so that skins that extend this
    // skin can customize both selectOrder/selectManyShuttle very easily
    // by just customizing the alias.
    SHUTTLE_MOVE_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
    new ReferenceIcon(SHUTTLE_MOVE_ICON_ALIAS_NAME),

    AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME,
    new ReferenceIcon(SHUTTLE_MOVE_ICON_ALIAS_NAME),

    // shuttle's 'Move All' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
    new ReferenceIcon(SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME),

    AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME,
    new ReferenceIcon(SHUTTLE_MOVE_ALL_ICON_ALIAS_NAME),

    // shuttle's 'Remove' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SHUTTLE_REMOVE_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
    new ReferenceIcon(SHUTTLE_REMOVE_ICON_ALIAS_NAME),

    AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME,
    new ReferenceIcon(SHUTTLE_REMOVE_ICON_ALIAS_NAME),


    // shuttle's 'Remove All' alias icon that the
    // selectManyShuttle/selectOrderShuttle's move all icon keys reference.
    SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
    new ReferenceIcon(SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME),

    AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME,
    new ReferenceIcon(SHUTTLE_REMOVE_ALL_ICON_ALIAS_NAME),
    
    // SelectOrderShuttle's reorder icons
    AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_UP_ALL"),
    AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_UP"),
    AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_DOWN"),
    AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME,
      new TranslatedTextIcon("af_selectOrderShuttle.REORDER_DOWN_ALL"),    

    // SortableHeader Icons
    AF_COLUMN_SORTED_ASCEND_ICON_NAME,
      new TextIcon("\u25B2",
                   null,
                   SORTABLE_HEADER_SORT_ICON_STYLE_CLASS,
                   null),

    AF_COLUMN_SORTED_DESCEND_ICON_NAME,
      new TextIcon("\u25BC",
                   null,
                   SORTABLE_HEADER_SORT_ICON_STYLE_CLASS,
                   null),

    // for now, pda does not have a separator icon, so we put this definition
    // in BaseDesktopSkin instead of XhtmlSkin.
    AF_SELECT_ONE_TAB_SEPARATOR_ICON_NAME,
    NullIcon.sharedInstance(),    
  };
}
