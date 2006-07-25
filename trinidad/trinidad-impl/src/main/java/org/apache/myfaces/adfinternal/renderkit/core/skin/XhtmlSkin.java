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

import org.apache.myfaces.adfinternal.skin.icon.ContextImageIcon;
import org.apache.myfaces.adfinternal.skin.icon.NullIcon;
import org.apache.myfaces.adfinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.adfinternal.skin.icon.TextIcon;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafConstants;
import org.apache.myfaces.adfinternal.util.IntegerUtils;



/**
 * Skin implementation for XHTML
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/skin/XhtmlSkin.java#0 $) $Date: 22-nov-2005.14:42:24 $
 * @author The Oracle ADF Faces Team
 */
public class XhtmlSkin extends BaseSkin
                       implements XhtmlLafConstants
{
  public XhtmlSkin()
  {
    super();

    CoreSkinUtils.registerIcons(this, _CUSTOMIZABLE_ICONS);
    _registerSkinProperties();
  }

  /**
   * @todo Move the "BLAF" bundle to a more generic location.
   */
  protected String getBundleName()
  {
    return _BUNDLE_CLASS;
  }

  private void _registerSkinProperties()
  {
    setProperty(AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY,
                Boolean.TRUE);
    setProperty(AF_PANEL_LIST_DEFAULT_COLUMNS, IntegerUtils.getInteger(3));
  }

  // fully qualified class name of our resource bundle
  // Note: this is the same as BLAF!
  private static final String _BUNDLE_CLASS =
    "org.apache.myfaces.adfinternal.renderkit.core.resource.CoreBundle";

  // Customizable LAF Icons
  private static final TextIcon _ERROR_ICON =
    new TextIcon("X",
                 null,
                 ERROR_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _INFO_ICON =
    new TextIcon("i",
                 null,
                 INFO_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _REQUIRED_ICON =
    new TextIcon("*",
                 null,
                 REQUIRED_ICON_STYLE_CLASS,
                 null);

  private static final TextIcon _WARNING_ICON =
    new TextIcon("!",
                 null,
                 WARNING_ICON_STYLE_CLASS,
                 null);


  private static final TextIcon _QUICK_SELECT_ICON =
    new TextIcon("Q",
                 null,
                 QUICK_SELECT_ICON_STYLE_CLASS,
                 null);


  private static final TextIcon _QUICK_SELECT_DISABLED_ICON =
    new TextIcon("Q",
                 null,
                 QUICK_SELECT_DISABLED_ICON_STYLE_CLASS,
                 null);


  // Icons array
  private static final Object[] _CUSTOMIZABLE_ICONS = new Object[]
  {
    // Global Icons. These are alias icons (they are referenced )
    // Someday we'll reference them from the select* components.
    ERROR_ICON_ALIAS_NAME,
    _ERROR_ICON,

    ERROR_ANCHOR_ICON_ALIAS_NAME,
    _ERROR_ICON,

    INFO_ICON_ALIAS_NAME,
    _INFO_ICON,

    INFO_ANCHOR_ICON_ALIAS_NAME,
    _INFO_ICON,

    REQUIRED_ICON_ALIAS_NAME,
    _REQUIRED_ICON,

    WARNING_ICON_ALIAS_NAME,
    _WARNING_ICON,

    WARNING_ANCHOR_ICON_ALIAS_NAME,
    _WARNING_ICON,

    QUICK_SELECT_ICON_NAME,
    _QUICK_SELECT_ICON,

    QUICK_SELECT_DISABLED_ICON_NAME,
    _QUICK_SELECT_DISABLED_ICON,

    PATH_SEPARATOR_ICON_ALIAS_NAME,
    new TextIcon("\u00a0"),

    AF_NAVIGATION_PATH_SEPARATOR_ICON_NAME,
    new ReferenceIcon(PATH_SEPARATOR_ICON_ALIAS_NAME),

    // navigationPath in treeTable
    AF_TREE_TABLE_MP_SEPARATOR_ICON_NAME,
    new ReferenceIcon(PATH_SEPARATOR_ICON_ALIAS_NAME),

    // alias icon for messages's header/panelHeader
    HEADER_ERROR_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    HEADER_WARNING_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    HEADER_INFO_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    HEADER_CONFIRMATION_ICON_ALIAS_NAME,
    NullIcon.sharedInstance(),

    // af:messages header icons point to reference icon so they can
    // be easily shared with af:panelHeader's icons
    AF_MESSAGES_ERROR_ICON_NAME,
    new ReferenceIcon(HEADER_ERROR_ICON_ALIAS_NAME),

    AF_MESSAGES_WARNING_ICON_NAME,
    new ReferenceIcon(HEADER_WARNING_ICON_ALIAS_NAME),

    AF_MESSAGES_INFO_ICON_NAME,
    new ReferenceIcon(HEADER_INFO_ICON_ALIAS_NAME),

    AF_MESSAGES_CONFIRMATION_ICON_NAME,
    new ReferenceIcon(HEADER_CONFIRMATION_ICON_ALIAS_NAME),

    // af:panelHeader header icons point to reference icon so they can
    // be easily shared with af:messages's icons
    AF_PANEL_HEADER_ERROR_ICON_NAME,
    new ReferenceIcon(HEADER_ERROR_ICON_ALIAS_NAME),

    AF_PANEL_HEADER_WARNING_ICON_NAME,
    new ReferenceIcon(HEADER_WARNING_ICON_ALIAS_NAME),

    AF_PANEL_HEADER_INFO_ICON_NAME,
    new ReferenceIcon(HEADER_INFO_ICON_ALIAS_NAME),

    AF_PANEL_HEADER_CONFIRMATION_ICON_NAME,
    new ReferenceIcon(HEADER_CONFIRMATION_ICON_ALIAS_NAME),

     // checkbox icons
    XhtmlLafConstants.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_CHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkdc.gif",
                        null,
                        IntegerUtils.getInteger(12),
                        IntegerUtils.getInteger(12)),
    XhtmlLafConstants.AF_SELECT_BOOLEAN_CHECKBOX_DISABLED_UNCHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkdn.gif",
                        null,
                        IntegerUtils.getInteger(12),
                        IntegerUtils.getInteger(12)),

    XhtmlLafConstants.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_CHECKED_ICON_NAME,
    new ContextImageIcon( "adf/images/checkrc.gif",
                        null,
                        IntegerUtils.getInteger(12),
                        IntegerUtils.getInteger(12)),

    XhtmlLafConstants.AF_SELECT_BOOLEAN_CHECKBOX_READONLY_UNCHECKED_ICON_NAME,
    new ContextImageIcon("adf/images/checkrn.gif",
                        null,
                        IntegerUtils.getInteger(12),
                        IntegerUtils.getInteger(12)),

     // radio icons
    XhtmlLafConstants.AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiods.gif",
                        null,
                        IntegerUtils.getInteger(11),
                        IntegerUtils.getInteger(11)),
    XhtmlLafConstants.AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiodn.gif",
                        null,
                        IntegerUtils.getInteger(11),
                        IntegerUtils.getInteger(11)),

    XhtmlLafConstants.AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME,
    new ContextImageIcon( "adf/images/radiors.gif",
                        null,
                        IntegerUtils.getInteger(10),
                        IntegerUtils.getInteger(10)),

    XhtmlLafConstants.AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME,
    new ContextImageIcon("adf/images/radiorn.gif",
                        null,
                        IntegerUtils.getInteger(10),
                        IntegerUtils.getInteger(10)),

    // progressIndicator icons
    AF_PROGRESS_INDICATOR_INDETERMINATE_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the selectRangeChoiceBar
    AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    // arrows for the navigationBar
    AF_TABLE_NB_PREV_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_TABLE_NB_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_TABLE_NB_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),

    AF_TABLE_NB_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),
    
    // arrows for the navigationBar
    AF_TREE_TABLE_NB_PREV_ICON_NAME,
    NullIcon.sharedInstance(),
    
    AF_TREE_TABLE_NB_NEXT_ICON_NAME,
    NullIcon.sharedInstance(),
    
    AF_TREE_TABLE_NB_PREV_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),
    
    AF_TREE_TABLE_NB_NEXT_DISABLED_ICON_NAME,
    NullIcon.sharedInstance(),    
    
    // arrows for the chooseDate
    // @todo think about how chooseDate and selectRangeChoiceBar have different
    // icons for the XhtmlSkin, but the same for oracleDesktopSkin.
    // This is weird, although I can see how they are needed to move months
    // for chooseDate, but you have the Next 5/ Prev 5 links for selectRangeCB.
    // OracleDesktopSkin
    // will need to create a global icon that it shares between its SelectRangeChoiceBar
    // and ChooseDate components.

    // buttons are the true hierarchical example. Work on those next.
    AF_CHOOSE_DATE_PREV_ICON_NAME,
    new TextIcon("<"),

    AF_CHOOSE_DATE_NEXT_ICON_NAME,
    new TextIcon(">"),

    AF_CHOOSE_DATE_PREV_DISABLED_ICON_NAME,
    new TextIcon("<"),

    AF_CHOOSE_DATE_NEXT_DISABLED_ICON_NAME,
    new TextIcon(">"),

    AF_SELECT_INPUT_DATE_PREV_ICON_NAME,
    new TextIcon("<"),

    AF_SELECT_INPUT_DATE_NEXT_ICON_NAME,
    new TextIcon(">"),

    AF_SELECT_INPUT_DATE_PREV_DISABLED_ICON_NAME,
    new TextIcon("<"),

    AF_SELECT_INPUT_DATE_NEXT_DISABLED_ICON_NAME,
    new TextIcon(">"),

  };
}
