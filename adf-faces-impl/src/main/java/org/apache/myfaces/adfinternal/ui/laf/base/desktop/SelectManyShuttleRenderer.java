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

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.el.ValueBinding;

import javax.faces.model.SelectItem;

import org.apache.myfaces.adf.component.UIXSelectMany;
import org.apache.myfaces.adf.component.UIXSelectOrder;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.FormRenderer;
import org.apache.myfaces.adfinternal.share.xml.NamespaceURI;
import org.apache.myfaces.adfinternal.ui.html.HTMLWebBean;
import org.apache.myfaces.adfinternal.skin.Skin;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.adfinternal.util.IntegerUtils;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.ui.MutableUINode;
import org.apache.myfaces.adfinternal.ui.NodeUtils;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.beans.MarlinBean;
import org.apache.myfaces.adfinternal.ui.collection.UINodeList;
import org.apache.myfaces.adfinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.adfinternal.ui.composite.RootAttributeBoundValue;
import org.apache.myfaces.adfinternal.ui.composite.UINodeRenderer;
import org.apache.myfaces.adfinternal.ui.data.BoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.ConcatBoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.FixedBoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.IfBoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.IsAgentApplicationBoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.OrBoundValue;
import org.apache.myfaces.adfinternal.ui.data.bind.NotBoundValue;
import org.apache.myfaces.adfinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.adfinternal.ui.laf.base.SkinTranslatedBoundValue;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

/**
 * Renders a shuttle element.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/SelectManyShuttleRenderer.java#0 $) $Date: 10-nov-2005.18:56:13 $
 * @author The Oracle ADF Faces Team
 */
public class SelectManyShuttleRenderer extends UINodeRenderer
               implements UIConstants, BaseDesktopConstants
{

  protected UINode getRenderingUINode(
    RenderingContext context,
    UINode           node
    )
  {
    if ( _INSTANCE == null )
      _INSTANCE = createCompositeUINode();

    return _INSTANCE;
  }

  /**
  * Render a UINode in a RenderingContext.
  */
  public void render(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    // set up info about this shuttle and store it on context
    _setShuttleInfoOnContext( context, node );
    XhtmlLafUtils.addOnSubmitRequiredValidator(
                                       context,
                                       node, 
                                       UIXSelectMany.REQUIRED_MESSAGE_ID, 
                                       getNodeName(context, node));
     

      super.render( context, node );

    // remove info about this shuttle from context
    _clearContext( context );
  }

  protected void prerender(
    RenderingContext context,
    UINode           node) throws IOException
  {
      // We are sharing this renderer for selectOrderShuttle and
      // selectManySHuttle. Set the resource key map if the component
      // is selectOrder
      _setSelectOrderResourceKeyMap(context, node);

      //have to put the js in!
      BaseDesktopUtils.addLib(context, "ShuttleProxy()");

      // Write out translated version of strings that
      // may be displayed in a javascript alert
      // if this hasn't been done yet
      if ( context.getProperty(MARLIN_NAMESPACE,
                               _TRANSLATED_VARS_EXIST_PROPERTY_KEY) == null)
      {
        context.setProperty(MARLIN_NAMESPACE,
                            _TRANSLATED_VARS_EXIST_PROPERTY_KEY,
                            Boolean.TRUE);

        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("script", null);
        XhtmlLafRenderer.renderScriptDeferAttribute(context);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        XhtmlLafRenderer.renderScriptTypeAttribute(context);

        String noItems =
          BaseLafRenderer.getTranslatedString(context,
                                              _SHUTTLE_NO_ITEMS_FEEDBACK_KEY);
        String noItemsSelected =
          BaseLafRenderer.getTranslatedString(context,
                                        _SHUTTLE_NO_ITEM_SELECTED_FEEDBACK_KEY);

        int noItemsLength = _TRANSLATED_JS_FEEDBACK_NO_ITEMS_LENGTH;

        if (noItems != null )
          noItemsLength = noItemsLength + noItems.length();

        int noItemsSelectedLength = _TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED_LENGTH;

        if (noItemsSelected != null )
          noItemsSelectedLength = noItemsSelectedLength + noItemsSelected.length();

        int bufferLength = noItemsLength + noItemsSelectedLength;


        StringBuffer buffer = new StringBuffer( bufferLength );

        buffer.append(_TRANSLATED_JS_FEEDBACK_NO_ITEMS);

        if ( noItems != null )
        {
          buffer.append(BaseDesktopUtils.escapeJS(noItems, true));
        }

        buffer.append("';");

        buffer.append(_TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED);
        if (noItemsSelected != null )
        {
          buffer.append(BaseDesktopUtils.escapeJS(noItemsSelected, true));
        }
        buffer.append("';");

        writer.writeText(buffer.toString(), null);
        _writeNavExclude(context, node);
        writer.endElement("script");
      }
      else
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("script", null);
        XhtmlLafRenderer.renderScriptDeferAttribute(context);
          // Bug #3426092:
          // render the type="text/javascript" attribute in accessibility mode
          XhtmlLafRenderer.renderScriptTypeAttribute(context);
        _writeNavExclude(context, node);
        writer.endElement("script");
      }
  }
  

  /**
   * Get the name for a node. In rare cases, the renderer must specify
   * the name.
   */
  protected Object getNodeName(
          RenderingContext context,
          UINode           node
          )
  {
    return node.getAttributeValue(context, ID_ATTR);
  }


  private static String _getShuttleName(
    RenderingContext context,
    UINode           node)
  {
    // get name of shuttle
    String shuttleName = BaseDesktopUtils.getStringAttributeValue(context,
                                                              node,
                                                              ID_ATTR);
    if ( shuttleName == null )
      shuttleName = _DEFAULT_SHUTTLE_NAME;

    return shuttleName;
  }

  private void _writeNavExclude(
    RenderingContext context,
    UINode           node) throws IOException
  {
    String name = _getShuttleName(context, node);
    ResponseWriter writer = context.getResponseWriter();
    writer.writeText("_addNavExclude('", null);
    writer.writeText(name, null);
    writer.writeText(_LEADING_COMPLETE, null);
    writer.writeText("');_addNavExclude('", null);
    writer.writeText(name, null);
    writer.writeText(_TRAILING_COMPLETE, null);
    writer.writeText("')", null);
  }

  protected UINode getMoveButtons(
    BoundValue hasTwoListsBV,
    BoundValue moveDestBV,
    BoundValue moveAllDestBV,
    BoundValue removeDestBV,
    BoundValue removeAllDestBV)
  {

    MarlinBean cell = new MarlinBean(CELL_FORMAT_NAME);
    cell.setAttributeValue(H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    cell.setAttributeValue(V_ALIGN_ATTR, MIDDLE_ATTRIBUTE_VALUE);

    cell.setAttributeValue(WRAPPING_DISABLED_ATTR, Boolean.TRUE);
    cell.setInlineStyle("padding:5px");

    // move
    UINode move = _getButton( AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
                              _SELECT_MANY_MOVE_TIP_KEY,
                              moveDestBV);

    BoundValue moveTextBV = new SkinTranslatedBoundValue(_SELECT_MANY_MOVE_KEY);
    BoundValue moveTipBV = new SkinTranslatedBoundValue(_SELECT_MANY_MOVE_TIP_KEY);
    MarlinBean moveLink = new MarlinBean(LINK_NAME);
    moveLink.setAttributeValue( TEXT_ATTR, moveTextBV );
    moveLink.setAttributeValue( DESTINATION_ATTR, moveDestBV );
    moveLink.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR) );
    moveLink.setAttributeValue( READ_ONLY_ATTR, new IsReadOnly());
    moveLink.setAttributeValue(SHORT_DESC_ATTR, moveTipBV);

    // move all
    UINode moveAll = _getButton( AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
                                 _SELECT_MANY_MOVE_ALL_TIP_KEY,
                                 moveAllDestBV);

    BoundValue moveAllTextBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_MOVE_ALL_KEY);
    BoundValue moveAllTipBV  =
      new SkinTranslatedBoundValue(_SELECT_MANY_MOVE_ALL_TIP_KEY);
    MarlinBean moveAllLink = new MarlinBean(LINK_NAME);
    moveAllLink.setAttributeValue( TEXT_ATTR, moveAllTextBV );
    moveAllLink.setAttributeValue( DESTINATION_ATTR, moveAllDestBV );
    moveAllLink.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR) );
    moveAllLink.setAttributeValue( READ_ONLY_ATTR, new IsReadOnly());
    moveAllLink.setAttributeValue( SHORT_DESC_ATTR, moveAllTipBV );

    // remove
    UINode remove = _getButton( AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
                                _SELECT_MANY_REMOVE_TIP_KEY,
                                removeDestBV);

    BoundValue removeTextBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_REMOVE_KEY);
    BoundValue removeTipBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_REMOVE_TIP_KEY);

    MarlinBean removeLink = new MarlinBean(LINK_NAME);
    removeLink.setAttributeValue( TEXT_ATTR, removeTextBV );
    removeLink.setAttributeValue( DESTINATION_ATTR, removeDestBV );
    removeLink.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR) );
    removeLink.setAttributeValue( READ_ONLY_ATTR,
                                  new IsReadOnly());
    removeLink.setAttributeValue( SHORT_DESC_ATTR, removeTipBV );

    // remove all
    UINode removeAll = _getButton(
                              AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
                              _SELECT_MANY_REMOVE_ALL_TIP_KEY,
                              removeAllDestBV);


    BoundValue removeAllTextBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_REMOVE_ALL_KEY);
    BoundValue removeAllTipBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_REMOVE_ALL_TIP_KEY);

    MarlinBean removeAllLink = new MarlinBean(LINK_NAME);
    removeAllLink.setAttributeValue( TEXT_ATTR, removeAllTextBV );
    removeAllLink.setAttributeValue( DESTINATION_ATTR, removeAllDestBV );
    removeAllLink.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR) );
    removeAllLink.setAttributeValue( READ_ONLY_ATTR,
                                     new IsReadOnly());
    removeAllLink.setAttributeValue( SHORT_DESC_ATTR, removeAllTipBV );

    HTMLWebBean spacer = new HTMLWebBean("div");
    spacer.setHTMLAttributeValue("style","margin-top:5px");
    // Can't use stack or space between image and text too great,
    // so using breaks.

    cell.addIndexedChild( move);
    cell.addIndexedChild( moveLink );

    cell.addIndexedChild( spacer );


    cell.addIndexedChild( moveAll);
    cell.addIndexedChild( moveAllLink );

    cell.addIndexedChild( spacer );


    cell.addIndexedChild( remove );
    cell.addIndexedChild( removeLink );

    cell.addIndexedChild( spacer );


    cell.addIndexedChild( removeAll );
    cell.addIndexedChild( removeAllLink );

    cell.setAttributeValue( RENDERED_ATTR, hasTwoListsBV );

    return cell;
  }

  protected UINode getReorderButtons(
    BoundValue hasReorderButtonsBV,
    BoundValue upTopDestBV,
    BoundValue upOneDestBV,
    BoundValue downOneDestBV,
    BoundValue downBottomDestBV
    )
  {
    MarlinBean cell = new MarlinBean(CELL_FORMAT_NAME);
    cell.setAttributeValue(H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    cell.setAttributeValue(V_ALIGN_ATTR, MIDDLE_ATTRIBUTE_VALUE);

    // these buttons are only used for selectOrderShuttle
    // up top
    UINode upTop = _getButton( AF_SELECT_ORDER_SHUTTLE_REORDER_TOP_ICON_NAME,
                               "af_selectOrderShuttle.REORDER_UP_ALL_TIP",
                               upTopDestBV);

    // up one
    UINode upOne = _getButton( AF_SELECT_ORDER_SHUTTLE_REORDER_UP_ICON_NAME,
                               "af_selectOrderShuttle.REORDER_UP_TIP",
                               upOneDestBV);

    // down one
    UINode downOne = _getButton( AF_SELECT_ORDER_SHUTTLE_REORDER_DOWN_ICON_NAME,
                                 "af_selectOrderShuttle.REORDER_DOWN_TIP",
                                 downOneDestBV);

    // down bottom
    UINode downBottom = _getButton(AF_SELECT_ORDER_SHUTTLE_REORDER_BOTTOM_ICON_NAME,
                                   "af_selectOrderShuttle.REORDER_DOWN_ALL_TIP",
                                   downBottomDestBV);

    MarlinBean spacer = new MarlinBean(SPACER_NAME);
    spacer.setAttributeValue(WIDTH_ATTR, "30");
    spacer.setAttributeValue(HEIGHT_ATTR, "15");

    MarlinBean spacerSmall = new MarlinBean(SPACER_NAME);
    spacerSmall.setAttributeValue(WIDTH_ATTR, "1");
    spacerSmall.setAttributeValue(HEIGHT_ATTR, "5");

    // Can't use stack or space between image and text too great,
    // so using breaks.
    cell.addIndexedChild( upTop );
    cell.addIndexedChild( spacerSmall );
    cell.addIndexedChild( _BR );
    cell.addIndexedChild( upOne );
    cell.addIndexedChild( spacer );
    cell.addIndexedChild( _BR );
    cell.addIndexedChild( downOne );
    cell.addIndexedChild( spacerSmall );
    cell.addIndexedChild( _BR );
    cell.addIndexedChild( downBottom );

    cell.setAttributeValue( RENDERED_ATTR, hasReorderButtonsBV );

    return cell;
  }


  private static UINode _getButton(
    String     iconName,
    String     altTextKey,
    BoundValue destinationBV
    )
  {
    // Create an IconBean for displaying the icon
    MarlinBean icon = new MarlinBean(ICON_NAME);
    icon.setAttributeValue(NAME_ATTR,
                           new NamespaceURI(MARLIN_NAMESPACE, iconName));
    icon.setAttributeValue(SHORT_DESC_ATTR,
                           new SkinTranslatedBoundValue(altTextKey));
    icon.setAttributeValue(STYLE_CLASS_ATTR,"p_OraDisplayBlock");
    // Set the embedded attribute - this allows text-based icons to
    // render the short desc on the outer link
    icon.setAttributeValue(EMBEDDED_ATTR, Boolean.TRUE);

    // Wrap the IconBean in a link
    MarlinBean link = new MarlinBean(LINK_NAME);
    link.setAttributeValue( DESTINATION_ATTR, destinationBV);
    link.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR) );
    link.setAttributeValue( READ_ONLY_ATTR, new IsReadOnly());
    link.addIndexedChild(icon);

    // Wrap the button and the subsequent br in a flowlayout so that
    // we the button and br can be shown or hidden together
    MarlinBean wrapper = new MarlinBean(FLOW_LAYOUT_NAME);
    wrapper.addIndexedChild(link);

    // Only display button if we have an icon
    wrapper.setAttributeValue(RENDERED_ATTR,
                              new CheckIconBoundValue(iconName));



    return wrapper;
  }


  /*
   * Renders the reorder icons on the side of the list
   */
  private UINode _getReorderButtons(
    BoundValue hasReorderButtonsBV,
    BoundValue listNameBV
    )
  {
    // up top
    BoundValue upTopDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _ORDER_ITEMS_TOP_BV,
                                                            listNameBV,
                                                            _END_FUNC_BV});


    // up one
    BoundValue upOneDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _ORDER_ITEMS_UP_BV,
                                                            listNameBV,
                                                            _END_FUNC_BV});


    // down one
    BoundValue downOneDestBV = new ConcatBoundValue(
                                         new BoundValue[]{ _ORDER_ITEMS_DOWN_BV,
                                                           listNameBV,
                                                           _END_FUNC_BV});

    // down bottom
    BoundValue downBottomDestBV = new ConcatBoundValue(
                                       new BoundValue[]{ _ORDER_ITEMS_BOTTOM_BV,
                                                         listNameBV,
                                                         _END_FUNC_BV});

    return getReorderButtons( hasReorderButtonsBV,
                              upTopDestBV, upOneDestBV,
                              downOneDestBV, downBottomDestBV);


  }


  private UINode _getMoveButtons(
    BoundValue hasTwoListsBV,
    BoundValue leadingListNameBV,
    BoundValue trailingListNameBV
    )
  {


    // move
    BoundValue moveDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _MOVE_ITEMS_BV,
                                                            leadingListNameBV,
                                                            _COMMA_BV,
                                                            trailingListNameBV,
                                                            _END_FUNC_BV});



    // move All
    BoundValue moveAllDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _MOVE_ALL_ITEMS_BV,
                                                            leadingListNameBV,
                                                            _COMMA_BV,
                                                            trailingListNameBV,
                                                            _END_FUNC_BV});

    // remove
    BoundValue removeDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _MOVE_ITEMS_BV,
                                                            trailingListNameBV,
                                                            _COMMA_BV,
                                                            leadingListNameBV,
                                                            _END_FUNC_BV});

    // remove All
    BoundValue removeAllDestBV = new ConcatBoundValue(
                                          new BoundValue[]{ _MOVE_ALL_ITEMS_BV,
                                                            trailingListNameBV,
                                                            _COMMA_BV,
                                                            leadingListNameBV,
                                                            _END_FUNC_BV});
    return getMoveButtons(hasTwoListsBV,
                          moveDestBV,
                          moveAllDestBV,
                          removeDestBV,
                          removeAllDestBV);
  }

  protected UINode createCompositeUINode()
  {

    // Since this tree is static everything that needs the
    // context must be databound

    MarlinBean root = new MarlinBean(TABLE_LAYOUT_NAME);
    root.setAttributeValue(WIDTH_ATTR, "10%");
    root.setAttributeValue(
        ID_ATTR,
        RootAttributeBoundValue.getBoundValue(ID_ATTR));
    root.setAttributeValue(
        SHORT_DESC_ATTR,
        RootAttributeBoundValue.getBoundValue(SHORT_DESC_ATTR));

    BoundValue hasTwoLists =
      new NotBoundValue(
        RootAttributeBoundValue.getBoundValue(REORDER_ONLY_ATTR));

    //render the headers
    UINode headerRow = _getHeaderRow( hasTwoLists);
    root.addIndexedChild( headerRow );

    //render the containers
    UINode containerRow = _getContainerRow( hasTwoLists);
    root.addIndexedChild( containerRow );
    return root;

  }

   /*
   * Renders the headers of the 2 lists
   */
  private static UINode _getHeaderRow( BoundValue hasTwoLists)
  {

    MarlinBean row = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean format1 = new MarlinBean(CELL_FORMAT_NAME);
    MarlinBean format2 = new MarlinBean(CELL_FORMAT_NAME);
    MarlinBean format3 = new MarlinBean(CELL_FORMAT_NAME);

    MarlinBean space = new MarlinBean(SPACER_NAME);
    space.setAttributeValue(WIDTH_ATTR, "1");
    space.setAttributeValue(HEIGHT_ATTR, "1");
    space.setAttributeValue(RENDERED_ATTR,
                 new IsAgentApplicationBoundValue(AdfFacesAgent.APPLICATION_NETSCAPE));

    MarlinBean spaceRequired = new MarlinBean(SPACER_NAME);
    spaceRequired.setAttributeValue(WIDTH_ATTR, "1");
    spaceRequired.setAttributeValue(HEIGHT_ATTR, "1");
    
    spaceRequired.setAttributeValue( RENDERED_ATTR, new RequiredIconBoundValue());
    MarlinBean leadingHeader = new MarlinBean(STYLED_TEXT_NAME);
    MarlinBean trailingHeader = new MarlinBean(STYLED_TEXT_NAME);

    leadingHeader.setAttributeValue(TEXT_ATTR,
            new SelectManyShuttleRenderer.ShuttleInfoBV( _LEADING_INFO_KEY,
                                             ContainerInfo.HEADER_TEXT_KEY ));

    trailingHeader.setAttributeValue(TEXT_ATTR,
            new SelectManyShuttleRenderer.ShuttleInfoBV( _TRAILING_INFO_KEY,
                                            ContainerInfo.HEADER_TEXT_KEY ));

    MarlinBean requiredIcon = new MarlinBean(ICON_NAME);
    requiredIcon.setAttributeValue(NAME_ATTR,
           new NamespaceURI(MARLIN_NAMESPACE, REQUIRED_ICON_ALIAS_NAME));
    requiredIcon.setAttributeValue( SHORT_DESC_ATTR, new RequiredTipBoundValue());
    requiredIcon.setAttributeValue( RENDERED_ATTR, new RequiredIconBoundValue());
    
    format1.setAttributeValue(STYLE_CLASS_ATTR, SHUTTLE_HEADER_STYLE_CLASS);
    format1.setAttributeValue(V_ALIGN_ATTR, BOTTOM_ATTRIBUTE_VALUE);
    format1.addIndexedChild(space);
    format1.setAttributeValue( RENDERED_ATTR, hasTwoLists );
    format1.addIndexedChild(leadingHeader);

    format2.addIndexedChild(space);
    format2.setAttributeValue( RENDERED_ATTR, hasTwoLists );


    format3.setAttributeValue(STYLE_CLASS_ATTR, SHUTTLE_HEADER_STYLE_CLASS);
    format3.setAttributeValue(V_ALIGN_ATTR, BOTTOM_ATTRIBUTE_VALUE);
    format3.addIndexedChild(requiredIcon);
    format3.addIndexedChild(spaceRequired);
    format3.addIndexedChild(space);
    format3.addIndexedChild(trailingHeader);

    row.addIndexedChild(format1);
    row.addIndexedChild(format2);
    row.addIndexedChild(format3);

    return row;
  }


  /*
   * Renders the two content containers
   */
  private UINode _getContainerRow(
    BoundValue hasTwoListsBV
    )
  {
    MarlinBean row = new MarlinBean(ROW_LAYOUT_NAME);

    UINode filter = ContextPoppingUINode.getUINode(FILTER_CHILD);


    // list
    BoundValue leadingListNameBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _LEADING_INFO_KEY,
                                               ContainerInfo.LIST_NAME_KEY);
    BoundValue trailingListNameBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _TRAILING_INFO_KEY,
                                             ContainerInfo.LIST_NAME_KEY);

    BoundValue leadingListItemsBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _LEADING_INFO_KEY,
                                       ContainerInfo.ITEMS_LIST_KEY);
    BoundValue trailingListItemsBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _TRAILING_INFO_KEY,
                                       ContainerInfo.ITEMS_LIST_KEY);

    BoundValue listLenBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _SHUTTLE_INFO_KEY,
                                              ShuttleInfo.LIST_LEN_KEY);

    BoundValue listWidthBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _SHUTTLE_INFO_KEY,
                                                ShuttleInfo.DESC_WIDTH_KEY);

    BoundValue formNameBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _SHUTTLE_INFO_KEY,
                                              ShuttleInfo.FORM_NAME_KEY);

    MarlinBean option = new MarlinBean(OPTION_NAME);
    option.setAttributeValue( TEXT_ATTR,
       new SelectManyShuttleRenderer.ShuttleInfoBV( _SHUTTLE_INFO_KEY,
                                             ShuttleInfo.BAR_TEXT_KEY));

    // descriptions
    BoundValue hasLeadingDescAreaBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _LEADING_INFO_KEY,
                                            ContainerInfo.HAS_DESC_AREA_KEY);

    BoundValue hasTrailingDescAreaBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( _TRAILING_INFO_KEY,
                                            ContainerInfo.HAS_DESC_AREA_KEY);

    BoundValue hasDescAreaBV = new OrBoundValue( hasLeadingDescAreaBV,
                                                 hasTrailingDescAreaBV);

    // footers
    BoundValue hasLeadingFooterBV =
                BaseDesktopUtils.createIsRenderedBoundValue(LEADING_FOOTER_CHILD);
    BoundValue hasTrailingFooterBV =
                BaseDesktopUtils.createIsRenderedBoundValue(TRAILING_FOOTER_CHILD);
    BoundValue hasFootersBV = new OrBoundValue( hasLeadingFooterBV,
                                                hasTrailingFooterBV);


    //---------------------------------------------------
    // 1st Content Container
    //---------------------------------------------------
    UINode leading = _getContainer( _LEADING_INFO_KEY,
                                    leadingListNameBV,
                                    listLenBV,
                                    listWidthBV,
                                    hasTwoListsBV,
                                    RootAttributeBoundValue.getBoundValue(LEADING_HEADER_ATTR),
                                    hasFootersBV,
                                    hasDescAreaBV,
                                    hasLeadingDescAreaBV,
                                    leadingListItemsBV,
                                    trailingListNameBV,
                                    formNameBV,
                                    LEADING_FOOTER_CHILD,
                                    filter,
                                    option);

    row.addIndexedChild(leading);

    //---------------------------------------------------
    // move buttons
    //---------------------------------------------------
    UINode moveButtons =  _getMoveButtons(hasTwoListsBV,
                                          leadingListNameBV,
                                          trailingListNameBV);

    row.addIndexedChild( moveButtons );


    //---------------------------------------------------
    // 2nd Content Container
    //---------------------------------------------------

    UINode trailing = _getContainer( _TRAILING_INFO_KEY,
                                     trailingListNameBV,
                                     listLenBV,
                                     listWidthBV,
                                     FixedBoundValue.TRUE_VALUE,
                                     RootAttributeBoundValue.getBoundValue(TRAILING_HEADER_ATTR),
                                     hasFootersBV,
                                     hasDescAreaBV,
                                     hasTrailingDescAreaBV,
                                     trailingListItemsBV,
                                     leadingListNameBV,
                                     formNameBV,
                                     TRAILING_FOOTER_CHILD,
                                     null,
                                     option);




    MarlinBean resetScript = new MarlinBean(SCRIPT_NAME);

    BoundValue resetScriptBV =
       new SelectManyShuttleRenderer.ShuttleInfoBV(_SHUTTLE_INFO_KEY,
                                              ShuttleInfo.RESET_SCRIPT_KEY);
    resetScript.setAttributeValue( TEXT_ATTR, resetScriptBV);

    MarlinBean flow = new MarlinBean(FLOW_LAYOUT_NAME);
    flow.addIndexedChild(trailing);
    flow.addIndexedChild(resetScript);
    row.addIndexedChild(flow);

    return row;
  }



  /*
   * Renders a container
   */
  private UINode _getContainer(
    Object     propertyKey,
    BoundValue listNameBV,
    BoundValue listLenBV,
    BoundValue listWidthBV,
    BoundValue renderListBV,
    BoundValue listShortDescBV,
    BoundValue hasFootersBV,
    BoundValue renderDescAreaBV,
    BoundValue hasDescAreaBV,
    BoundValue itemsBV,
    BoundValue otherListNameBV,
    BoundValue formNameBV,
    String     footerChildName,
    UINode     filter,
    UINode     option
    )
  {

    // the container
    MarlinBean  container = new MarlinBean(CONTENT_CONTAINER_NAME);

    // only render if there is a list
    container.setAttributeValue(RENDERED_ATTR, renderListBV );
    
    container.setAttributeValue(WIDTH_ATTR, "100%");

    // content of container is a table
    MarlinBean table = new MarlinBean(TABLE_LAYOUT_NAME);

    container.addIndexedChild( table );

    //
    // add filter
    //
    BoundValue renderFilterBV =
                         BaseDesktopUtils.createIsRenderedBoundValue(FILTER_CHILD);

    MutableUINode filterRow = _getRow( filter,
                                       renderFilterBV,
                                       _DEFAULT_FILTER_HEIGHT,
                                       3);

    table.addIndexedChild( filterRow );

    //
    // add list
    //
    MarlinBean modList = new MarlinBean(LIST_NAME);

    BoundValue onChangeFuncBV = new ConcatBoundValue( new BoundValue[]{
                                                        _DISPLAY_DESC_BV,
                                                        listNameBV,
                                                        _COMMA_BV,
                                                        formNameBV,
                                                        _END_FUNC_BV} );

    BoundValue onChangeBV = new IfBoundValue( hasDescAreaBV,
                                              onChangeFuncBV,
                                              null );

    BoundValue onDblClickBV = new ConcatBoundValue( new BoundValue[]{
                                                   _MOVE_ITEMS_NO_JS_PREFIX_BV,
                                                   listNameBV,
                                                   _COMMA_BV,
                                                   otherListNameBV,
                                                   _COMMA_BV,
                                                   formNameBV,
                                                   _END_FUNC_BV});


    modList.setAttributeValue(MULTIPLE_ATTR, Boolean.TRUE);
    modList.setAttributeValue(NAME_ATTR, listNameBV);
    modList.setAttributeValue(SIZE_ATTR, listLenBV);
    modList.setAttributeValue(ON_CHANGE_ATTR, onChangeBV);
    modList.setAttributeValue(ON_DOUBLE_CLICK_ATTR, onDblClickBV);
    // rdw - The workaround for bug 4184422 is to make the listboxes disabled instead of readonly
    modList.setAttributeValue(DISABLED_ATTR,
                              new OrBoundValue(RootAttributeBoundValue.getBoundValue(DISABLED_ATTR),
                                               new IsReadOnly()));
    modList.setAttributeValue(SHORT_DESC_ATTR,
                              listShortDescBV);

    UINodeList nodeList =
        new SelectManyShuttleRenderer.OptionsUINodeList(itemsBV, option);
    modList.setIndexedNodeList(nodeList);

    MutableUINode listRow = _getRow( modList, renderListBV, 0, 1);
    table.addIndexedChild( listRow );

    BoundValue reorderableBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( propertyKey,
                                              ContainerInfo.IS_REORDERABLE_KEY);

    UINode reorderButtons = _getReorderButtons( reorderableBV, listNameBV );
    listRow.addIndexedChild( reorderButtons );

    //
    // add hidden info
    //
    MarlinBean vals = new MarlinBean(FORM_VALUE_NAME);


    BoundValue valsNameBV =
                    new ConcatBoundValue( new BoundValue[]{listNameBV,
                                                           _ITEMS_COMPLETE_BV});

    BoundValue valsValueBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( propertyKey,
                                                   ContainerInfo.VALS_KEY);
    vals.setAttributeValue( NAME_ATTR, valsNameBV );
    vals.setAttributeValue( VALUE_ATTR, valsValueBV );

    MarlinBean descScript = new MarlinBean(SCRIPT_NAME);
    descScript.setAttributeValue(RENDERED_ATTR, renderDescAreaBV);

    BoundValue descValueBV =
      new SelectManyShuttleRenderer.ShuttleInfoBV( propertyKey,
                                          ContainerInfo.DESCRIPTIONS_KEY);
    descScript.setAttributeValue( TEXT_ATTR, descValueBV);

    MarlinBean flow = new MarlinBean(FLOW_LAYOUT_NAME);
    flow.addIndexedChild( descScript );
    flow.addIndexedChild( vals );
    listRow.addIndexedChild( flow );


    //
    // add descriptions
    //
    UINode spacerRow1 = _getRow( null, renderDescAreaBV, 8, 1 );
    table.addIndexedChild( spacerRow1 );

    MarlinBean stack = new MarlinBean(STACK_LAYOUT_NAME);

    MarlinBean label = new MarlinBean(STYLED_TEXT_NAME);
    label.setStyleClass(INSTRUCTION_TEXT_STYLE_CLASS);

    BoundValue labelBV =
      new SkinTranslatedBoundValue(_SELECT_MANY_DESCRIPTION_LABEL_KEY);
    label.setAttributeValue(TEXT_ATTR, labelBV);
    stack.addIndexedChild(label);

    MarlinBean descArea = new MarlinBean(TEXT_INPUT_NAME);
    descArea.setAttributeValue(ROWS_ATTR, new Integer(2));
    descArea.setAttributeValue(WRAP_ATTR, SOFT_ATTRIBUTE_VALUE);
    descArea.setAttributeValue( NAME_ATTR,
                                new ConcatBoundValue( new BoundValue[]{
                                                         listNameBV,
                                                         _DESC_COMPLETE_BV }));

    descArea.setAttributeValue( COLUMNS_ATTR, listWidthBV );
    descArea.setStyleClass(AF_FIELD_TEXT_STYLE_CLASS);
    descArea.setAttributeValue(SHORT_DESC_ATTR, labelBV);
    descArea.setAttributeValue( DISABLED_ATTR,
                      RootAttributeBoundValue.getBoundValue(DISABLED_ATTR));
    descArea.setAttributeValue( READ_ONLY_ATTR,
                                new IsReadOnly());

    stack.addIndexedChild(descArea);
    stack.setAttributeValue(RENDERED_ATTR, hasDescAreaBV);

    MutableUINode descRow = _getRow( stack,
                                     renderDescAreaBV,
                                     _DEFAULT_DESC_AREA_HEIGHT,
                                     3);

    table.addIndexedChild( descRow );

    //
    // add footer
    //

    UINode footer = ContextPoppingUINode.getUINode( footerChildName );

    UINode spacerRow = _getRow( null, hasFootersBV, 8, 1 );
    table.addIndexedChild( spacerRow );

    MutableUINode footerRow = _getRow( footer,
                                       hasFootersBV,
                                       _DEFAULT_FOOTER_HEIGHT,
                                       3);

    table.addIndexedChild( footerRow );

    return container;

  }

  /*
   * Create a row in the table
   */
  private static MutableUINode _getRow(
    UINode      node,
    BoundValue  renderRow,
    int         height,
    int         colSpan
    )
  {
    MarlinBean row = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean format = new MarlinBean(CELL_FORMAT_NAME);

    format.setAttributeValue(WRAPPING_DISABLED_ATTR, Boolean.TRUE);
    format.setAttributeValue(V_ALIGN_ATTR, MIDDLE_ATTRIBUTE_VALUE);
    format.setAttributeValue(COLUMN_SPAN_ATTR, colSpan );
    
    row.addIndexedChild( format );
    
    if ( node != null )
    {
      format.addIndexedChild(node);
    }

    if ( height > 0 )
    {
      MarlinBean spacer = new MarlinBean(SPACER_NAME);
      spacer.setAttributeValue(WIDTH_ATTR, "1");
      spacer.setAttributeValue(HEIGHT_ATTR, height);
      row.addIndexedChild( spacer );
    }
    
    row.setAttributeValue( RENDERED_ATTR, renderRow );
    
    return row;
  }




  private static final void _setShuttleInfoOnContext(
    RenderingContext context,
    UINode node )
  {

    if (context != null)
    {

      SelectManyShuttleRenderer.ShuttleInfo shuttleInfo =
         _getShuttleInfo(context, node);

      context.setProperty( MARLIN_NAMESPACE,
                           _SHUTTLE_INFO_KEY,
                           shuttleInfo.getData());
      context.setProperty( MARLIN_NAMESPACE,
                           _LEADING_INFO_KEY,
                           shuttleInfo.leadingInfo.getData());
      context.setProperty( MARLIN_NAMESPACE,
                           _TRAILING_INFO_KEY,
                           shuttleInfo.trailingInfo.getData());
    }
  }


    private static Integer _getBestListLen(
      RenderingContext context,
      UINode           node,
      int              leadingListCount,
      int              trailingListCount)
    {
      Integer rows = (Integer)node.getAttributeValue(context, SIZE_ATTR );

      if ( rows != null )
      {
        rows = IntegerUtils.getInteger( Math.min(_MAXIMUM_LIST_LEN,
                               Math.max(_MINIMUM_LIST_LEN, rows.intValue())));
      }
      else
      {
        // defaults to min
        rows = IntegerUtils.getInteger( _MINIMUM_LIST_LEN);

        boolean higher1 = (leadingListCount > _MAXIMUM_LIST_LEN);
        boolean higher2 = (trailingListCount > _MAXIMUM_LIST_LEN);
        boolean between1 = (leadingListCount > _MINIMUM_LIST_LEN &&
                            leadingListCount < _MAXIMUM_LIST_LEN);
        boolean between2 = (trailingListCount > _MINIMUM_LIST_LEN &&
                            trailingListCount < _MAXIMUM_LIST_LEN);

        //if either higher than max, take max
        if( higher1 || higher2 )
        {
          rows = IntegerUtils.getInteger( _MAXIMUM_LIST_LEN );
        }
        //if they are both between, take the lower so bars don't show
        else if ( between1 && between2 )
          rows = IntegerUtils.getInteger( Math.min( leadingListCount,
                                                     trailingListCount ));
        //if one is between and the other isn't, take the one between
        else if (  between1 || between2 )
          rows = IntegerUtils.getInteger( Math.max( leadingListCount,
                                                     trailingListCount ));
      }

      return rows;
    }



  private static void _setResetInfo(
    String           shuttleName,
    SelectManyShuttleRenderer.ShuttleInfo shuttleInfo)
  {
    int formNameLength = 0;

    if ( shuttleInfo.formName != null )
      formNameLength = shuttleInfo.formName.length();

    int shuttleNameLength = shuttleName.length();

    StringBuffer funcCallBuffer = new StringBuffer(
                                               19 +
                                               formNameLength +
                                               shuttleNameLength);

    funcCallBuffer.append("_resetItems('").append(shuttleName);
    funcCallBuffer.append("','").append(shuttleInfo.formName).append("');");
    FormRenderer.addResetCall( funcCallBuffer.toString());


    StringBuffer script = new StringBuffer(30 +
                                           (2 * shuttleNameLength) +
                                           (2 * formNameLength));
    script.append( "window[\"_" ).append(shuttleInfo.formName);
    script.append('_').append(shuttleName);
    script.append("_orig\"]=_copyLists('");
    script.append(shuttleName).append("','");
    script.append(shuttleInfo.formName).append("');");

    shuttleInfo.resetScript = script.toString();

  }


    private static void _setListInfo(
      SelectManyShuttleRenderer.ContainerInfo containerInfo
    )
    {
      StringBuffer vals = new StringBuffer();

      String baseJSId = XhtmlUtils.getJSIdentifier(containerInfo.listName);
      String descArrayName = baseJSId + _DESCRIPTION_COMPLETE;
      String selArrayName = baseJSId + _SELECTION_COMPLETE;

      StringBuffer descriptions = new StringBuffer( _VAR);
      descriptions.append(selArrayName);
      descriptions.append(_NEW_ARRAY);
      descriptions.append( _VAR);
      descriptions.append(descArrayName);
      descriptions.append( _NEW_ARRAY);

      if (containerInfo.itemsList == null)
        containerInfo.itemsList = Collections.EMPTY_LIST;

      containerInfo.listCount = containerInfo.itemsList.size();

      for(int i=0; i< containerInfo.listCount;i++)
      {
        SelectItem item = (SelectItem) containerInfo.itemsList.get(i);
        String text  = item.getLabel();
        Object value = item.getValue();
        Object description = item.getDescription();
        if ( description == null )
         description = "";

        char[] end = {']','=', '\''};
        descriptions.append( descArrayName );
        descriptions.append( '[' ).append( i ).append(  end );
        BaseDesktopUtils.escapeJS(descriptions,
                           description.toString(),
                           true /* inQuotes */ );
        descriptions.append( '\'').append(';' );

        if(value != null)
           vals.append( value );
        else if(text != null)
          vals.append( text );

        vals.append( ';');

        if (text != null)
        {
          containerInfo.maxWidth = Math.max( text.length(),
                                             containerInfo.maxWidth);
        }
      }

      containerInfo.descriptions = descriptions.toString();
      containerInfo.vals = vals.toString();
    }

    private static SelectManyShuttleRenderer.ShuttleInfo _getShuttleInfo(
      RenderingContext context,
      UINode node
      )
    {
      SelectManyShuttleRenderer.ShuttleInfo shuttleInfo =
                          new SelectManyShuttleRenderer.ShuttleInfo();
      SelectManyShuttleRenderer.ContainerInfo leadingInfo =
                          new SelectManyShuttleRenderer.ContainerInfo();
      SelectManyShuttleRenderer.ContainerInfo trailingInfo =
                          new SelectManyShuttleRenderer.ContainerInfo();

      shuttleInfo.leadingInfo = leadingInfo;
      shuttleInfo.trailingInfo = trailingInfo;

      // get name of shuttle
      String shuttleName = _getShuttleName(context, node);
      shuttleInfo.formName = BaseLafRenderer.getParentFormName(context);

      // list names
      leadingInfo.listName = shuttleName + _LEADING_COMPLETE;
      trailingInfo.listName = shuttleName + _TRAILING_COMPLETE;

      // headers
      leadingInfo.headerText = node.getAttributeValue(context,
                                                      LEADING_HEADER_ATTR);

      trailingInfo.requiredInd = (String) node.getAttributeValue(context,
                                                        UIConstants.REQUIRED_ATTR);
                                                        
      trailingInfo.headerText = node.getAttributeValue(context,
                                                       TRAILING_HEADER_ATTR);

      leadingInfo.itemsList = (List) node.getAttributeValue(context,
         org.apache.myfaces.adfinternal.renderkit.uix.SelectManyShuttleRenderer.SELECT_ITEMS_ATTR);
      trailingInfo.itemsList = (List) node.getAttributeValue(context,
         org.apache.myfaces.adfinternal.renderkit.uix.SelectManyShuttleRenderer.VALUE_ITEMS_ATTR);

      // descriptions
      Object hasLeadingDescArea = node.getAttributeValue( context,
                                                       LEADING_DESC_SHOWN_ATTR);
      Object hasTrailingDescArea = node.getAttributeValue( context,
                                                      TRAILING_DESC_SHOWN_ATTR);

      if ( Boolean.TRUE.equals( hasLeadingDescArea ) )
        leadingInfo.hasDescArea = Boolean.TRUE;

      if ( Boolean.TRUE.equals( hasTrailingDescArea ) )
        trailingInfo.hasDescArea = Boolean.TRUE;



      // footer
      // reorderable
      UIComponent component = NodeUtils.getUIComponent(context, node);
      trailingInfo.isReorderable =
        UIXSelectOrder.COMPONENT_FAMILY.equals(component.getFamily());

      _setListInfo(leadingInfo);
      _setListInfo(trailingInfo);

      _setResetInfo( shuttleName, shuttleInfo);

      // length of lists
      shuttleInfo.listLen = _getBestListLen( context,
                                             node,
                                             leadingInfo.listCount,
                                             trailingInfo.listCount);

      int barWidth = Math.max(Math.max(trailingInfo.maxWidth,
                                       leadingInfo.maxWidth),
                              _BARS_MINIMUM_WIDTH);

      // determine description area width
      // the description area width needs to be tweaked because
      // the description area should be roughly the same
      // size as the list, but just setting the
      // width to be the width of the list isn't enough
      AdfFacesAgent agent = context.getAgent();
      int agentApp = agent.getAgentApplication();
      int descWidth = barWidth;

      // On IE and mozilla width too narrow so increasing
      if ( agentApp == AdfFacesAgent.APPLICATION_IEXPLORER ||
           agentApp == AdfFacesAgent.APPLICATION_GECKO)
        descWidth = (descWidth *6)/5;
      // size would be right in netscape, but in
      // TextInputRenderer we are shrinking width of textinputs
      // on netscape, so to counter this multiplying by 4/3rd's
      else if ( agentApp == AdfFacesAgent.APPLICATION_NETSCAPE )
        descWidth = ( descWidth * 4 )/3;


      shuttleInfo.descWidth = IntegerUtils.getInteger(descWidth) ;

      if( barWidth <= _BARS_MINIMUM_WIDTH)
      {
        shuttleInfo.barText =  _BARS_MINIMUM;
      }
      else
      {
        char[] addedChars = new char[barWidth - _BARS_MINIMUM_WIDTH];

        for(int i=0; i < barWidth - _BARS_MINIMUM_WIDTH; i++)
          addedChars[i] = '_';

        shuttleInfo.barText = _BARS_MINIMUM + new String(addedChars);
      }

      return(shuttleInfo);
    }


  private static final void _clearContext(
    RenderingContext context
  )
  {
    if ( context != null )
    {
      // clear property from context
      context.setProperty( MARLIN_NAMESPACE, _SHUTTLE_INFO_KEY, null);
      context.setProperty( MARLIN_NAMESPACE,_LEADING_INFO_KEY, null);
      context.setProperty( MARLIN_NAMESPACE,_TRAILING_INFO_KEY, null);
    }
  }




  private final static int _MAXIMUM_LIST_LEN   = 20;
  private final static int _MINIMUM_LIST_LEN   = 10;
  private final static int _BARS_MINIMUM_WIDTH = 15;
  private final static Integer _BARS_MINIMUM_WIDTH_INTEGER =
                                                   IntegerUtils.getInteger(15);

  private final static String _BARS_MINIMUM    = "_______________";


  private final static int _DEFAULT_DESC_AREA_HEIGHT = 68;
  private final static int _DEFAULT_FILTER_HEIGHT    = 36;
  private final static int _DEFAULT_FOOTER_HEIGHT    = 36;


  private final static String _DEFAULT_SHUTTLE_NAME = "theShuttle";
  private final static String _LEADING_COMPLETE     = ":leading";
  private final static String _TRAILING_COMPLETE    = ":trailing";
  private final static String _ITEMS_COMPLETE       = ":items";
  private final static String _DESCRIPTION_COMPLETE = "_desc";
  private final static String _SELECTION_COMPLETE   = "_sel";
  private final static String _VAR                  = "var ";
  private final static String _NEW_ARRAY            = "=new Array();";

  private final static String _TRANSLATED_JS_FEEDBACK_NO_ITEMS
                                 = "var _shuttle_no_items='";
  private final static int _TRANSLATED_JS_FEEDBACK_NO_ITEMS_LENGTH =
                       _TRANSLATED_JS_FEEDBACK_NO_ITEMS.length() + 2;
  private final static String _TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED
                                 = "var _shuttle_no_items_selected='";
  private final static int _TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED_LENGTH =
                       _TRANSLATED_JS_FEEDBACK_NO_ITEMS_SELECTED.length() + 2;
  private static final Object _TRANSLATED_VARS_EXIST_PROPERTY_KEY =
                                                             new Object();

  private final static Object _SHUTTLE_INFO_KEY = new Object();
  private final static Object _LEADING_INFO_KEY = new Object();
  private final static Object _TRAILING_INFO_KEY = new Object();


  private static final BoundValue _ITEMS_COMPLETE_BV    =
                                             new FixedBoundValue( _ITEMS_COMPLETE );
  private static final BoundValue _DESC_COMPLETE_BV    =
                                             new FixedBoundValue( ":desc" );

  private static final BoundValue _DISPLAY_DESC_BV   =
                            new FixedBoundValue( "_displayDesc('");
  private static final BoundValue _MOVE_ITEMS_NO_JS_PREFIX_BV     =
                            new FixedBoundValue( "_moveItems('" );
  private static final BoundValue _MOVE_ITEMS_BV     =
                            new FixedBoundValue( "javascript:_moveItems('" );
  private static final BoundValue _MOVE_ALL_ITEMS_BV =
                            new FixedBoundValue( "javascript:_moveAllItems('" );

  private static final BoundValue _ORDER_ITEMS_TOP_BV    =
                    new FixedBoundValue( "javascript:_orderTopBottomList(0,'" );
  private static final BoundValue _ORDER_ITEMS_BOTTOM_BV =
                    new FixedBoundValue( "javascript:_orderTopBottomList(1,'" );
  private static final BoundValue _ORDER_ITEMS_UP_BV     =
                    new FixedBoundValue( "javascript:_orderList(0, '" );
  private static final BoundValue _ORDER_ITEMS_DOWN_BV   =
                    new FixedBoundValue( "javascript:_orderList(1, '" );

  private static final BoundValue _COMMA_BV = new FixedBoundValue( "','" );
  private static final BoundValue _END_FUNC_BV = new FixedBoundValue( "');" );

  private final static HTMLWebBean _BR = new HTMLWebBean( BREAK_ELEMENT );

  private static UINode _INSTANCE;

  // BoundValue which checks whether an Icon is available
  private static class CheckIconBoundValue implements BoundValue
  {
    private CheckIconBoundValue(String iconName)
    {
      _iconName = iconName;
    }

    public Object getValue(RenderingContext context)
    {
      Skin skin = context.getSkin();

      return ((skin.getIcon(_iconName) == null) ?
                 Boolean.FALSE :
                 Boolean.TRUE);
    }

    private String _iconName;
  }

  // BoundValue which checks whether the required Icon is  to be rendered
  private static class RequiredIconBoundValue implements BoundValue
  {
    public Object getValue(RenderingContext context)
    {
      RenderingContext parentContext = context.getParentContext();
      if (parentContext != null)
      {
        UIComponent component = NodeUtils.getUIComponent(parentContext,
                                                         parentContext.getAncestorNode(0));

        Object requiredAttr = component.getAttributes().get("required");
        if (Boolean.TRUE.equals(requiredAttr))
        {
          return Boolean.TRUE;         
        }

        requiredAttr = component.getAttributes().get("showRequired");
        if (Boolean.TRUE.equals(requiredAttr))
        {
          return Boolean.TRUE;         
        }
      }

      return Boolean.FALSE;
    }
  }

 // BoundValue for Required Tip
  private static class RequiredTipBoundValue implements BoundValue
  {
    public Object getValue(RenderingContext context)
    {
       Object requiredTip = 
              context.getTranslatedValue("REQUIRED_TIP");
       
       return requiredTip;
    }
  }

  /*
   * Per render shuttle info
   */
  private static class ShuttleInfo
  {
    public String  barText       = "";
    public Object listLen        =  ZERO;
    public Integer descWidth     = _BARS_MINIMUM_WIDTH_INTEGER;
    public String formName       = null;
    public String resetScript    = null;

    public SelectManyShuttleRenderer.ContainerInfo leadingInfo = null;
    public SelectManyShuttleRenderer.ContainerInfo trailingInfo = null;

    public Object[] getData()
    {
      return new Object[]{barText,
                          listLen,
                          descWidth,
                          formName,
                          resetScript};
    }

    public static int BAR_TEXT_KEY = 0;
    public static int LIST_LEN_KEY = 1;
    public static int DESC_WIDTH_KEY = 2;
    public static int FORM_NAME_KEY = 3;
    public static int RESET_SCRIPT_KEY = 4;

  }

  /*
   * Per render info for each container
   */
  private static class ContainerInfo
  {

    public String  vals           = "";
    public String  descriptions   = "";
    public String  listName       = "";
    public Object  headerText     = "";
    public String  requiredInd    = "";
    public Boolean isReorderable  = Boolean.FALSE;
    public Boolean hasDescArea    = Boolean.FALSE;

    public List itemsList;

    public int maxWidth  = 0;
    public int listCount = 0;

    public Object[] getData()
    {
      return new Object[]{ vals,
                           descriptions,
                           listName,
                           headerText,
                           isReorderable,
                           hasDescArea,
                           itemsList};
    }

    public static int VALS_KEY           = 0;
    public static int DESCRIPTIONS_KEY   = 1;
    public static int LIST_NAME_KEY      = 2;
    public static int HEADER_TEXT_KEY    = 3;
    public static int IS_REORDERABLE_KEY = 4;
    public static int HAS_DESC_AREA_KEY  = 5;
    public static int ITEMS_LIST_KEY     = 6;

  }

  /*
   * options in list plus bar at bottom
   */
  private static class OptionsUINodeList implements UINodeList
  {
    public OptionsUINodeList(
      BoundValue itemsBV,
      UINode     lastNode
      )
    {
      _itemsBV  = itemsBV;
      _lastNode = lastNode;
    }

    public UINode getUINode(RenderingContext context,
                        int index)
    {
      List selectItems = _getSelectItems(context);
      if ((selectItems == null) && index == 0)
        return _lastNode;

      int size = selectItems.size();
      if (size == index)
        return _lastNode;

      SelectItem item = (SelectItem) selectItems.get(index);
      MarlinBean option = new MarlinBean(OPTION_NAME);
      option.setAttributeValue(TEXT_ATTR, item.getLabel());
      option.setAttributeValue(VALUE_ATTR, item.getValue());
      option.setAttributeValue(DISABLED_ATTR, Boolean
          .valueOf(item.isDisabled()));
      option.setAttributeValue(LONG_DESC_ATTR, item.getDescription());
      return option;
    }

    public int size(RenderingContext context)
    {
      List selectItems = _getSelectItems(context);
      if (selectItems == null)
        return 1;
      return selectItems.size() + 1;
    }

    public UINode setUINode(int index, UINode node)
    {
      throw new UnsupportedOperationException();
    }


    public void   addUINode(int index, UINode node)
    {
      throw new UnsupportedOperationException();
    }

    public void   addUINode(UINode node)
    {
      throw new UnsupportedOperationException();
    }

    public UINode removeUINode(int index)
    {
      throw new UnsupportedOperationException();
    }

    public void clearUINodes()
    {
      throw new UnsupportedOperationException();
    }

    public Object clone()
    {
      throw new UnsupportedOperationException();
    }

    private List _getSelectItems(RenderingContext context)
    {
      return (List) _itemsBV.getValue(context);
    }

    private BoundValue _itemsBV;
    private UINode     _lastNode;
  }

  /*
   * Per render shuttle info in BV
   */
  private static class ShuttleInfoBV implements BoundValue
  {
    /*
     * property - shuttle or one of the containers
     * index - index into property
     */
    ShuttleInfoBV(
      Object propertyKey,
      int    index)
    {
      _key = propertyKey;
      _index = index;
    }

    public Object getValue(
      RenderingContext context
      )
    {
      if ( context != null )
      {
        Object[] data = (Object[])context.getProperty( MARLIN_NAMESPACE, _key);

        if ( data == null )
          return null;

        return data[_index];
      }

      return null;
    }

    Object _key;
    int    _index;
  }

  /**
   * Get the component, and if its component family is
   * UIXSelectOrder.COMPONENT_FAMILY, then set the _RESOURCE_KEY_MAP
   * which maps the selectMany translation keys to selectOne translation keys.
   * =-=jmw @todo find out if I should clear this map, or if it is ok since
   * the composite renderer resets it.
   * @param context
   * @param node
   */
  private void _setSelectOrderResourceKeyMap(
    RenderingContext context,
    UINode           node)
  {
    UIComponent component = BaseLafRenderer.getUIComponent(context, node);
    if ((component != null) &&
        ((UIXSelectOrder.COMPONENT_FAMILY).equals(component.getFamily())))
    {
        context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
    }
    else
    {
      context.setSkinResourceKeyMap(_SHUTTLE_KEY_MAP);
  }
  }

  // translation keys.
  // Feedback in javascript alert when user tries to use
  // move/remove buttons when there are no items
  private final static String _SHUTTLE_NO_ITEMS_FEEDBACK_KEY =
    "SHUTTLE_NO_ITEMS_FEEDBACK";
  private final static String _SHUTTLE_NO_ITEM_SELECTED_FEEDBACK_KEY =
    "SHUTTLE_NO_ITEM_SELECTED_FEEDBACK";

  private final static String _SELECT_MANY_DESCRIPTION_LABEL_KEY =
    "af_selectManyShuttle.DESCRIPTION_LABEL";
  private final static String _SELECT_ORDER_DESCRIPTION_LABEL_KEY =
    "af_selectOrderShuttle.DESCRIPTION_LABEL";
  private final static String _SELECT_MANY_MOVE_ALL_TIP_KEY =
    "af_selectManyShuttle.MOVE_ALL_TIP";
  private final static String _SELECT_ORDER_MOVE_ALL_TIP_KEY =
    "af_selectOrderShuttle.MOVE_ALL_TIP";
  private final static String _SELECT_MANY_MOVE_TIP_KEY =
    "af_selectManyShuttle.MOVE_TIP";
  private final static String _SELECT_ORDER_MOVE_TIP_KEY =
    "af_selectOrderShuttle.MOVE_TIP";
  private final static String _SELECT_MANY_REMOVE_ALL_TIP_KEY =
    "af_selectManyShuttle.REMOVE_ALL_TIP";
  private final static String _SELECT_ORDER_REMOVE_ALL_TIP_KEY =
    "af_selectOrderShuttle.REMOVE_ALL_TIP";
  private final static String _SELECT_MANY_REMOVE_TIP_KEY =
    "af_selectManyShuttle.REMOVE_TIP";
  private final static String _SELECT_ORDER_REMOVE_TIP_KEY =
    "af_selectOrderShuttle.REMOVE_TIP";


  private final static String _SELECT_MANY_MOVE_ALL_KEY =
    "af_selectManyShuttle.MOVE_ALL";
  private final static String _SELECT_ORDER_MOVE_ALL_KEY =
    "af_selectOrderShuttle.MOVE_ALL";
  private final static String _SELECT_MANY_MOVE_KEY =
    "af_selectManyShuttle.MOVE";
  private final static String _SELECT_ORDER_MOVE_KEY =
    "af_selectOrderShuttle.MOVE";
  private final static String _SELECT_MANY_REMOVE_ALL_KEY =
    "af_selectManyShuttle.REMOVE_ALL";
  private final static String _SELECT_ORDER_REMOVE_ALL_KEY =
    "af_selectOrderShuttle.REMOVE_ALL";
  private final static String _SELECT_MANY_REMOVE_KEY =
    "af_selectManyShuttle.REMOVE";
  private final static String _SELECT_ORDER_REMOVE_KEY =
    "af_selectOrderShuttle.REMOVE";
 
  // map the selectMany translation keys to the selectOrder translation keys.
  // This renderer code uses the selectMany translation keys. If selectOrder
  // shuttle is being rendered, then context.getTranslatedValue will use
  // this translation map to map the selectMany keys to the selectOrder keys.
  private static final Map _RESOURCE_KEY_MAP  =  new HashMap();
  private static final Map _SHUTTLE_KEY_MAP   = new HashMap();

  static private class IsReadOnly implements BoundValue
  {
    public Object getValue(RenderingContext context)
    {
      
      if (Boolean.TRUE.equals(_readOnlyRoot.getValue(context)))
        return Boolean.TRUE;

      RenderingContext parentContext = context.getParentContext();
      if (parentContext != null)
      {
        UIComponent component = NodeUtils.getUIComponent(parentContext,
                                                         parentContext.getAncestorNode(0));
        ValueBinding vb = component.getValueBinding("value");
        if (vb != null)
        {
          FacesContext fContext = (context == null) ? 
            FacesContext.getCurrentInstance() : context.getFacesContext();
          if (vb.isReadOnly(fContext))
            return Boolean.TRUE;
        }

      }

      return Boolean.FALSE;
    }
    
    static private final BoundValue _readOnlyRoot = RootAttributeBoundValue.getBoundValue(READ_ONLY_ATTR);
  }


  static
  {
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_DESCRIPTION_LABEL_KEY,
                              _SELECT_ORDER_DESCRIPTION_LABEL_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_MOVE_ALL_TIP_KEY,
                              _SELECT_ORDER_MOVE_ALL_TIP_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_MOVE_TIP_KEY,
                              _SELECT_ORDER_MOVE_TIP_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_REMOVE_ALL_TIP_KEY,
                              _SELECT_ORDER_REMOVE_ALL_TIP_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_REMOVE_TIP_KEY,
                              _SELECT_ORDER_REMOVE_TIP_KEY);

    _RESOURCE_KEY_MAP.put(_SELECT_MANY_MOVE_ALL_KEY,
                              _SELECT_ORDER_MOVE_ALL_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_MOVE_KEY,
                              _SELECT_ORDER_MOVE_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_REMOVE_ALL_KEY,
                              _SELECT_ORDER_REMOVE_ALL_KEY);
    _RESOURCE_KEY_MAP.put(_SELECT_MANY_REMOVE_KEY,
                              _SELECT_ORDER_REMOVE_KEY);

    // add the icons that are shared between selectManyShuttle
    // and selectOrderShuttle to resource key map
    // Move, Move All, Remove, RemoveAll
    _RESOURCE_KEY_MAP.put(AF_SELECT_MANY_SHUTTLE_MOVE_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_MOVE_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_SELECT_MANY_SHUTTLE_MOVE_ALL_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_MOVE_ALL_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_SELECT_MANY_SHUTTLE_REMOVE_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_REMOVE_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_SELECT_MANY_SHUTTLE_REMOVE_ALL_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_REMOVE_ALL_ICON_NAME);


    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_BODY_STYLE_CLASS,
                          AF_SELECT_ORDER_SHUTTLE_PB_BODY_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_CONTENT_LIGHT_STYLE_CLASS,
                          AF_SELECT_ORDER_SHUTTLE_PB_CONTENT_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_BG_LIGHT_STYLE_CLASS,
                          AF_SELECT_ORDER_SHUTTLE_PB_BG_LIGHT_STYLE_CLASS);

    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_START_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_BOTTOM_START_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_END_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_BOTTOM_END_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_START_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_TOP_START_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_END_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_TOP_END_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_BG_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_BOTTOM_BG_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_BG_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_TOP_BG_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_START_BG_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_START_BG_ICON_NAME);
    _RESOURCE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_END_BG_ICON_NAME,
                          AF_SELECT_ORDER_SHUTTLE_END_BG_ICON_NAME);


    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_BODY_STYLE_CLASS,
                          AF_SELECT_MANY_SHUTTLE_PB_BODY_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_CONTENT_LIGHT_STYLE_CLASS,
                          AF_SELECT_MANY_SHUTTLE_PB_CONTENT_STYLE_CLASS);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_BG_LIGHT_STYLE_CLASS,
                          AF_SELECT_MANY_SHUTTLE_PB_BG_LIGHT_STYLE_CLASS);

    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_START_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_BOTTOM_START_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_END_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_BOTTOM_END_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_START_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_TOP_START_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_END_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_TOP_END_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_BOTTOM_BG_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_BOTTOM_BG_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_TOP_BG_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_TOP_BG_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_START_BG_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_START_BG_ICON_NAME);
    _SHUTTLE_KEY_MAP.put(AF_PANEL_BOX_LIGHT_END_BG_ICON_NAME,
                          AF_SELECT_MANY_SHUTTLE_END_BG_ICON_NAME);


  }

}
