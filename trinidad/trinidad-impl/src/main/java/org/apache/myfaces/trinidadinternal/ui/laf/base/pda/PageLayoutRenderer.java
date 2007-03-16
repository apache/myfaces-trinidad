/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;

import org.apache.myfaces.trinidadinternal.ui.composite.ContextPoppingUINode;
import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeMap;
import org.apache.myfaces.trinidadinternal.ui.composite.RootUINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;

import org.apache.myfaces.trinidadinternal.ui.data.bind.NotBoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.ui.data.bind.OrBoundValue;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;


/**
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PageLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:00 $
 */
public class PageLayoutRenderer extends UINodeRenderer
                  implements UIConstants, XhtmlLafConstants
{
  private static UINode _createCompositeUINode()
  {

    //
    // Create the page header layout
    //
    MarlinBean headerLayout = new MarlinBean(PAGE_HEADER_LAYOUT_NAME);

    headerLayout.setNamedChild(NAVIGATION_GLOBAL_CHILD,
                    ContextPoppingUINode.getUINode(NAVIGATION_GLOBAL_CHILD));
    headerLayout.setNamedChild(NAVIGATION1_CHILD,
                    ContextPoppingUINode.getUINode(NAVIGATION1_CHILD));

    headerLayout.setNamedChild(NAVIGATION2_CHILD, _createGlobalHeaders());

    headerLayout.setNamedChild(BRANDING_CHILD,
                    ContextPoppingUINode.getUINode(BRANDING_CHILD));
    headerLayout.setNamedChild(BRANDING_APP_CHILD,
                    ContextPoppingUINode.getUINode(BRANDING_APP_CHILD));

    // =-= bts a little bogus since this isn't really an attribute of PageHeader
    headerLayout.setAttributeValue(WIDTH_ATTR, "100%");

    //
    // Create the content container containing all of the indexed children
    //
    //  MarlinBean contentRoot = new MarlinBean(FLOW_LAYOUT_NAME);

    // use stackLayout as the default layout as it is the layout used in
    // the desktop version.
    MarlinBean contentRoot = new MarlinBean(STACK_LAYOUT_NAME);
    contentRoot.setIndexedNodeList(RootUINodeList.getNodeList());

    //
    // add the page buttons line
    //
    MarlinBean pageButtonsLine = new MarlinBean(SEPARATOR_NAME);
    pageButtonsLine.setAttributeValue(
            RENDERED_ATTR,
             new OrBoundValue(
         PdaHtmlLafUtils.createIsRenderedBoundValue(ACTIONS_CHILD),
         PdaHtmlLafUtils.createIsRenderedBoundValue(INFO_RETURN_CHILD)));




    // Create the area containing the footer

    MarlinBean footerTable = new MarlinBean(TABLE_LAYOUT_NAME);
    footerTable.setAttributeValue( WIDTH_ATTR, ONE_HUNDRED_PERCENT_ATTRIBUTE_VALUE);

    MarlinBean globalButtonsRow = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean globalButtonsCell = new MarlinBean(CELL_FORMAT_NAME);
    globalButtonsCell.setAttributeValue( H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    globalButtonsCell.addIndexedChild(
                        ContextPoppingUINode.getUINode(NAVIGATION_GLOBAL_CHILD));
    globalButtonsRow.addIndexedChild( globalButtonsCell);
    footerTable.addIndexedChild( globalButtonsRow);

    MarlinBean copyrightRow = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean copyrightCell = new MarlinBean(CELL_FORMAT_NAME);
    copyrightCell.setAttributeValue( H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    copyrightCell.setStyleClass( AF_PANEL_PAGE_COPYRIGHT_STYLE_CLASS ) ;
    copyrightCell.addIndexedChild(ContextPoppingUINode.getUINode(APP_COPYRIGHT_CHILD));
    copyrightRow.addIndexedChild( copyrightCell);
    footerTable.addIndexedChild( copyrightRow);


    MarlinBean privacyRow = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean privacyCell = new MarlinBean(CELL_FORMAT_NAME);
    privacyCell.setAttributeValue( H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    privacyCell.setStyleClass( AF_PANEL_PAGE_PRIVACY_STYLE_CLASS ) ;
    privacyCell.addIndexedChild(ContextPoppingUINode.getUINode(APP_PRIVACY_CHILD));
    privacyRow.addIndexedChild( privacyCell);
    footerTable.addIndexedChild( privacyRow);


    MarlinBean aboutRow = new MarlinBean(ROW_LAYOUT_NAME);
    MarlinBean aboutCell = new MarlinBean(CELL_FORMAT_NAME);
    aboutCell.setAttributeValue( H_ALIGN_ATTR, CENTER_ATTRIBUTE_VALUE);
    aboutCell.setStyleClass( AF_PANEL_PAGE_ABOUT_STYLE_CLASS ) ;
    aboutCell.addIndexedChild(ContextPoppingUINode.getUINode(APP_ABOUT_CHILD));
    aboutRow.addIndexedChild( aboutCell);
    footerTable.addIndexedChild( aboutRow);




    MarlinBean footer = new MarlinBean(FLOW_LAYOUT_NAME);
    MarlinBean footerLine = new MarlinBean(CONTENT_FOOTER_NAME);
    footer.addIndexedChild(footerLine);
    footer.addIndexedChild(footerTable);

    BoundValue renderFooter = new OrBoundValue(new BoundValue[]{
               PdaHtmlLafUtils.createIsRenderedBoundValue(APP_PRIVACY_CHILD),
               PdaHtmlLafUtils.createIsRenderedBoundValue(APP_ABOUT_CHILD),
               PdaHtmlLafUtils.createIsRenderedBoundValue(APP_COPYRIGHT_CHILD),
               PdaHtmlLafUtils.createIsRenderedBoundValue(NAVIGATION_GLOBAL_CHILD)
                   });
    footer.setAttributeValue( RENDERED_ATTR, renderFooter);




    MarlinBean content = new MarlinBean(STACK_LAYOUT_NAME);
     content.addIndexedChild( ContextPoppingUINode.getUINode(LOCATION_CHILD));
    content.addIndexedChild( contentRoot );
    content.addIndexedChild(pageButtonsLine);

    //
    // Add the footer children
    //
    content.addIndexedChild(
                       ContextPoppingUINode.getUINode(ACTIONS_CHILD));
    content.addIndexedChild(
                       ContextPoppingUINode.getUINode(INFO_RETURN_CHILD));

    MarlinBean compositeRoot = new MarlinBean(FLOW_LAYOUT_NAME);

    // delegate all of the attributes to the RootAttribtueMap
    compositeRoot.setAttributeMap(RootAttributeMap.getAttributeMap());

    compositeRoot.addIndexedChild(headerLayout);

    compositeRoot.addIndexedChild(
                             ContextPoppingUINode.getUINode( MESSAGES_CHILD ));

    compositeRoot.addIndexedChild(content);

    compositeRoot.addIndexedChild(footer);

    return compositeRoot;
  }


  /**
   * Create the global headers to use to render the page.  The first child is
   * the global header set on the page layout, the second is the global
   * header to use if the first client global header doesn't exist or
   * isn't rendered.
   */
  private static UINode _createGlobalHeaders()
  {
    MarlinBean globalHeaders = new MarlinBean(FLOW_LAYOUT_NAME);

    //
    // add the client header
    //
    globalHeaders.addIndexedChild(
                       ContextPoppingUINode.getUINode(NAVIGATION2_CHILD));

    //
    // create and add the default header
    //
    MarlinBean defaultHeader = new MarlinBean(GLOBAL_HEADER_NAME);

    defaultHeader.setAttributeValue(
            RENDERED_ATTR,
            new NotBoundValue(
               PdaHtmlLafUtils.createIsRenderedBoundValue(NAVIGATION2_CHILD)));

    globalHeaders.addIndexedChild(defaultHeader);

    return globalHeaders;
  }

  @Override
  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _INSTANCE;
  }

  private static final UINode _INSTANCE = _createCompositeUINode();
}
