/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidaddemo;

import org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.commandButton.CommandButtonDemo;
import org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.commandLink.CommandLinkDemo;
import org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.goButton.GoButtonDemo;
import org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.goLink.GoLinkDemo;
import org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.resetButton.ResetButtonDemo;
import org.apache.myfaces.trinidaddemo.components.graphic.icon.IconDemo;
import org.apache.myfaces.trinidaddemo.components.graphic.image.ImageDemo;
import org.apache.myfaces.trinidaddemo.components.graphic.media.MediaDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputColor.InputColorDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputDate.InputDateDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputFile.InputFileDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputListOfValues.InputListOfValuesDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputNumberSpinbox.InputNumberSpinboxDemo;
import org.apache.myfaces.trinidaddemo.components.input.inputText.InputTextDemo;
import org.apache.myfaces.trinidaddemo.components.layout.panelBorderLayout.BorderLayoutDemo;
import org.apache.myfaces.trinidaddemo.components.layout.panelFormLayout.FormLayoutDemo;
import org.apache.myfaces.trinidaddemo.components.layout.panelGroupLayout.GroupLayoutDemo;
import org.apache.myfaces.trinidaddemo.components.layout.panelHorizontalLayout.HorizontalLayoutDemo;
import org.apache.myfaces.trinidaddemo.components.layout.separator.SeparatorDemo;
import org.apache.myfaces.trinidaddemo.components.layout.spacer.SpacerDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.breadCrumbs.BreadCrumbsDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.commandNavigationItem.CommandNavigationItemDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.navigationPane.NavigationPaneDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.navigationTree.NavigationTreeDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.processChoiceBar.ProcessChoiceBarDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.singleStepButtonBar.SingleStepButtonBarDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.train.TrainDemo;
import org.apache.myfaces.trinidaddemo.components.navigation.tree.TreeDemo;
import org.apache.myfaces.trinidaddemo.components.output.chart.ChartDemo;
import org.apache.myfaces.trinidaddemo.components.output.legend.LegendDemo;
import org.apache.myfaces.trinidaddemo.components.output.message.MessageDemo;
import org.apache.myfaces.trinidaddemo.components.output.messages.MessagesDemo;
import org.apache.myfaces.trinidaddemo.components.output.outputDocument.OutputDocumentDemo;
import org.apache.myfaces.trinidaddemo.components.output.outputFormated.OutputFormatedDemo;
import org.apache.myfaces.trinidaddemo.components.output.outputLabel.OutputLabelDemo;
import org.apache.myfaces.trinidaddemo.components.output.outputText.OutputTextDemo;
import org.apache.myfaces.trinidaddemo.components.output.progessIndicator.ProgressIndicatorDemo;
import org.apache.myfaces.trinidaddemo.components.output.statusIndicator.StatusIndicatorDemo;
import org.apache.myfaces.trinidaddemo.components.panel.group.GroupDemo;
import org.apache.myfaces.trinidaddemo.components.panel.page.PageDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelAccordion.AccordionPanelDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelBox.PanelBoxDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelButtonBar.PanelButtonBarDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelCaptionGroup.PanelCaptionGroupDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelChoice.PanelChoiceDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelHeader.PanelHeaderDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelLabelAndMessage.PanelLabelAndMessageDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelList.PanelListDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelPage.PanelPageDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelPageHeader.PanelPageHeaderDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelPopup.PanelPopupDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelRadio.PanelRadioDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelSideBar.PanelSideBarDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelTabbed.PanelTabbedDemo;
import org.apache.myfaces.trinidaddemo.components.panel.panelTip.PanelTipDemo;
import org.apache.myfaces.trinidaddemo.components.select.chooseColor.ChooseColorDemo;
import org.apache.myfaces.trinidaddemo.components.select.chooseDate.ChooseDateDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectBooleanCheckbox.SelectBooleanCheckboxDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectBooleanRadio.SelectBooleanRadioDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectItem.SelectItemDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectManyCheckbox.SelectManyCheckboxDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectManyListbox.SelectManyListboxDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectManyShuttle.SelectManyShuttleDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectOneChoice.SelectOneChoiceDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectOneListbox.SelectOneListboxDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectOneRadio.SelectOneRadioDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectOrderShuttle.SelectOrderShuttleDemo;
import org.apache.myfaces.trinidaddemo.components.select.selectRangeChoiceBar.SelectRangeChoiceBarDemo;
import org.apache.myfaces.trinidaddemo.components.showDetail.showDetail.ShowDetailDemo;
import org.apache.myfaces.trinidaddemo.components.showDetail.showDetailHeader.ShowDetailHeaderDemo;
import org.apache.myfaces.trinidaddemo.components.showDetail.showDetailItem.ShowDetailItemDemo;
import org.apache.myfaces.trinidaddemo.components.table.column.ColumnDemo;
import org.apache.myfaces.trinidaddemo.components.table.table.TableDemo;
import org.apache.myfaces.trinidaddemo.components.table.treeTable.TreeTableDemo;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoCategoryId;

/**
 *
 */
public class ComponentDemoInitializer  {

    private static ComponentDemoInitializer instance = new ComponentDemoInitializer();
    
    private static final String CATEGORY_PANEL_TITLE   = "Panel";
    private static final String CATEGORY_LAYOUT_TITLE  = "Layout";
    private static final String CATEGORY_INPUT_TITLE   = "Input";
    private static final String CATEGORY_OUTPUT_TITLE  = "Output";
    private static final String CATEGORY_BUTTON_AND_LINKS_TITLE  = "Buttons & Links";
    private static final String CATEGORY_SELECT_TITLE  = "Select";
    private static final String CATEGORY_TABLE_TITLE   = "Table";
    private static final String CATEGORY_SHOW_DETAILS_TITLE   = "Show detail";
    private static final String CATEGORY_NAVIGATION_TITLE   = "Navigation";
    private static final String CATEGORY_GRAPHIC_TITLE   = "Graphic";

    /**
     * Constructor.
     */
    private ComponentDemoInitializer(){}

    /**
     * Gets the singleton instance of the component demos initializer.
     *
     * @return a ComponentDemoInitializer.
     */
    public static ComponentDemoInitializer getInstance() {
        return instance;
    }

	public void init() {
		ComponentDemoRegistry registry = ComponentDemoRegistry.getInstance();
        registerComponentDemos(registry);
	}

    /**
     * @param registry
     */
    public void registerComponentDemos(ComponentDemoRegistry registry) {
        //registering Panel category components
        //registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new FormDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new GroupDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PageDemo());
		registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new AccordionPanelDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelBoxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelButtonBarDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelCaptionGroupDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelChoiceDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelHeaderDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelLabelAndMessageDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelListDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelPageDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelPageHeaderDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelPopupDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelRadioDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelSideBarDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelTabbedDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.panel, CATEGORY_PANEL_TITLE, new PanelTipDemo());

        //  registering Layout category components
		registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new BorderLayoutDemo());
		registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new FormLayoutDemo());
		registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new GroupLayoutDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new HorizontalLayoutDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new SeparatorDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.layout, CATEGORY_LAYOUT_TITLE, new SpacerDemo());

        //  registering Input category components
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputColorDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputDateDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputFileDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputListOfValuesDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputNumberSpinboxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.input, CATEGORY_INPUT_TITLE, new InputTextDemo());

        //  registering Output category components
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new ChartDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new LegendDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new MessageDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new MessagesDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new OutputDocumentDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new OutputFormatedDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new OutputLabelDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new OutputTextDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new ProgressIndicatorDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.output, CATEGORY_OUTPUT_TITLE, new StatusIndicatorDemo());

        //  registering Buttons & Links category components
        registry.registerComponentDemo(ComponentDemoCategoryId.buttonsAndLinks, CATEGORY_BUTTON_AND_LINKS_TITLE, new CommandButtonDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.buttonsAndLinks, CATEGORY_BUTTON_AND_LINKS_TITLE, new CommandLinkDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.buttonsAndLinks, CATEGORY_BUTTON_AND_LINKS_TITLE, new GoButtonDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.buttonsAndLinks, CATEGORY_BUTTON_AND_LINKS_TITLE, new GoLinkDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.buttonsAndLinks, CATEGORY_BUTTON_AND_LINKS_TITLE, new ResetButtonDemo());

        //  registering Select category components
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new ChooseColorDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new ChooseDateDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectBooleanCheckboxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectBooleanRadioDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectItemDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectManyCheckboxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectManyListboxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectManyShuttleDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectOneChoiceDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectOneListboxDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectOneRadioDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectOrderShuttleDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.select, CATEGORY_SELECT_TITLE, new SelectRangeChoiceBarDemo());

        //  registering Select category components
        registry.registerComponentDemo(ComponentDemoCategoryId.table, CATEGORY_TABLE_TITLE, new ColumnDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.table, CATEGORY_TABLE_TITLE, new TableDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.table, CATEGORY_TABLE_TITLE, new TreeTableDemo());

        //  registering Show Detail category components
        registry.registerComponentDemo(ComponentDemoCategoryId.showDetail, CATEGORY_SHOW_DETAILS_TITLE, new ShowDetailDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.showDetail, CATEGORY_SHOW_DETAILS_TITLE, new ShowDetailHeaderDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.showDetail, CATEGORY_SHOW_DETAILS_TITLE, new ShowDetailItemDemo());

        //registering Show Detail category components
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new BreadCrumbsDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new CommandNavigationItemDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new NavigationPaneDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new NavigationTreeDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new ProcessChoiceBarDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new SingleStepButtonBarDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new TrainDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.navigation, CATEGORY_NAVIGATION_TITLE, new TreeDemo());

        //  registering Media category components
        registry.registerComponentDemo(ComponentDemoCategoryId.graphic, CATEGORY_GRAPHIC_TITLE, new IconDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.graphic, CATEGORY_GRAPHIC_TITLE, new ImageDemo());
        registry.registerComponentDemo(ComponentDemoCategoryId.graphic, CATEGORY_GRAPHIC_TITLE, new MediaDemo());
    }
}
