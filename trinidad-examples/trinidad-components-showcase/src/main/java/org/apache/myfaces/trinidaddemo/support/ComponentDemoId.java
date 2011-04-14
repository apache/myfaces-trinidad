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
package org.apache.myfaces.trinidaddemo.support;

/**
 * Defines the unique identifiers of each component that has a demo available. As a naming convention, the corresponding component 
 * tag name is used.
 */
public enum ComponentDemoId {

    //  Panel cathegory components
    form,
    group,
    page,
	panelAccordion,
    panelBox,
    panelButtonBar,
    panelCaptionGroup,
    panelChoice,
    panelHeader,
    panelLabelAndMessage,
    panelList,
    panelPage,
    panelPageHeader,
    panelPopup,
    panelRadio,
    panelSideBar,
    panelTabbed,
    panelTip,
    
    //  Layout cathegory components
    panelBorderLayout,
	panelFormLayout,
	panelGroupLayout,
    panelHorizontalLayout,
    separator,
    spacer,

    //  Input cathegory components
    inputColor,
    inputDate,
    inputFile,
    inputHidden,
    inputListOfValues,
    inputNumberSpinbox,
    inputText,

    //  Output cathegory components
    chart,
    legend,
    message,
    messages,
    outputDocument,
    outputFormatted,
    outputLabel,
    outputText,
    progressIndicator,
    statusIndicator,

    //  Buttons & Links cathegory components
    commandButton,
    commandLink,
    goButton,
    goLink,
    resetButton,

    //  Panel cathegory components
    selectBooleanCheckbox,
    selectBooleanRadio,
    chooseColor,
    chooseDate,
    selectItem,
    selectManyCheckbox,
    selectManyListbox,
    selectManyShuttle,
    selectOneChoice,
    selectOneListbox,
    selectOneRadio,
    selectOrderShuttle,
    selectRangeChoiceBar,

    //  Table cathegory components
    column,
    table,
    treeTable,

    //  Show Detail cathegory components
    showDetail,
    showDetailHeader,
    showDetailItem,

    //  Navigation cathegory components
    breadCrumbs,
    commandNavigationItem,
    navigationTree,
    navigationPane,
    processChoiceBar,
    singleStepButtonBar,
    train,
    tree,

    //  Graphic cathegory components
    icon,
    image,
    media;
}
