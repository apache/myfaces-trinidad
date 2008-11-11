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
package org.apache.myfaces.trinidaddemo.resource;

import java.util.ListResourceBundle;
/*
 * This class is used in the demo's trinidad-skins.xml file when
 * setting bundle-name. Run the panelPageSkinDemo.jspx
 * page to see some of these translation values in action.
 */
public class SkinBundle extends ListResourceBundle
{
  @Override
  public Object[][] getContents()
  {
    return _CONTENTS;
  }

  static private final Object[][] _CONTENTS =
  {
    {"af_tableSelectMany.SELECT_COLUMN_HEADER", "SBundle Select A Lot"},
    {"af_tableSelectOne.SELECT_COLUMN_HEADER", "SBundle Select Just One"},
    {"af_inputDate.LAUNCH_PICKER_TIP", "SBundle Launch Picker"},
    {"Birds.SELECT_MANY", "SBundle Select Many"},
    {"af_showDetail.DISCLOSED_TIP", "SBundle Hide Tip"},
    {"af_showDetail.DISCLOSED", "SBundle Hide"},
    {"af_showDetail.UNDISCLOSED_TIP", "SBundle Show Tip"},
    {"af_showDetail.UNDISCLOSED", "SBundle Show"},
  };
}





