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

import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;

/* 
 * This class is used in the demo's trinidad-skins.xml file when
 * binding the translation-source element. It tests the EL expression
 * pointing to a Map or a resourceBundle. Run the panelPageSkinDemo.jspx
 * page to see some of these translation values in action.
 */
public class SkinTranslationMapDemo
{
  /* Test a skin's translation-source EL pointing to a Map */
  public Map<String, String> getContents()
  {
    return _CONTENTS;
  }

  /* Test a skin's translation-source EL pointing to a ResourceBundle */
  public ResourceBundle getResourceBundle()
  {
    return new SkinBundle_fr();
  }

  static private final Map<String, String> _CONTENTS = new HashMap<String, String>();
  static 
  {
    _CONTENTS.put("af_inputDate.LAUNCH_PICKER_TIP", "Launch PickerMap");
    _CONTENTS.put("af_showDetail.DISCLOSED_TIP", "Hide Tip Map");
    _CONTENTS.put("af_showDetail.DISCLOSED", "Hide Map");

  }

}




