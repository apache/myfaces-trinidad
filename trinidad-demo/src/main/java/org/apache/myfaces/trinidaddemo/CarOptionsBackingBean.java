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
package org.apache.myfaces.trinidaddemo;

import java.util.ArrayList;
import java.util.List;

import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.context.RequestContext;


public class CarOptionsBackingBean
{
  public List<String> getOptions() { return _options; }
  public void setOptions(List<String> options) { _options = options; }

  public String pickOptions()
  {
    List<String> options = getOptions();
    List<CarOption> realOptionObjects = new ArrayList<CarOption>();
    for (int i = 0; i < _AVAILABLE_OPTIONS.length; i++)
    {
      if (options.contains("" + i))
        realOptionObjects.add(_AVAILABLE_OPTIONS[i]);
    }

    RequestContext.getCurrentInstance().returnFromDialog(realOptionObjects,
                                                          null);
    return null;
  }

  public List<SelectItem> getOptionsItems()
  {
    return _OPTIONS_ITEMS;
  }

  private List<String> _options;

  static private final List<SelectItem> _OPTIONS_ITEMS = new ArrayList<SelectItem>();

  static private final CarOption[] _AVAILABLE_OPTIONS =
  {
    new CarOption("Power windows", 350),
    new CarOption("Automatic transmission", 990),
    new CarOption("Side-curtain air bags", 600)
  };

  static
  {
    _OPTIONS_ITEMS.add(new SelectItem("0",
                                      "Power windows ($350)"));
    _OPTIONS_ITEMS.add(new SelectItem("1",
                                      "Automatic transmission ($990)"));
    _OPTIONS_ITEMS.add(new SelectItem("2",
                                      "Side-curtain air-bags ($600)"));

  }
}
