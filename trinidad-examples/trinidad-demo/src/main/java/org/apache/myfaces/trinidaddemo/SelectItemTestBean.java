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

import javax.faces.model.SelectItem;
import java.util.ArrayList;
import java.util.HashMap;

public class SelectItemTestBean
{
  public SelectItemTestBean()
  {
    _oneItem = new SelectItem("foo", "Foo", "Description of foo", false);

    _itemList = new ArrayList<SelectItem>();
    _itemList.add(new SelectItem("foo", "Foo", "Description of foo", false));
    _itemList.add(new SelectItem("bar", "Bar", "Description of bar", false));
    _itemList.add(new SelectItem("baz", "Baz", "Description of baz", false));

    _itemArray = new SelectItem[3];
    _itemArray[0] = new SelectItem("foo", "Foo", "Description of foo", false);
    _itemArray[1] = new SelectItem("bar", "Bar", "Description of bar", false);
    _itemArray[2] = new SelectItem("baz", "Baz", "Description of baz", false);

    _itemMap = new HashMap<String, String>();
    _itemMap.put("Foo", "foo");
    _itemMap.put("Bar", "bar");
    _itemMap.put("Baz", "baz");
  }

  public SelectItem getOneItem()
  {
    return _oneItem;
  }

  public ArrayList<SelectItem> getItemList()
  {
    return _itemList;
  }

  public HashMap<String, String> getItemMap()
  {
    return _itemMap;
  }

  public SelectItem[] getItemArray()
  {
    return _itemArray;
  }

  private SelectItem              _oneItem;
  private ArrayList<SelectItem>   _itemList;
  private HashMap<String, String> _itemMap;
  private SelectItem[]            _itemArray;
}
