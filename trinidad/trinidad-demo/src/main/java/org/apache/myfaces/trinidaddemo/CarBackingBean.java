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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UISelectItems;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidad.component.UIXOutput;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;

import org.apache.myfaces.trinidad.event.ReturnEvent;

public class CarBackingBean
{
  public List<SelectItem> getMakes()
  {
    return _MAKE_ITEMS;
  }
 
  public void makeChanged(ValueChangeEvent vce)
  {
       
    if ("".equals(vce.getNewValue()))
      _modelItems.setValue(Collections.EMPTY_LIST);
      
    Object newValue = vce.getNewValue();
    if (newValue instanceof Car)
    {
      String newCar = ((Car)newValue).getName();
      if ("cordera".equals(newCar))
        _modelItems.setValue(_CORDERA_MODELS);
      else if ("autumno".equals(newCar))
        _modelItems.setValue(_AUTUMNO_MODELS);
      else if ("grabowski".equals(newCar))
        _modelItems.setValue(_GRABOWSKI_MODELS);
    }
      
    _options.setValue(null);
    _price.setValue(null);
    _launchOptions.setDisabled(true);
  }

  public void modelChanged(ValueChangeEvent vce)
  {
    _price.setValue(_getPriceOfCar(vce.getNewValue()));
    _options.setValue(null);
    _launchOptions.setDisabled("".equals(vce.getNewValue()));
  }

  public UISelectItems getModelItems() { return _modelItems; } 
  public void setModelItems(UISelectItems modelItems) { _modelItems = modelItems; }

  public UIXOutput getPrice() { return _price; }
  public void setPrice(UIXOutput price) { _price = price; }

  public UIXOutput getOptions() { return _options; }
  public void setOptions(UIXOutput options) { _options = options; }

  public CoreCommandLink getLaunchOptions() { return _launchOptions; }
  public void setLaunchOptions(CoreCommandLink launchOptions) { _launchOptions = launchOptions; }

  public Object _getPriceOfCar(Object model)
  {
    return _PRICES.get(model);
  }

  @SuppressWarnings("unchecked")
  public void returnOptions(ReturnEvent event)
  {
    Object value = event.getReturnValue();
    StringBuffer buffer = new StringBuffer();
    int addedPrice = 0;
    if (value instanceof List)
    {
      for(CarOption option : (List<CarOption>) value)
      {
        addedPrice = addedPrice + option.getPrice();
        if (buffer.length() != 0)
          buffer.append(", ");
        buffer.append(option.getName());
      }
    }

    _options.setValue(buffer.toString());

    Integer price = (Integer) _price.getValue();
    _price.setValue(new Integer(price.intValue() + addedPrice));
  }

  private UIXOutput      _price;
  private UIXOutput      _options;
  private CoreCommandLink _launchOptions;
  private UISelectItems  _modelItems;

  static private final List<SelectItem> _MAKE_ITEMS       = new ArrayList<SelectItem>();
  static private final List<SelectItem> _CORDERA_MODELS   = new ArrayList<SelectItem>();
  static private final List<SelectItem> _AUTUMNO_MODELS   = new ArrayList<SelectItem>();
  static private final List<SelectItem> _GRABOWSKI_MODELS = new ArrayList<SelectItem>();

  static private final Map<String, Integer> _PRICES = new HashMap<String, Integer>();

  static private final SelectItem _NULL_SELECT_ITEM = new SelectItem("");

  static
  {
    _MAKE_ITEMS.add(_NULL_SELECT_ITEM);
    // test a Car Object instead of a String in SelectItem.
    // If you do not have a converter, 
    // make sure to use "useIndexValue='true'" in selectOne/selectMany
    // component.
    _MAKE_ITEMS.add(new SelectItem(new Car("cordera"), "Cordera Ltd."));
    _MAKE_ITEMS.add(new SelectItem(new Car("autumno"), "Autumno Inc."));
    _MAKE_ITEMS.add(new SelectItem(new Car("grabowski"), "Grabowski Motors"));

    _CORDERA_MODELS.add(_NULL_SELECT_ITEM);
    _CORDERA_MODELS.add(new SelectItem("XL", "XL"));
    _CORDERA_MODELS.add(new SelectItem("SV", "SV"));
    _CORDERA_MODELS.add(new SelectItem("DUH", "DUH"));

    _AUTUMNO_MODELS.add(_NULL_SELECT_ITEM);
    _AUTUMNO_MODELS.add(new SelectItem("390"));
    _AUTUMNO_MODELS.add(new SelectItem("490"));
    _AUTUMNO_MODELS.add(new SelectItem("585"));

    _GRABOWSKI_MODELS.add(_NULL_SELECT_ITEM);
    _GRABOWSKI_MODELS.add(new SelectItem("OneAndOnly"));
    
    _PRICES.put("XL", new Integer(25000));
    _PRICES.put("SV", new Integer(17000));
    _PRICES.put("DUH", new Integer(12000));
    _PRICES.put("390", new Integer(29500));
    _PRICES.put("490", new Integer(39500));
    _PRICES.put("585", new Integer(49500));
    _PRICES.put("OneAndOnly", new Integer(116500));
  }

  // this is used to test a Car Object instead of a String in SelectItem
  static private class Car
  {
    public Car (String name)
    {
      _name = name;
    }
    public String getName()
    {
      return _name;
    }

    private String _name;

  } 

}
