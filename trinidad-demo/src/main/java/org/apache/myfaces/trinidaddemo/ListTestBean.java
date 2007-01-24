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

import java.lang.reflect.Array;

import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.SelectItem;

public class ListTestBean implements java.io.Serializable
{
  public void valueChanged(ValueChangeEvent vce)
  {
    String oldValue = _toString(vce.getOldValue());
    Object newValue = _toString(vce.getNewValue());
    FacesContext context = FacesContext.getCurrentInstance();
    String message =
      "Value changed from " + oldValue + " to " + newValue;
    context.addMessage(vce.getComponent().getClientId(context),
                       new FacesMessage(message));
  }

  public Integer getSingleInt()
  {
    return _int;
  }

  public void setSingleInt(Integer val)
  {
    _int = val;
  }

  public int[] getIntArray()
  {
    return _intArray;
  }

  public void setIntArray(int[] val)
  {
    _intArray = val;
  }

  public String getSingleString()
  {
    return _string;
  }

  public void setSingleString(String val)
  {
    _string = val;
  }

  public String[] getStringArray()
  {
    return _stringArray;
  }

  public void setStringArray(String[] val)
  {
    _stringArray = val;
  }

  public List<Object> getObjectList()
  {
    return _objectList;
  }

  public void setObjectList(List<Object> val)
  {
    _objectList = val;
  }

  public List<String> getStringList()
  {
    return _stringList;
  }

  public void setSelectedSelectItems(List<?> val)
  {
    _selectedSelectItems = val;
  }

  public List<?> getSelectedSelectItems()
  {
    return _selectedSelectItems;
  }
  
  public void setSelectedCars(List<?> selectedCars)
  {
    _selectedCars = selectedCars;
  }

  public List<?> getSelectedCars()
  {
    return _selectedCars;
  }
    
  public void setSelectedCars2(List<?> selectedCars)
  {
    _selectedCars2 = selectedCars;
  }

  public List<?> getSelectedCars2()
  {
    return _selectedCars2;
  }

  public List<SelectItem> getMakes()
  {
    return _MAKE_ITEMS;
  }
  
  public SelectItem getFirstSelectItemString()
  {
    return _FIRST_CAR;
  }
  
  public SelectItem getSecondSelectItemString()
  {
    return _SECOND_CAR;
  }
  
  public SelectItem getThirdSelectItemString()
  {
    return _THIRD_CAR;
  }  
  
  public SelectItem getFirstSelectItemCar()
  {
    return _FIRST_SELECT_ITEM_CAR;
  }
  
  public SelectItem getSecondSelectItemCar()
  {
    return _SECOND_SELECT_ITEM_CAR;
  }
  
  public SelectItem getThirdSelectItemCar()
  {
    return _THIRD_SELECT_ITEM_CAR;
  }  
  
  static private String _toString(Object o)
  {
    if (o == null)
      return "null";

    if (o instanceof List)
    {
      String s = "List[";
      for (int i = 0; i < ((List) o).size(); i++)
      {
        if (i != 0)
          s += ",";

        s += _toString(((List) o).get(i));
      }

      return s + "]";
    }
    else if (o.getClass().isArray())
    {
      String s = "Array[";
      int size = Array.getLength(o);
      for (int i = 0; i < size; i++)
      {
        if (i != 0)
          s += ",";

        s += _toString(Array.get(o, i));
      }

      return s + "]";
    }

    if (o instanceof Car)
      return "\"" + ((Car)o).getName() + "\"";
      
    if (o instanceof String)
      return "\"" + o.toString() + "\"";

    return o.toString();
  }

  // this is used to test a Car Object instead of a String in SelectItem
  // when we use this, we also need to set "useIndexValue" attribute to true.
  static public class Car  implements java.io.Serializable
  {
    public Car(){}
    
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
  
  static private final List<SelectItem> _MAKE_ITEMS = new ArrayList<SelectItem>();
  
  static private final SelectItem _FIRST_CAR = 
    new SelectItem("cordera", "Cordera Ltd.");
  static private final SelectItem _SECOND_CAR = 
    new SelectItem("automno", "Autumno Inc.");
  static private final SelectItem _THIRD_CAR  = 
    new SelectItem("grabowski", "Grabowski Motors");

  static private final SelectItem _FIRST_SELECT_ITEM_CAR = 
    new SelectItem(new Car("cordera"), "Cordera Ltd.");
  static private final SelectItem _SECOND_SELECT_ITEM_CAR = 
    new SelectItem(new Car("automno"), "Autumno Inc.");
  static private final SelectItem _THIRD_SELECT_ITEM_CAR  = 
    new SelectItem(new Car("grabowski"), "Grabowski Motors"); 


  static
  {
    // test a Car Object instead of a String in SelectItem.
    // If you do not have a converter, 
    // make sure to use "useIndexValue='true'" in selectOne/selectMany
    // component.
    _MAKE_ITEMS.add(_FIRST_SELECT_ITEM_CAR);
    _MAKE_ITEMS.add(_SECOND_SELECT_ITEM_CAR);
    _MAKE_ITEMS.add(_THIRD_SELECT_ITEM_CAR);
  }   
  
  private List<?> _selectedCars;
  private List<?> _selectedCars2;
  private List<?> _selectedSelectItems;
    
  private Integer _int = 1;
  private int[] _intArray;

  private String _string;
  private String[] _stringArray;

  private List<String> _stringList;
  private List<Object> _objectList;
}
