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

import java.awt.Color;

import java.util.ArrayList;
import java.util.List;

/**
 * Managed bean for color component demos.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/ColorBean.java#1 $) $Date: 16-aug-2005.15:12:27 $
 */
public class ColorBean implements java.io.Serializable
{
  public Color getColorValue1()
  {
    return _colorValue1;
  }

  public void setColorValue1(Color colorValue)
  {
    _colorValue1 = colorValue;
  }

  public Color getColorValue2()
  {
    return _colorValue2;
  }

  public void setColorValue2(Color colorValue)
  {
    _colorValue2 = colorValue;
  }

  public Color getColorValue3()
  {
    return _colorValue3;
  }

  public void setColorValue3(Color colorValue)
  {
    _colorValue3 = colorValue;
  }

  public Color getColorValue4()
  {
    return _colorValue4;
  }

  public void setColorValue4(Color colorValue)
  {
    _colorValue4 = colorValue;
  }

  public Color[] getColorArray()
  {
    Color[] colorArray = new Color[4];
    colorArray[0] = _colorValue1;
    colorArray[1] = _colorValue2;
    colorArray[2] = _colorValue3;
    colorArray[3] = _colorValue4;
    return colorArray;
  }

  public List<Color> getColorList()
  {
    List<Color> colorList = new ArrayList<Color>();
    colorList.add(_colorValue3);
    colorList.add(_colorValue1);
    colorList.add(_colorValue4);
    colorList.add(_colorValue2);
    return colorList;
  }

  private Color _colorValue1 = new Color(255, 0, 0);
  private Color _colorValue2 = new Color(0, 255, 0);
  private Color _colorValue3 = new Color(0, 0, 255);
  private Color _colorValue4 = new Color(255, 255, 0);

}
