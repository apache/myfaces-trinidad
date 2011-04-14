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

import java.util.Arrays;
import java.util.List;

import javax.faces.event.ActionEvent;


public class SubformBean
{

  public SubformBean()
  {

  }

  public List<MyItem> getTexts2()
  {
    return sItems2;
  }

  public List<MyItem> getTexts()
  {
    return sItems;
  }


  public void setSubformValue1(String _subformValue1)
  {
    this._subformValue1 = _subformValue1;
  }

  public String getSubformValue1()
  {
    return _subformValue1;
  }

  public void setSubformValue2(String _subformValue2)
  {
    this._subformValue2 = _subformValue2;
  }

  public String getSubformValue2()
  {
    return _subformValue2;
  }

  public void setSubformValue3(String _subformValue3)
  {
    this._subformValue3 = _subformValue3;
  }

  public String getSubformValue3()
  {
    return _subformValue3;
  }

  public void setSubformValue4(String _subformValue4)
  {
    this._subformValue4 = _subformValue4;
  }

  public String getSubformValue4()
  {
    return _subformValue4;
  }


  private String _subformValue1;
  private String _subformValue2;
  private String _subformValue3;
  private String _subformValue4;

  private static final List<MyItem> sItems =
    Arrays.asList(new MyItem(null), new MyItem(null), new MyItem(null));

  private static final List<MyItem> sItems2 =
    Arrays.asList(new MyItem(null), new MyItem(null), new MyItem(null));


  public static class MyItem
  {
    public MyItem(String text)
    {
      mText = text;
    }

    public String getText()
    {
      return mText;
    }

    public void setText(String text)
    {
      System.out.println("setter called with " + text);
      mText = text;
    }

    public void doSomething(ActionEvent event)
    {
      System.out.println("in doSomething(), value is" + mText);
    }

    private String mText;
  }
}
