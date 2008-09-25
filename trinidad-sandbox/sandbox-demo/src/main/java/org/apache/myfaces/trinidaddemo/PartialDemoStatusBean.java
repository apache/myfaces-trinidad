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

import java.text.SimpleDateFormat;

import java.util.Date;

public class PartialDemoStatusBean implements java.io.Serializable
{
  public PartialDemoStatusBean()
  {
    reset();
  }

  public boolean getChecked()
  {
    return Boolean.TRUE.equals(_checkBoxState);
  }

  public int getCheckBoxUpdateCount()
  {
    return _checkBoxUpdateCount;
  }

  public String getCheckBoxStateText()
  {
    if (_checkBoxState == null)
      return _DEFAULT_CHECK_STATE;

    if (Boolean.TRUE.equals(_checkBoxState))
      return _CHECKED_STATE;

    return _NOT_CHECKED_STATE;
  }

  public String getChoiceInt()
  {
    if (_choiceInt == null)
      return "1";
    return _choiceInt.toString();
  }

  public String getChoiceText()
  {
    if (_choiceInt == null)
      return _DEFAULT_CHOICE_TEXT;

    return "value #" + _choiceInt;
  }

  public String getLinkUpdate()
  {
    return _linkUpdate;
  }

  public String getRadioStateText()
  {
    return _radioState;
  }

  public String getTextStateText()
  {
    if (_DEFAULT_TEXT_VALUE.equals(_textValue))
      return _DEFAULT_TEXT_STATE;
    return _textValue;
  }

  public String getTextValue()
  {
    return _textValue;
  }

  public void setChecked(boolean checked)
  {
    _checkBoxState = (checked ? Boolean.TRUE : Boolean.FALSE);
  }

  public void setChecked(Boolean checked)
  {
    _checkBoxState = checked;
  }

  public void setChoiceText(String txt)
  {
    // does nothing
  }

  public void setChoiceInt(String ci)
  {
    _choiceInt = new Integer(ci);
  }

  public void setLinkUpdate()
  {
    SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
    _linkUpdate = sdf.format(new Date());
  }

  public void setRadioStateText(String t)
  {
    _radioState = t;
  }

  void setSelectBooleanState(String value)
  {
    _radioState = "selectBoolean set, " + value;
  }

  void setSelectOneState(String value)
  {
    _radioState = "selectOne set, item " + value;
  }

  public void setTextValue(String t)
  {
    _textValue = t;
  }

  public void resetCheckBox()
  {
    _checkBoxUpdateCount = 0;
    _checkBoxState = null;
  }

  public void incrementCheckBoxUpdateCount()
  {
    _checkBoxUpdateCount++;
  }

  public void reset()
  {
    resetCheckBox();
    _choiceInt = null;
    _linkUpdate = _DEFAULT_LINK_UPDATE;
    _radioState = _DEFAULT_RADIO_STATE;
    _textValue = _DEFAULT_TEXT_VALUE;
  }

  private int     _checkBoxUpdateCount;
  // This is kept as a Boolean so we can reset to the default value.
  private Boolean _checkBoxState;
  private Integer _choiceInt;
  private String  _linkUpdate;
  private String  _radioState;
  private String  _textValue;

  private static String _NOTHING              = "nothing yet.";
  private static String _DEFAULT_CHECK_STATE  = "updates this text.";
  private static String _CHECKED_STATE        = "is checked.";
  private static String _NOT_CHECKED_STATE    = "is not checked.";
  private static String _DEFAULT_CHOICE_TEXT  = _NOTHING;
  private static String _DEFAULT_LINK_UPDATE  = "never.";
  private static String _DEFAULT_RADIO_STATE  = "no selection yet.";
  private static String _DEFAULT_TEXT_STATE   = _NOTHING;
  private static String _DEFAULT_TEXT_VALUE   = "Change this text";
}
