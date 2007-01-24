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
package org.apache.myfaces.trinidaddemo.survey;

import java.io.Serializable;

/***
 *  class TextQuestionBean
 *
 * This bean represents a single question with a text field as its answer form.
 * It has fields for the question prompt and the correct answer.
 *
 * ***/


public class TextQuestionBean implements QuestionBean, Serializable
{
  /** The question as a String object. */
  private String    _prompt;

  /** The correct answer. */
  private String    _correctAnswer;

 /**
  *  Class constructor (no arguments).
  */
  public TextQuestionBean()
  {
    _prompt = "";
    _correctAnswer = "";
  }

 /**
  *  Class constructor.
  */
  public TextQuestionBean(String prompt, String correctAnswer)
  {
    _prompt = prompt;
    _correctAnswer = correctAnswer;
  }


  /*** JDev generated accessors ***/

  public void setPrompt(String prompt)
  {
    _prompt = prompt;
  }

  public String getPrompt()
  {
    return _prompt;
  }

  public String getCorrectAnswer()
  {
    return _correctAnswer;
  }

  public String getCorrectAnswerMessage()
  {
    return "The correct answer is: " + _correctAnswer;
  }


}
