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
import java.util.List;

/***
 *  class MultChoiceQuestionBean
 *
 * This bean represents a single multiple choice questions.  It has fields for
 * the question prompt, a list of choices, and the correct answer.
 *
 ***/


public class MultChoiceQuestionBean implements QuestionBean, Serializable
{
  
  /** Constructors **/
  
  /**
   *  Class constructor (no arguments).
   */
  public MultChoiceQuestionBean()
  {
  }
  
  /**
   *  Class constructor.
   */
  public MultChoiceQuestionBean(String prompt,
                                List<String> choices,
                                int correctIndex)
  {
    _prompt = prompt;
    _choices = choices;
    _correctIndex = correctIndex;
  }


  /*** Accessors ***/
  /**
   *  returns the question prompt.
   *
   *  @return the question prompt
   */
  public String getPrompt()
  {
    return _prompt;
  }


  /**
    *  returns the list of answer strings.
    */
  public List<String> getAnswerStrings()
  {
    return _choices;
  }


  /**
   * typical toString method
   *
   * @return a String representation of a MultChoiceQuestionBean
   */
  @Override
  public String toString()
  {
    String str = "[" + _prompt + "; " + _choices.toString() + "]";
    return str;
  }



  /**
   * returns a message describing the correct answer choices.
   *
   * @return a message describing the correct answer choices
   */
  public String getCorrectAnswerMessage()
  {
    return "The correct answer is: " + _choices.get(_correctIndex);
  }

  /**
   * returns the index of the correct answer
   *
   * @return the index of the correct answer
   */
  public int getCorrectIndex()
  {
    return _correctIndex;
  }

  /* The question as a String object */
  private String   _prompt;
  
  /* arbitrary number of possible answer choices (Strings) */
  private List<String>  _choices;

  /* The index of the correct answer */
  private int     _correctIndex;


} //end QuestionBean
