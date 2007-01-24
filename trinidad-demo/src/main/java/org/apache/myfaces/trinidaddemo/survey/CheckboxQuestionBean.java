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
import java.util.ArrayList;

/***
 *  class CheckboxQuestionBean
 *
 * This bean represents a single multiple-answer checkbox style question.
 * It has fields for the question prompt, a list of choices, and all the
 * correct checkboxes in the answer.
 *
 * [Note: Due to a lack of support for dynamically iterating through data in
 * early releases of Trinidad, this bean contains special get methods that help
 * iterate through the list of choices for printing out on the screen.]
 *
 * ***/

public class CheckboxQuestionBean implements QuestionBean, Serializable
{

 /** The question prompt as a String object */
 private String   _prompt;

 /** arbitrary number of possible answer choices (Strings) */
 private ArrayList<String>  _choices;


  /** An integer that represents the correct checkbox choices */
  private int   _correctAnswer;

  /** the booleans to represent the correct choices */
  private boolean   _checkbox0;
  private boolean   _checkbox1;
  private boolean   _checkbox2;
  private boolean   _checkbox3;

  // a debug flag
  private boolean debug = false;



  /**
    *  Class constructor (no arguments).
    */
  public CheckboxQuestionBean()
  {
  }

    /**
     *  Class constructor.
     */
    public CheckboxQuestionBean(
        String prompt,
        ArrayList<String> choices,
        int correctAnswer,
        boolean checkbox0,
        boolean checkbox1,
        boolean checkbox2,
        boolean checkbox3)
    {
      _prompt = prompt;
      _choices = choices;
      _correctAnswer = correctAnswer;

      _checkbox0 = checkbox0;
      _checkbox1 = checkbox1;
      _checkbox2 = checkbox2;
      _checkbox3 = checkbox3;
    }


  /*** Accessors ***/

 /**
   *  returns the question prompt.
   *
   *  @return the question prompt
   */
 public String getPrompt()
 {
    // after getting the prompt, we want to initialize the iterator index
    // for the answers
  return _prompt;
 }


  public String getText1()
  {
    return _choices.get(0);
  }
  
  public String getText2()
  {
    return _choices.get(1);
  }
  
  public String getText3()
  {
    return _choices.get(2);
  }
  
  public String getText4()
  {
    return _choices.get(3);
  }

  /*** util functions ***/

 
  /**
   * typical toString method
   *
   * @return a String representation of a QuestionBean
   */
  @Override
  public String toString()
  {
    String str = _prompt + "; " + _choices.toString();
    return str;
  }


  /**
   * returns a message describing the correct answer choices.
   *
   * @return a message describing the correct answer choices
   */
 public String getCorrectAnswerMessage()
 {
    String message = "The correct answer is: ";
    String bitMap = Integer.toBinaryString(_correctAnswer);
    int i,j;
    boolean atLeastOneSelected = false;

    // since toBinaryString() library method does not put in leading zeros,
    // we need to make sure bitMap is as long as NUMBER_OF_ANSWER_CHOICES
    while (bitMap.length() < SurveyBean.NUMBER_OF_ANSWER_CHOICES)
    {
      // prepend leading zeros
      bitMap = '0' + bitMap;
    }

    // beginning with the NUMBER_OF_ANSWER_CHOICES-th bit from the right
    // check until the end of the string
    for (i=bitMap.length()-SurveyBean.NUMBER_OF_ANSWER_CHOICES, j=0;
          i<bitMap.length() && j<_choices.size();
          i++, j++)
    {
      // if the considered bit is 1
      if (debug)
      {
        System.out.println("in cbqb, bitMap is: " + bitMap + ", getting " + i + "th char, which is: " + bitMap.charAt(i));
      }
      if( bitMap.charAt(i) == '1')
      {
         // it's a correct solution
         message = message + _choices.get(j) + " & ";
         atLeastOneSelected = true;
      }
    } //end for loop

    if (atLeastOneSelected)
    {
      // remove extra " & " from end of message
      message = message.substring(0, message.length()-3);
    }

    return message;
 }


  /**
   * Returns a message describing the correct answer for a particular checkbox.
   *
   * @param   checkboxNum the index of the checkbox
   *
   * @return a message describing the correct answer choices
   */
 public String getCorrectAnswerMessage(int checkboxNum)
 {
    String message = "INCORRECT";
//    String message = "The correct answer is: ";
//
//    switch (checkboxNum)
//    {
//      case(0):
//        message = message + getCheckbox0();
//        break;
//      case(1):
//        message = message + getCheckbox1();
//        break;
//      case(2):
//        message = message + getCheckbox2();
//        break;
//      case(3):
//        message = message + getCheckbox3();
//        break;
//    }

    return message;
 }

/*** IDE generated accessors ***/

  public boolean getCheckbox0()
  {
    return _checkbox0;
  }


  public boolean getCheckbox1()
  {
    return _checkbox1;
  }


  public boolean getCheckbox2()
  {
    return _checkbox2;
  }


  public boolean getCheckbox3()
  {
    return _checkbox3;
  }


  public int getCorrectAnswer()
  {
    return _correctAnswer;
  }

}
