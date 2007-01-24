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
import java.util.List;

/***
 *  class SurveyBean
 *
 * This is the main backing bean for the Survey Demo project.
 *  It represents a multiple-choice survey.  It contains state about
 *  the list of questions to ask in the survey and the user's responses to
 *  these questions.  It also has methods that help with application
 *  navigation.
 *
 * ***/

public class SurveyBean extends Object implements Serializable
{

  /** A List of QuestionBean objects */
  private ArrayList<QuestionBean> _questions = new ArrayList<QuestionBean>();

  /** The index of the (current) question displayed */
  private int               _currentIndex;

  /** A List of QuestionBean objects */
  private ArrayList<String> _userAnswers = new ArrayList<String>();

  /** number of choices */
  static final int NUMBER_OF_ANSWER_CHOICES = 4;

  /** debug flag */
  private boolean     debug = false;

  /** beans for each question in the survey */
  MultChoiceQuestionBean _q0;

  TextQuestionBean _q1; // insert a text field question here

  MultChoiceQuestionBean _q2;

  CheckboxQuestionBean _q3; // this is a checkbox question

  MultChoiceQuestionBean _q4;

  /** a String for each user response in the survey */
  String _a0;   //response for a multiple choice (normal) question
  String _a1;   //response for TextQuestion
  String _a2;   //response for a multiple choice (normal) question

  String _a3;   //special case: responses for CheckboxQuestion
  boolean _a30;   // these booleans represent the user's choices for the
  boolean _a31;   // checkbox question
  boolean _a32;
  boolean _a33;

  String _a4;   //response for a multiple choice (normal) question

  /** A List of SurveyPage objects for use with the processTrain component */
  private List<SurveyPage> _pages;
  /**
   *  Class constructor (no arguments).
   */
  public SurveyBean()
  {
    init("survey.bundles.california");
  }

  /**
   *  Class constructor.
   *
   *  @param  bundleName  the path of a bundle file
   */
  public SurveyBean(String bundleName)
  {
    init(bundleName);
  }

  /**
   *  Initializes survey application.
   *
   *  @param  bundleName  the path of a bundle file
   */
  private void init(String bundleName)
  {
    //TODO
    /*FacesContext context = FacesContext.getCurrentInstance();
     *   ResourceBundle data = null;
     *data = ResourceBundle.getBundle(bundleName);
     */

    // load in the questions
    loadQuestions();

    // initialize other state for each survey session
    initSurveyState();
  }

    /**
     *  Loads the questions into survey application.
     */
  private void loadQuestions()
  {
    // notice there are redundant data structures
    // the QuestionBeans are both stored in instance fields, which are linked
    // together in an Array List

    // question 0, for surveyPage1
    ArrayList<String> choices = new ArrayList<String>();
    choices.add("A. Loading zone for freight or passengers.");
    choices.add("B. Loading zone for passengers or mail only.");
    choices.add("C. Loading zone for freight only.");
    choices.add("D. They ran out of red paint.");
    _q0 = new MultChoiceQuestionBean("A white painted curb means ", choices, 1);
    _questions.add(_q0);

    // question 1, for surveyPage2
    _q1 = new TextQuestionBean("The color of a typical stop sign is ", "RED");
    _questions.add(_q1);

    // question 2, for surveyPage3
    ArrayList<String> choices2 = new ArrayList<String>();
    choices2.add("A. If the shoulder is wide enough to accommodate your vehicle.");
    choices2.add("B. If the vehicle ahead of you is turning left.");
    choices2.add("C. Under no circumstances.");
    choices2.add("D. If you are driving a Hummer.");
    _q2 = new MultChoiceQuestionBean("You may drive off of the paved roadway to pass another vehicle ", choices2, 2);
    _questions.add(_q2);

    // question 3, for surveyPage4
    ArrayList<String> choices3 = new ArrayList<String>();
    choices3.add("A. In a crosswalk.");
    choices3.add("B. Within 10 feet of a fire hydrant.");
    choices3.add("C. Next to a red painted curb.");
    choices3.add("D. If you are driving a Hummer.");
    _q3 = new CheckboxQuestionBean("It is illegal to park your vehicle (check all that apply) ", choices3, 14, true, true, true, false);
    _questions.add(_q3);

    // question 4, for surveyPage5
    ArrayList<String> choices4 = new ArrayList<String>();
    choices4.add("A. Stop, then proceed when you think all of the children have exited the bus.");
    choices4.add("B. Slow to 25 MPH and pass cautiously.");
    choices4.add("C. Stop as long as the red lights are flashing. ");
    choices4.add("D. Grab your sack lunch and jump onboard so you're not late for homeroom like yesterday.");
    _q4 = new MultChoiceQuestionBean("A school bus ahead of you in your lane is stopped with red lights flashing. You should ", choices4, 2);
    _questions.add(_q4);

    if (debug)
    {
      System.out.println("init() printing out the list of questions:\n" + _questions);
      System.out.println("length of list of questions: " + _questions.size());
    }

  }  //end loadQuestions()

  /**
   *  Initializes various survey state.  Should be called for each new sesion
   *  through the survey.
   */
  public void initSurveyState()
  {
    /* start with the first(0th) question */
    _currentIndex = 0;

    /* holders for user responses are initialized to "no selection" */
    _a0 = null;
    _a1 = "";   //response for a TextQuestion
    _a2 = null;

    _a3 = "0"; //response for a CheckboxQuestion
    _a30 = false;
    _a31 = false;
    _a32 = false;
    _a33 = false;

    _a4 = null;

    /* add initialized responses into array */
    _userAnswers.add(_a0);
    _userAnswers.add(_a1);
    _userAnswers.add(_a2);
    _userAnswers.add(_a3);
    _userAnswers.add(_a4);

  } //end initSurveyState()



  /*** navigation methods ***/

  /**
   * advances the survey to the next question.
   *
   * @return a String object, "next"
   */
  public String next()
  {
    advanceToNextQuestion();
    if (debug)
    {
      System.out.println("the currentIndex is:" + _currentIndex);
      System.out.println("next() is finished");
    }

    return "next";
  }
  /**
   * moves the survey to the previous question.
   *
   * @return a String object, "back"
   */
  public String back()
  {
    goToPreviousQuestion();
    if (debug)
    {
      System.out.println("the currentIndex is:" + _currentIndex);
      System.out.println("next() is finished");
    }
    return "back";
  }

  /**
   * called when the first run through the survey is completed.
   *
   * @return a String object, "finish"
   */
 public String finish()
  {
    return "finish";
  }

  /**
   * called to advance to the results page.
   *
   * @return a String object, "check"
   */
  public String check()
  {
    return "check";
  }

  /**
   * called to go back to beginning of survey and clears all previous responses.
   *
   * @return a String object, "start"
   */
  public String start()
  {
    initSurveyState();
    return "start";
  }


  /*** Accessors ***/

  /**
   * advances the survey to the next question.
   *
   * @return  a String object representing the number of questions in the survey
   */
  public String getNumQuestions()
  {
    return ( String.valueOf(_questions.size()) );
  }

  /**
   * determines whether we have reached the end of the survey
   *
   * @return  a boolean that is true if we have reached the end of the survey
   */
  public boolean getDone()
  {
    // when we've reached the last question in the list
    return ( _currentIndex == _questions.size()-1 );
  }

  /**
   * determines the number of the current question in the survey.
   *
   * @return  a String that represents the number of the current question
   */
  public String getCurrentQuestionNumber()
  {
    // the question number is index+1 since the index starts at 0
    int i = _currentIndex + 1 ;
    return String.valueOf(i);
  }

  /*** Utils ***/


  /**
   * increments question index to next question.
   */
  public void advanceToNextQuestion()
  {
    _currentIndex++;
  }

  /**
   * decrements question index to previous question.
   */
  public void goToPreviousQuestion()
  {
    _currentIndex--;
  }

  /**
   * a hack to return the String "error" inside a binding using EL
   *
   * @return  the literal String, "error"
   */
  public String getError()
  {
    return "error";
  }

  /**
   * a hack to return the String "none" inside a binding using EL
   *
   * @return  the literal String, "none"
   */
  public String getNone()
    // a hack to return a String inside a binding using EL
  {
    return "none";
  }

  /**
   * a hack to return the empty String "" inside a binding using EL
   *
   * @return  the empty String, ""
   */
  public String getEmptyString()
    // a hack to return a String inside a binding using EL
  {
    return "";
  }

 /**
   * a hack to return the String "correct" inside a binding using EL
   *
   * @return  the literal String, "correct"
   */
  public String getCorrect()
    // a hack to return a String inside a binding using EL
  {
    return "correct";
  }


  /**
   * a hack to return the String "incorrect" inside a binding using EL
   *
   * @return  the literal String, "incorrect"
   */
  public String getIncorrect()
    // a hack to return a String inside a binding using EL
  {
    return "incorrect";
  }


  /**
   * stores whether a checkbox is checked.  This app uses a simple bit mapping
   * to represent the "checked" state of the checkboxes.  For example, assume the
   * number of checkboxes is 4. If all four checkboxes were checked, then
   * the representation is "1111", which is equivalent to the number 15 in
   * base 10.  Likewise, if only the first and third checkboxes are selected,
   * then the representation is "1010", which is the number 10 in base 10.
   * This method turns the bits on/off according to whether the "index"-th
   * checkbox is "checked".
   *
   * @param checked             whether this checkbox is checked
   * @param index               the index of the checkbox
   * @param storageDestination  the String of the integer representation of the selection state
   *
   * @return  a String that represents the current selections (an integer)
   */
  private String storeCheckSelection (boolean checked, int index,
                                      String storageDestination)
  {
    // extract an integer from the string
    int storageInt = Integer.parseInt(storageDestination);
    if (checked)
    {
      // if checked, then turn on index-th bit
      storageInt = storageInt | 1<<(NUMBER_OF_ANSWER_CHOICES-index-1);
    }
    else
    {
      // if not checked, then turn off index-th bit
      storageInt = storageInt & ~(1<<(NUMBER_OF_ANSWER_CHOICES-index-1));
    }

   if (debug)
   {
    System.out.println("checked is: " + checked + ", index is: " + index + ", storageInt is:" + storageInt);
   }

   // store the integer as a string
   return String.valueOf(storageInt);

  } // end storeCheckSelection


 /*** IDE generated accessors ***/

  public MultChoiceQuestionBean getQ0()
  {
    return _q0;
  }

  public TextQuestionBean getQ1()
  {
    return _q1;
  }

  public MultChoiceQuestionBean getQ2()
  {
    return _q2;
  }

  public CheckboxQuestionBean getQ3()
  {
    return _q3;
  }

  public MultChoiceQuestionBean getQ4()
  {
    return _q4;
  }


  public void setA0(String a0)
  {
    _a0 = a0;
  }


  public String getA0()
  {
    return _a0;
  }


  public void setA1(String a1)
  {
  // special set method to trim and convert response from text input
    _a1 = a1.trim().toUpperCase();
  }


  public String getA1()
  {
    return _a1;
  }


  public void setA2(String a2)
  {
    _a2 = a2;
  }


  public String getA2()
  {
    return _a2;
  }


  public void setA3(String a3)
  {
    _a3 = a3;
  }


  public String getA3()
  {
    return _a3;
  }


  public void setA4(String a4)
  {
    _a4 = a4;
  }


  public String getA4()
  {
    return _a4;
  }


  public void setA30(boolean a30)
  {
    _a30 = a30;
    _a3 = storeCheckSelection(a30, 0, _a3);

  }


  public boolean getA30()
  {
    return _a30;
  }


  public void setA31(boolean a31)
  {
    _a31 = a31;
    _a3 = storeCheckSelection(a31, 1, _a3);
  }


  public boolean getA31()
  {
    return _a31;
  }


  public void setA32(boolean a32)
  {
    _a32 = a32;
    _a3 = storeCheckSelection(a32, 2, _a3);
  }


  public boolean getA32()
  {
    return _a32;
  }


  public void setA33(boolean a33)
  {
    _a33 = a33;
    _a3 = storeCheckSelection(a33, 3, _a3);
  }


  public boolean getA33()
  {
    return _a33;
  }

  public List<SurveyPage> getPages()
  {
    if (_pages == null)
    {
      _pages = new ArrayList<SurveyPage>();
      _pages.add(new SurveyPage("/surveydemo/surveyPage1.jspx", "Step1"));
      _pages.add(new SurveyPage("/surveydemo/surveyPage2.jspx", "Step2"));
      _pages.add(new SurveyPage("/surveydemo/surveyPage3.jspx", "Step3"));
      _pages.add(new SurveyPage("/surveydemo/surveyPage4.jspx", "Step4"));
      _pages.add(new SurveyPage("/surveydemo/surveyPage5.jspx", "Step5"));
    }
    return _pages;
  }

} //end SurveyBean class
