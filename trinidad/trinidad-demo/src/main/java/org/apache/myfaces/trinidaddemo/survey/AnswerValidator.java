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

import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.application.FacesMessage;
import javax.faces.validator.Validator;

public class AnswerValidator implements Validator, StateHolder
{

  public void setQuestionIndex(String index) 
  {
    _questionIndex = index;
  }

  public String getQuestionIndex()
  {
    return _questionIndex;
  }

  public void validate(FacesContext context, UIComponent component, Object value) {

   String userResponse = (String.valueOf(value)).toUpperCase();
   String correctResponse = lookupAnswer().toUpperCase();

   if(!userResponse.equals(correctResponse))
   {
        FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                                            "Incorrect Response",
                                            lookupCorrectAnswerMsg());

        context.addMessage(component.getClientId(context), msg);
   }
   else
   {
      FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_INFO,
                                            "Correct Response",
                                            "CORRECT!");
        context.addMessage(component.getClientId(context), msg);
   }
 } // end validate()

 // -------------------------------------------------------- Private Methods
 private String lookupCorrectAnswerMsg()
  {
    // based on the current question index
    // get the answer message from the appropriate question bean

    if (_questionIndex.equals("0"))
    {
      return _surveyBean.getQ0().getCorrectAnswerMessage();
    }
    else if (_questionIndex.equals("1"))
    {
      return _surveyBean.getQ1().getCorrectAnswerMessage();
    }
    else if (_questionIndex.equals("2"))
    {
      return _surveyBean.getQ2().getCorrectAnswerMessage();
    }
    else if (_questionIndex.equals("3A"))
    {
      return String.valueOf(_surveyBean.getQ3().getCorrectAnswerMessage(0));
    }
    else if (_questionIndex.equals("3B"))
    {
      return String.valueOf(_surveyBean.getQ3().getCorrectAnswerMessage(1));
    }
    else if (_questionIndex.equals("3C"))
    {
      return String.valueOf(_surveyBean.getQ3().getCorrectAnswerMessage(2));
    }
    else if (_questionIndex.equals("3D"))
    {
      return String.valueOf(_surveyBean.getQ3().getCorrectAnswerMessage(3));
    }
    else if (_questionIndex.equals("4"))
    {
      return _surveyBean.getQ4().getCorrectAnswerMessage();
    }
    else
    {
      return ""; // error: passed in incorrect questionindex
    }
  } // end getCorrectAnswerMsg()

  private String lookupAnswer()
  {

    // based on the current question index
    // get the answer string from the appropriate question bean

    if (_questionIndex.equals("0"))
    {
      return String.valueOf(_surveyBean.getQ0().getCorrectIndex());
    }
    else if (_questionIndex.equals("1"))
    {
      return _surveyBean.getQ1().getCorrectAnswer();
    }
    else if (_questionIndex.equals("2"))
    {
      return String.valueOf(_surveyBean.getQ2().getCorrectIndex());
    }
    else if (_questionIndex.equals("3A"))
    {
      return String.valueOf(_surveyBean.getQ3().getCheckbox0());
    }
    else if (_questionIndex.equals("3B"))
    {
      return String.valueOf(_surveyBean.getQ3().getCheckbox1());
    }
    else if (_questionIndex.equals("3C"))
    {
      return String.valueOf(_surveyBean.getQ3().getCheckbox2());
    }
    else if (_questionIndex.equals("3D"))
    {
      return String.valueOf(_surveyBean.getQ3().getCheckbox3());
    }
    else if (_questionIndex.equals("4"))
    {
      return String.valueOf(_surveyBean.getQ4().getCorrectIndex());
    }
    else
    {
      return ""; // error: passed in incorrect questionindex
    }
  } // end loookupAnswer()


  //-------------------------------------------------  StateHolder Methods

  public Object saveState(FacesContext context)
  {
    Object values[] = new Object[1];
    values[0] = _questionIndex;
    return (values);
  }


  public void restoreState(FacesContext context, Object state)
  {
    Object values[] = (Object[]) state;
    _questionIndex = (String) values[0];
  }

   public boolean isTransient()
  {
    return (_transientValue);
  }

  public void setTransient(boolean transientValue)
  {
    _transientValue = transientValue;
  }

  

  private String      _questionIndex =  "";
  private SurveyBean  _surveyBean = new SurveyBean();
  private boolean     _transientValue = false;
  
} // end class AnswerValidator
