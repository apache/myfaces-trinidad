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

import javax.faces.webapp.ValidatorTag;
import javax.faces.validator.Validator;
import javax.servlet.jsp.JspException;

public class AnswerValidatorTag extends ValidatorTag
{

  private String _questionIndex = "";
  private final String ID = "survey answer validator";

  public AnswerValidatorTag()
  {
      super();
      super.setValidatorId(ID);
  }

  public void setQuestionIndex(String index)
  {
    _questionIndex = index;
  }

  public String getQuestionIndex()
  {
    return _questionIndex;
  }

  @Override
  protected Validator createValidator() throws JspException {

      AnswerValidator validator = (AnswerValidator)super.createValidator();
      validator.setQuestionIndex(_questionIndex);

      //System.out.println("just instantiated " + validator + " with: " + validator.getQuestionIndex());
      return validator;

  }


} // end AnswerValidatorTag
