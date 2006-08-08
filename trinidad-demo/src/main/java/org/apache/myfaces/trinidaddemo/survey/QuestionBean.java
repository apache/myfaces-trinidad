/*
 * QuestionBean.java
 *
 * Created 4-Aug-06 4:31:10 PM
 * 
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.trinidaddemo.survey;

/**
 * @author Simon Lessard, Fujitsu Consulting
 */
public interface QuestionBean
{
  /**
   * returns a message describing the correct answer choices.
   *
   * @return a message describing the correct answer choices
   */
  public String getCorrectAnswerMessage();
  
  /**
   *  returns the question prompt.
   *
   *  @return the question prompt
   */
  public String getPrompt();
}
