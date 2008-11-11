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

function passwordValidate(value)
{
  if (!value)
    return null;
    
  if (value == '******')
    return null;
    
  var hasNumber = false;
  
  for (var i = 0; i < value.length; i++)
  {
    var subValue = value.substring(i, i+1);
    
    if (!isNaN(parseInt(subValue)))
    {
      hasNumber = true;
      break;
    }
  }
  
  if (hasNumber == false)
  {
    var facesMessage = new TrFacesMessage(
                        this._messages[PasswordValidator.NUMBER_SUMMARY],
                        this._messages[PasswordValidator.NUMBER_DETAIL],
                        TrFacesMessage.SEVERITY_ERROR)
    throw new TrValidatorException(facesMessage);
  }
    
  return null;
}

function PasswordValidator(messages)
  {this._messages = messages;}
PasswordValidator.prototype = new TrValidator();
PasswordValidator.prototype.validate = passwordValidate;
PasswordValidator.NUMBER_DETAIL = 'ND'; 
PasswordValidator.NUMBER_SUMMARY = 'NS'; 
