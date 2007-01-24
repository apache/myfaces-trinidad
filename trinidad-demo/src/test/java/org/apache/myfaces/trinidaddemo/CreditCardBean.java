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

public class CreditCardBean
{

  public CreditCardBean()
  {

  }


  public String getNumber()
  {
    return _number ;
  }

  public void setNumber(String number)
  {
    _number = number;
  }

  public String getExpirationDate()
  {
    return _expirationDate ;
  }

  public void setExpirationDate(String expirationDate)
  {
    _expirationDate = expirationDate;
  }


  public String getFirstName()
  {
     return _firstName;
  }

  public void setFirstName(String firstName)
  {
    _firstName = firstName;
  }

  public String getLastName()
  {
     return _lastName;
  }

  public void setLastName(String lastName)
  {
    _lastName = lastName;
  }

  private String _firstName = null;
  private String _lastName = null;
  private String _number = null;
  private String _expirationDate = null;
}

