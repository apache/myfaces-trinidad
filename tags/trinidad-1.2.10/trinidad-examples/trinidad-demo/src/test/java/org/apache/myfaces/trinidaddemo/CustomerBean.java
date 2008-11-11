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

public class CustomerBean
{

  public CustomerBean()
  {

    // This isn't thread-safe.  I don't care. :)
    _id = _sID++;

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

  public String getUserName()
  {
     return _userName;
  }

  public void setUserName(String userName)
  {
    _userName = userName;
  }

  public String getPassword()
  {
     return _password;
  }

  public void setPassword(String password)
  {
    _password = password;
  }

  private String _firstName = null;
  private String _lastName = null;
  private String _userName = null;
  private String _password = null;
  // Never read
  @SuppressWarnings("unused")
  private int   _id;

  static private int _sID = 2242;

}

