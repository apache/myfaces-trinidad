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

public class AddressBean
{

  public AddressBean()
  {
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


  public String getStreetAddress()
  {
    return _streetAddress ;
  }

  public void setStreetAddress(String streetAddress)
  {
    _streetAddress = streetAddress;
  }

  public String getCity()
  {
    return _city ;
  }

  public void setCity(String city)
  {
    _city = city;
  }

  public String getState()
  {
    return _state ;
  }

  public void setState(String state)
  {
    _state = state;
  }


  public String getZip()
  {
    return _zip ;
  }

  public void setZip(String zip)
  {
    _zip = zip;
  }


  private String _firstName = null;
  private String _lastName = null;
  private String _streetAddress = null;
  private String _city = null;
  private String _state = null;
  private String _zip = null;

}

