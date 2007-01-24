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

import javax.faces.event.ActionEvent;

public class OrderBean
{

  public OrderBean()
  {
System.out.println("Order constructor");

    // This isn't thread-safe.  I don't care. :)
    _id = _sOrderID++;

  }



  public int getId()
  {
     return _id;
  }

  public void setId(int id)
  {
    _id = id;
  }


   public String getNextAction()
  {
    return "next";

  }

  public String getPreviousAction()
  {
    return "previous";

  }



  public void processNext(
      ActionEvent event)
    {
      System.out.println(" demo processNext");

    }


    public void processPrevious(
      ActionEvent event)
    {
      System.out.println(" demo processPrevious" );

    }

    public AddressBean getShippingAddress()
    {
      return _shippingAddress;
    }

    public void setShippingAddress(AddressBean shippingAddress)
    {
      _shippingAddress = shippingAddress;
    }

    public AddressBean getBillingAddress()
    {
      return _billingAddress;
    }

    public void setBillingAddress(AddressBean billingAddress)
    {
      _billingAddress = billingAddress;
    }

    public CustomerBean getCustomer()
    {
      return _customer;
    }

    public void setCustomer(CustomerBean customer)
    {
      _customer = customer;
    }

    public CreditCardBean getCreditCard()
    {
      return _creditCard;
    }

    public void setCreditCard(CreditCardBean creditCard)
    {
      _creditCard = creditCard;
    }

  private int _id;
  private CustomerBean _customer = new CustomerBean();
  private CreditCardBean _creditCard = new CreditCardBean();
  private AddressBean _shippingAddress = new AddressBean();
  private AddressBean _billingAddress = new AddressBean();

  static private int _sOrderID = 4524;

}

