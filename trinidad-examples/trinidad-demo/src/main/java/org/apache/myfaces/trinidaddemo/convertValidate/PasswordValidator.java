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
package org.apache.myfaces.trinidaddemo.convertValidate;

import java.util.Collection;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidad.util.LabeledFacesMessage;

/**
 * <p>Password validator - this is an incredibly simple
 * password validator that makes sure there's at least one number
 * in the password.</p>
 *
 */
public class PasswordValidator implements Validator, ClientValidator
{
  public static final String VALIDATOR_ID = "org.apache.myfaces.trinidaddemo.PasswordValidator";

  public void validate(
    FacesContext context,
    UIComponent component,
    Object value) throws ValidatorException
  {

    String password = "";

    if ( value != null)
      password = value.toString().trim();

    for (int j = 0;j < password.length();j++)
    {
      if (Character.isDigit(password.charAt(j)))
      {
        return;
      }
    }

    // Using the LabeledFacesMessage allows the <tr:messages> component to
    // properly prepend the label as a link.
    LabeledFacesMessage lfm =
      new LabeledFacesMessage(FacesMessage.SEVERITY_ERROR,
                              "Validation Error",
                              "The password must contain at least one number");
    lfm.setLabel(_getLabel(component));
    throw new ValidatorException(lfm);
  }


  public String getClientValidation(
    FacesContext context,
   UIComponent component)
  {
    return (_VALIDATOR_INSTANCE_STRING);
  }


  public Collection<String> getClientImportNames()
  {
    return null;
  }

  public String getClientLibrarySource(
   FacesContext context)
  {
    return context.getExternalContext().getRequestContextPath() + 
            "/jsLibs/passwordValidator.js";    
  }


  @SuppressWarnings("unchecked")
  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
      return null;
   }

  private static Object _getLabel(UIComponent component)
  {
    Object o = null;
    if (component != null)
    {
      o = component.getAttributes().get("label");
      if (o == null)
        o = component.getValueExpression("label");
    }
    return o;
  }

  // in a real app the messages would be translated
  // The fourth field marker gets the field label
  private static final String _VALIDATOR_INSTANCE_STRING =
    "new PasswordValidator({"
    + "NS:'The password is invalid.',"
    + "ND:'The password value must contain at least one number.'})";


}
