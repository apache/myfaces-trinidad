/*
 * Copyright 2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidaddemo.convertValidate;


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


  @SuppressWarnings("unchecked")
  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    // check if the script has already been returned this request
    Object scriptReturned =
                context.getExternalContext().getRequestMap().get(VALIDATOR_ID);

    // if scriptReturned is null the script hasn't been returned yet
    if ( scriptReturned == null)
    {
      context.getExternalContext().getRequestMap().put(VALIDATOR_ID,
                                                       Boolean.TRUE);
      return  _sPasswordValidatorJS;
    }
    // if scriptReturned is not null, then script has already been returned,
    // so don't return it again.
    else
      return null;

   }

  private static Object _getLabel(UIComponent component)
  {
    Object o = null;
    if (component != null)
    {
      o = component.getAttributes().get("label");
      if (o == null)
        o = component.getValueBinding("label");
    }
    return o;
  }

  // in a real app the messages would be translated
  // The fourth field marker gets the field label
  private static final String _VALIDATOR_INSTANCE_STRING =
    "new PasswordValidator({"
    + "N:'{0} - The password value must contain at least one number.'})";

  private static final String _sPasswordValidatorJS =
    "function passwordValidate(value)" +
       "{if (!value)return void (0);" +
        "if (value == '******')return void (0);" +
        "var messageKey = PasswordValidator.NUMBER;" +
        "for (var i = 0; i < value.length; i++)" +
        "{var subValue = value.substring(i, i+1);" +
         "if (!isNaN(parseInt(subValue))){" +
            "messageKey = void (0);break;}}" +
        "if (messageKey != void(0))" +
          "return new ValidatorException(this._messages[messageKey]);" +
        "return void(0);}" +
    "function PasswordValidator(messages)" +
      "{this._messages = messages;}" +
    "PasswordValidator.prototype = new Validator();" +
    "PasswordValidator.prototype.validate = passwordValidate;" +
    "PasswordValidator.NUMBER = 'N';" ;
}
