/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidad.validator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/** 
 * <b>Please be aware that the api's for this interface are under review and
 * may change. It's likely there may be minor changes to the client-side
 * contract in the future.</b>
 * <p>
 * Interface implemented by Objects that wish to perform client-side
 * validation in addition to server-side validation.  Client side validation
 * should always be the same or or more lenient than the server-side
 * validation.
 * </p>
 * 
 * <P>
 *     One of the benefits of ADF Faces is that it supports client-side versions of converters and validators. This means that errors can be caught on the client and a round trip avoided. This class can be used to add client-side validation to a validator.
 *     </P>
 *     <p>
 *     The basic idea of ADF Faces client-side validation is that it works on the client in a very similar way to how it works on the server, except the language on the client is javascript instead of java. ADF Faces supports javascript Validator objects that support the method validate(). A Validator can throw a ValidatorException.  
 *     </p>      Let's say you've written a javax.faces.validator.Validator implementation and now you want to add client-side validation. The first thing to do is write a version of the validator in javascript. Here is the javascript code for the validator "interface".
 *     </p>
 *       <p>
 *  <pre><code>
 * /**
 *  * Validator "interface" similar to javax.faces.validator.Validator,
 *  * except that all relevant information must be passed to the constructor
 *  * as the context and component are not passed to the validate method 
 *  * /
 * function Validator(){}
 * 
 * /**
 * * Perform the correctness checks implemented by this Validator. 
 * * If any violations are found, a ValidatorException will be thrown.
 * * /
 * Validator.prototype.validate = function(value){}
 * 
 * </code></pre>
 * Validators can throw a ValidatorException, here is the signature:
 * <ul>
 * <li>ValidatorException(detail) 
 *  <ul>
 *    <li>detail - Localized detail message text </li>
 *   </ul> 
 * </li>
 * </ul>
 * The javascript validator is attached using this interface, ClientValidator. The method <code>getClientScript()</code> is expected to return an implementation of the javascript Validator object. The method <code>getClientValidation()</code> is expected to return a  javascript constructor which will be used to instantiate an instance of the validator. 
 * 
 * <p>
 * @see javax.faces.validator.Validator
 */
public interface ClientValidator
{
 /**
   * Opportunity for the ClientValidator to return script content.
   * For HTML, this will be javascript that will be embedded in a 
   * script tag. For HTML this method is expected to return an 
   * implementation of the javascript Validator object.
   * <p>This method will be called once per validator instance. 
   * Content that should only be written once per request
   * should only be returned once.
   */
  public String getClientScript(
   FacesContext context,
   UIComponent component);

  /**
   * Called to retrieve the appropriate client
   * validation code for the node and context.  
   * For HTML, this will be javascript that will be embedded in a 
   * script tag. For HTML this method is expected to return a 
   * constructor of the javascript Validator object 
   * returned by getClientScript().
   */
  public String getClientValidation(
   FacesContext context,
   UIComponent component);

 
}
