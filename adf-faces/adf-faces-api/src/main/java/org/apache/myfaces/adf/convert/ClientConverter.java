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
package org.apache.myfaces.adf.convert;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * <b>Please be aware that the api's for this interface are under review and
 * may change. It's likely there may be minor changes to the client-side
 * contract in the future.</b>
 * <p>
 * Interface implemented by Objects that wish to perform client-side
 * conversion in addition to server-side conversion.  Client side conversion
 * should always be the same as or more lenient than the server-side
 * conversion.
 * </p>
 *
 * <P>
 *     One of the benefits of ADF Faces is that it supports client-side versions of converters and validators. This means that errors can be caught on the client and a round trip avoided. This class can be used to add client-side conversion to a converter.
 *     </P>
 *     <p>
 *     The basic idea of ADF Faces client-side conversion is that it works on the client in a very similar way to how it works on the server, except the language on the client is javascript instead of java. ADF Faces supports javascript Converter objects that support the methods getAsString() and getAsObject(). A Converter can throw a ConverterException.
 *     </p>      Let's say you've written a javax.faces.convert.Converter implementation and now you want to add client-side conversion. The first thing to do is write a version of the converter in javascript. Here is the javascript code for the converter "interface".
 *     </p>
 *       <p>
 *  <pre><code>
 * /**
 *  * Converter "interface" similar to javax.faces.convert.Converter,
 *  * except that all relevant information must be passed to the constructor
 *  * as the context and component are not passed to the getAsString or getAsObject method
 *  *
 *  * /
 * function Converter()
 * {
 * }
 *
 *  /**
 *  * Convert the specified model object value, into a String for display
 *  *
 *  * @param value Model object value to be converted
 *  * /
 * Converter.prototype.getAsString = function(value){}
 *
 * /**
 *  * Convert the specified string value into a model data object
 *  * which can be passed to validators
 *  *
 *  * @param value String value to be converted
 *  * /
 * Converter.prototype.getAsObject = function(value){}
 * </code></pre>
 * Converters can throw a ConverterException, here is the signature:
 * <ul>
 * <li>ConverterException(detail)
 *  <ul>
 *    <li>detail - Localized detail message text </li>
 *   </ul>
 * </li>
 * </ul>
 * The javascript converter is attached using this interface, ClientConverter. The method <code>getClientScript()</code> is expected to return an implementation of the javascript Converter object. The method <code>getClientConversion()</code> is expected to return a  javascript constructor which will be used to instantiate an instance of the converter.
 * </p>
 * <p>
 * @see javax.faces.convert.Converter
 * @author The Oracle ADF Faces Team
 */
public interface ClientConverter
{
  /**
   * Opportunity for the ClientConverter to return script content.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return an
   * implementation of the javascript Converter object.
   * <p>This method will be called once per converter instance.
   * Content that should only be written once per request
   * should only be returned once.
   */
  public String getClientScript(
   FacesContext context,
   UIComponent component);

  /**
   * Called to retrieve the appropriate client
   * conversion code for the node and context.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return a
   * constructor of the javascript Converter object
   * returned by getClientScript().
   */
  public String getClientConversion(
   FacesContext context,
   UIComponent component);

  static public final String ALERT_FORMAT_KEY =
    "org.apache.myfaces.adf.convert.ALERT_FORMAT";
}
