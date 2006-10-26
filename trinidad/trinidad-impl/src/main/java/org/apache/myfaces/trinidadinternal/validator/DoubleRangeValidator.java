/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.validator;

import java.util.Collection;
import java.util.Collections;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;




/**
 * <p>Implementation for <code>java.lang.Long</code> values.</p>
 *
 * @author The Oracle ADF Faces Team
 */
public class DoubleRangeValidator extends org.apache.myfaces.trinidad.validator.DoubleRangeValidator
                                implements ClientValidator
{

  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }


  /**
   * @todo this should have not_in_range messages, not just max and min!
   * @todo Format these numbers properly.
   */
  public String getClientValidation(
    FacesContext context,
    UIComponent component)
  {
    
    String maxStr = Double.toString(getMaximum());
    String minStr = Double.toString(getMinimum());
    
    return  ConverterUtils.getClientValidation(context, component,
                                               MAXIMUM_MESSAGE_ID,
                                               MINIMUM_MESSAGE_ID,
                                               javax.faces.validator.DoubleRangeValidator.VALIDATOR_ID,
                                               maxStr, minStr, "TrRangeValidator");
  }
  
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );

}