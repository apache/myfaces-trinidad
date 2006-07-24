/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.convert;

import java.util.List;

/**
 * This class is capable of converting
 * one Object into another.
 * A converter may be capable of converting a single Object into
 * many different types.
 * @author The Oracle ADF Faces Team
 */
public abstract class GenericConverter
{
  /**
   * converts the given Object into an instance of the 
   * targetType.
   * @return an instance of the targetType.
   */
  public abstract Object convert(Object source, Class targetType);
    
  /**
   * Gets all the supported targetTypes for the given sourceType.
   * This converter must be able to convert the sourceType into each of the
   * supported targetTypes.
   * @return Each item is of type {@link Class}. An empty list must be
   * returned if the given sourceType cannot be converted into anything.
   */
  public abstract List getTargetTypes(Class sourceType);
}
