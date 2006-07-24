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

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

class BaseConverter extends GenericConverter
{
  public BaseConverter()
  {
  }

  public Object convert(Object source, Class targetType)
  {
    if (source instanceof Calendar)
    {
      Calendar cal = (Calendar) source;
      return cal.getTime();
    }
    if (source instanceof Date)
    {
      Date date = (Date) source;
      Calendar cal = Calendar.getInstance();
      cal.setTime(date);
      return cal;
    }
    // Source must be a Number
    Number num = (Number) source;
    
    // identity-equality is used since these are all final classes:
    if ((targetType == Integer.class) || (targetType == Integer.TYPE))
      return new Integer(num.intValue());
    if ((targetType == Byte.class) || (targetType == Byte.TYPE))
      return new Byte(num.byteValue());
    if ((targetType == Double.class) || (targetType == Double.TYPE))
      return new Double(num.doubleValue());
    if ((targetType == Float.class) || (targetType == Float.TYPE))
      return new Float(num.floatValue());
    if ((targetType == Long.class) || (targetType == Long.TYPE))
      return new Long(num.longValue());
    if ((targetType == Short.class) || (targetType == Short.TYPE))
      return new Short(num.shortValue());
    
    throw new IllegalArgumentException("Unsupported conversion from:"+
      source.getClass() + " to:"+targetType);
  }

  public List getTargetTypes(Class sourceType)
  {
    if (Date.class.isAssignableFrom(sourceType))
    {
      return Collections.singletonList(Calendar.class);
    }
    if (Calendar.class.isAssignableFrom(sourceType))
    {
      return Collections.singletonList(Date.class);
    }
    if (Number.class.isAssignableFrom(sourceType))
    {
      return Arrays.asList(new Class[] {
        Byte.class, Double.class, Float.class, 
        Integer.class, Long.class, Short.class,
        Byte.TYPE, Double.TYPE, Float.TYPE, // bug 4891181
        Integer.TYPE, Long.TYPE, Short.TYPE}
        );
    }
    return Collections.EMPTY_LIST;
  }
}
