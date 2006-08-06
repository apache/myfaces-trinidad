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
package org.apache.myfaces.trinidadinternal.convert;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

class BaseConverter extends GenericConverter
{
  public BaseConverter()
  {
  }

  @Override
  public Object convert(Object source, Class<?> targetType)
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

  @SuppressWarnings("unchecked")
  @Override
  public List<Class<?>> getTargetTypes(Class<?> sourceType)
  {
    ArrayList<Class<?>> list = new ArrayList<Class<?>>(1);
    if (Date.class.isAssignableFrom(sourceType))
    {
      list.add(Calendar.class);
    }
    else if (Calendar.class.isAssignableFrom(sourceType))
    {
      list.add(Date.class);
    }
    else if (Number.class.isAssignableFrom(sourceType))
    {
      list.ensureCapacity(12);
      list.add(Byte.class);
      list.add(Double.class);
      list.add(Float.class);
      list.add(Integer.class);
      list.add(Long.class);
      list.add(Short.class);
      list.add(Byte.TYPE);
      list.add(Double.TYPE);
      list.add(Float.TYPE); // bug 4891181
      list.add(Integer.TYPE);
      list.add(Long.TYPE);
      list.add(Short.TYPE);
    }
    
    return list;
  }
}
