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
package org.apache.myfaces.trinidadinternal.taglib.convert;

import javax.faces.convert.Converter;
import javax.faces.webapp.ConverterTag;

import javax.servlet.jsp.JspException;
import org.apache.myfaces.trinidadinternal.convert.IntegerConverter;

/**

 * @version 2.0 (1.0) 2000/03/16 23:23:33
 */
public class ConvertIntegerTag extends ConverterTag
{

  public ConvertIntegerTag()
  {
  }

  @Override
  public int doStartTag() throws JspException
  {
    super.setConverterId(IntegerConverter.CONVERTER_ID);
    return super.doStartTag();
  }

  /**
   * 
   */
  @Override
  protected Converter createConverter() throws JspException
  {
    IntegerConverter converter =
                              (IntegerConverter)super.createConverter();
    return converter;
  }
}
