/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidad.webapp;

import java.lang.reflect.Method;

import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.convert.Converter;
import javax.faces.webapp.UIComponentClassicTagBase;
import javax.faces.webapp.UIComponentELTag;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.Tag;

import org.apache.myfaces.trinidad.convert.ColorConverter;
import org.apache.myfaces.trinidad.convert.DateTimeConverter;
import org.apache.myfaces.trinidad.convert.NumberConverter;

/**
 * This is the Trinidad version of the JSF <code>ConverterELTag</code> class.
 * The main difference is that this class is <b>NOT</b> inheriting from
 * the standard <code>TagSupport</code> and therefore does not
 * implement <code>Serializable</code> interface.
 *
 * @author Apache MyFaces team
 */
public abstract class TrinidadConverterELTag extends TrinidadTagSupport
{
    @Override
    public int doStartTag() throws JspException
    {
        UIComponentClassicTagBase componentTag = UIComponentELTag.getParentUIComponentClassicTagBase(pageContext);
        if (componentTag == null)
        {
            throw new JspException("no parent UIComponentTag found");
        }
        if (!componentTag.getCreated())
        {
            return Tag.SKIP_BODY;
        }

        UIComponent component = componentTag.getComponentInstance();
        if (component == null)
        {
            throw new JspException("parent UIComponentTag has no UIComponent");
        }
        if (!(component instanceof ValueHolder))
        {
            throw new JspException("UIComponent is no ValueHolder");
        }

        Converter converter = createConverter();

        // Check whether component supports multiple converters with method addConverter
        // if it does, add it by calling addConverter, otherwise call setConverter
        Class cls = component.getClass();
        boolean multipleConvertersSupported = true;
        Method methodAddConverter = null;
        try
        {
          Class[] types = new Class[] {Converter.class};
          methodAddConverter = cls.getMethod("addConverter", types);
          types = new Class[] {};
        }
        catch (NoSuchMethodException e)
        {
          multipleConvertersSupported = false;
        }
        
        if (multipleConvertersSupported)
        {
          try
          {
            methodAddConverter.invoke(component, new Object[] {converter});
          }
          catch (Exception e)
          {
            // let it go
            ;
          }
        }
        else            
          ((ValueHolder)component).setConverter(converter);

        return Tag.SKIP_BODY;
    }

    protected abstract Converter createConverter() throws JspException;
}
