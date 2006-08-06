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
package org.apache.myfaces.trinidadinternal.taglib;

import javax.faces.component.UIComponent;

import javax.servlet.jsp.tagext.TagSupport;

import org.apache.myfaces.trinidad.component.UIXComponentRef;

import javax.servlet.jsp.JspException;
import org.apache.myfaces.trinidadinternal.taglib.util.TagUtils;
import javax.faces.webapp.UIComponentTag;

public class ComponentDefTag extends TagSupport
{
  public ComponentDefTag()
  {
  }


  private String _var;
  public void setVar(String var)
  {
    _var = var;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentTag tag = UIComponentTag.getParentUIComponentTag(pageContext);
    if (tag == null)
    {
      throw new JspException(
        "componentDef cannot be run as a stand-alone. It must be included inside a JSF component tree.");
    }

    // Only run on the first time the tag executes
    if (tag.getCreated())
    {
      UIComponent component = tag.getComponentInstance();
      if (!(component instanceof UIXComponentRef))
      {
        throw new JspException(
          "componentDef must be included as a child of an <tr:componentRef>.");
      }

      if (_var != null)
      {
        if (TagUtils.isValueReference(_var))
          throw new JspException("tr:componentDef does not support EL on 'var'");
          
        ((UIXComponentRef) component).setVar(_var);
      }
    }

    return EVAL_PAGE;
  }

  @Override
  public void release()
  {
    super.release();
    _var = null;
  }
}
