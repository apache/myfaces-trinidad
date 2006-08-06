/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.taglib.listener;

import org.apache.myfaces.trinidadinternal.taglib.util.TagUtils;

import javax.servlet.jsp.tagext.TagSupport;
import javax.servlet.jsp.JspException;

import javax.faces.application.Application;
import javax.faces.component.ActionSource;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentTag;

import org.apache.myfaces.trinidad.webapp.ELContextTag;


/**
 * JavaServer Faces version 1.2 a <code>setPropertyActionListener</code>, which provides the 
 * same functionality. In JSF 1.2 days this class should be <code>deprecated</code>.
 *
 * @author The Oracle ADF Faces Team
 */
public class SetActionListenerTag extends TagSupport
{
  public void setFrom(String from)
  {
    _from = from;
  }

  public void setTo(String to)
  {
    _to = to;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentTag tag = UIComponentTag.getParentUIComponentTag(pageContext);
    if (tag == null)
    {
      throw new JspException(
        "setActionListener must be inside of a UIComponent tag.");
    }

    // Only run on the first time the tag executes
    if (!tag.getCreated())
      return SKIP_BODY;

    UIComponent component = tag.getComponentInstance();
    if (!(component instanceof ActionSource))
    {
      throw new JspException(
        "setActionListener must be inside of a ActionSource component.");
    }

    ELContextTag parentELContext = (ELContextTag)
       findAncestorWithClass(this, ELContextTag.class);

    Application application =
      FacesContext.getCurrentInstance().getApplication();

    SetActionListener listener = new SetActionListener();
    if (_from != null)
    {
      if (TagUtils.isValueReference(_from))
      {
        String from = _from;
        if (parentELContext != null)
          from = parentELContext.transformExpression(from);

        listener.setValueBinding(SetActionListener.FROM_KEY,
                                 application.createValueBinding(from));
      }
      else
      {
        listener.setFrom(_from);
      }

      if (TagUtils.isValueReference(_to))
      {
        String to = _to;
        if (parentELContext != null)
          to = parentELContext.transformExpression(to);

        listener.setValueBinding(SetActionListener.TO_KEY,
                                 application.createValueBinding(to));
      }
      else
      {
        throw new JspException("setActionListener's 'to' attribute must " +
                               "be an EL expression.");
      }
    }

    ((ActionSource) component).addActionListener(listener);

    return super.doStartTag();
  }

  @Override
  public void release()
  {
    super.release();
    _from = null;
    _to = null;
  }

  private String _from;
  private String _to;
}
