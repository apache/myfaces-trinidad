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
package org.apache.myfaces.trinidadinternal.taglib.listener;

import org.apache.myfaces.trinidadinternal.taglib.util.TagUtils;

import java.io.OutputStream;

import javax.servlet.jsp.tagext.TagSupport;
import javax.servlet.jsp.JspException;

import javax.faces.application.Application;
import javax.faces.component.ActionSource;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentTag;

import org.apache.myfaces.trinidad.webapp.ELContextTag;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 */
public class FileDownloadActionListenerTag extends TagSupport
{
  public void setContentType(String contentType)
  {
    _contentType = contentType;
  }

  public void setFilename(String filename)
  {
    _filename = filename;
  }

  public void setMethod(String method)
  {
    _method = method;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentTag tag = UIComponentTag.getParentUIComponentTag(pageContext);
    if (tag == null)
    {
      throw new JspException(_LOG.getMessage(
        "FILEDOWNLOADACTIONLISTENER_MUST_INSIDE_UICOMPONENT_TAG"));
    }

    // Only run on the first time the tag executes
    if (!tag.getCreated())
      return SKIP_BODY;

    UIComponent component = tag.getComponentInstance();
    if (!(component instanceof ActionSource))
    {
      throw new JspException(_LOG.getMessage(
        "FILEDOWNLOADACTIONLISTENER_MUST_INSIDE_UICOMPONENT_TAG"));
    }

    ELContextTag parentELContext = (ELContextTag)
       findAncestorWithClass(this, ELContextTag.class);

    Application application =
      FacesContext.getCurrentInstance().getApplication();

    FileDownloadActionListener listener = new FileDownloadActionListener();

    if (_filename != null)
    {
      if (TagUtils.isValueReference(_filename))
      {
        String filename = _filename;
        if (parentELContext != null)
          filename = parentELContext.transformExpression(filename);

        listener.setValueBinding(FileDownloadActionListener.FILENAME_KEY,
                                 application.createValueBinding(filename));
      }
      else
      {
        listener.setFilename(_filename);
      }
    }

    if (_contentType != null)
    {
      if (TagUtils.isValueReference(_contentType))
      {
        String contentType = _contentType;
        if (parentELContext != null)
          contentType = parentELContext.transformExpression(contentType);

        listener.setValueBinding(FileDownloadActionListener.CONTENT_TYPE_KEY,
                                 application.createValueBinding(contentType));
      }
      else
      {
        listener.setContentType(_contentType);
      }
    }


    if (TagUtils.isValueReference(_method))
    {
      String method = _method;
      if (parentELContext != null)
          method = parentELContext.transformExpression(method);
      
      listener.setMethod(application.createMethodBinding(method,
                  new Class[]{FacesContext.class, OutputStream.class}));
    }
    else
    {
      throw new JspException(_LOG.getMessage(
        "FILEDOWNLOADACTIONLISTENERS_METHOD_MUST_BE_EL_EXPRESSION"));
    }

    ((ActionSource) component).addActionListener(listener);

    return super.doStartTag();
  }

  @Override
  public void release()
  {
    super.release();
    _contentType = null;
    _filename = null;
    _method = null;
  }

  private String _contentType;
  private String _filename;
  private String _method;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    FileDownloadActionListenerTag.class);
}
