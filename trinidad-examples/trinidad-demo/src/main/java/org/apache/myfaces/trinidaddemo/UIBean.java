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
package org.apache.myfaces.trinidaddemo;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

import java.util.Iterator;
import java.util.Map;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.event.ValueChangeEvent;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelPage;

import org.apache.myfaces.trinidad.model.UploadedFile;

public class UIBean
{
  public UIBean()
  {
  }

  public CorePanelPage getPanelPage()
  {
    return _panelPage;
  }

  public UIBeanState getState()
  {
    return _state;
  }

  public void setState(UIBeanState state)
  {
    _state = state;
  }

  public void setPanelPage(CorePanelPage panelPage)
  {
    _panelPage = panelPage;
  }

  public void fileUploaded(ValueChangeEvent event) throws IOException
  {
    UploadedFile file = (UploadedFile) event.getNewValue();
    if (file != null)
    {
      FacesContext context = FacesContext.getCurrentInstance();
      FacesMessage message = new FacesMessage(
         "Uploaded file " + file.getFilename() +
         " (" + file.getLength() + " bytes)"+". Bytes available to read: " +
         file.getInputStream().available());
      context.addMessage(event.getComponent().getClientId(context), message);
    }
  }

  @SuppressWarnings("unchecked")
  public void testFailover()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    Map<String, Object> session = 
      context.getExternalContext().getSessionMap();
    
    Map.Entry<String, Object> writing = null;
    try
    {
      ObjectOutputStream oos =
        new ObjectOutputStream(new ByteArrayOutputStream(2 << 16));
      Iterator<Map.Entry<String, Object>> entries = session.entrySet().iterator();
      while (entries.hasNext())
      {
        writing = entries.next();
        oos.writeObject(writing.getValue());
        context.addMessage(null,
                           new FacesMessage("Successfully serialized " + writing.getValue() + " [at " + writing.getKey() + "]"));
      }
    }
    catch (IOException ioe)
    {
      context.addMessage(null,
           new FacesMessage(FacesMessage.SEVERITY_ERROR,
                            "Failed while outputting object " + writing.getValue()  +
                            " [at " + writing.getKey() + "]",
                            ioe.getMessage()));
    }
  }

  //
  // For testing purposes, here's a series of methods that can
  // be EL-addressed that blow up.  These exceptions should
  // be displayed somewhere (preferably in a logged error
  // message), not swallowed.
  //

  public String actionThatFails()
  {
    throw new IllegalStateException("Calling this action is a bad move");
  }

  public void listenerThatFails(ValueChangeEvent event)
  {
    throw new IllegalStateException("Using this listener is a bad move");
  }

  public String getFailedProperty()
  {
    throw new IllegalStateException("Getting this property is a bad move");
  }


  private CorePanelPage _panelPage;
  private UIBeanState _state;
}
