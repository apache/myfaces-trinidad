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
package org.apache.myfaces.trinidaddemo.email;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.mail.Folder;
import javax.mail.MessagingException;
import javax.mail.Part;

import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * Backing bean for the "show message" page. Provides support
 * for downloading attachments, and hides the fact that the "message"
 * comes in pageFlowScope from the actual page content.
 */
public class ShowMessageBackingBean
{
  public ShowMessageBackingBean()
  {
    // Code necessary because of:
    //   https://javaserverfaces.dev.java.net/issues/show_bug.cgi?id=22
    RequestContext afContext = RequestContext.getCurrentInstance();
    setMessage((MessageData) afContext.getPageFlowScope().get("message"));
  }

  public MessageData getMessage()
  {
    return _message;
  }

  public void setAttachmentToDownload(Part attachment)
  {
    _attachmentToDownload = attachment;
  }


  public Part getAttachmentToDownload()
  {
    return _attachmentToDownload;
  }


  public void setMessage(MessageData message)
  {
    try
    {
      _LOG.log(Level.INFO, "Displaying message {0}", message.getSubject());
    }
    catch (MessagingException me)
    {
      _LOG.log(Level.SEVERE, "Can't get the subject", me);
    }

    _message = message;
  }

  public String downloadAttachment() throws IOException, MessagingException
  {
    if (_attachmentToDownload == null)
    {
      _LOG.severe("No attachment available");
      return null;
    }

    _message.getMessage().getFolder().open(Folder.READ_ONLY);

    InputStream in = _attachmentToDownload.getInputStream();

    FacesContext context = FacesContext.getCurrentInstance();
    // Get the ServletResponse;  nothing on ExternalContext is sufficient
    ServletResponse response = (ServletResponse)
      context.getExternalContext().getResponse();
    response.setContentType(_attachmentToDownload.getContentType());

    // If the size of the attachment is known, pass that on.
    int size = _attachmentToDownload.getSize();
    if (size >= 0)
    {
      response.setContentLength(size);
    }

    if (_LOG.isLoggable(Level.INFO))
      _LOG.info("Downloading content+ [size=" + size +",contentType=" +
                _attachmentToDownload.getContentType() + "]");

    if (response instanceof HttpServletResponse)
    {
      String filename = _attachmentToDownload.getFileName();
      if (filename != null)
      {
        ((HttpServletResponse) response).setHeader(
            "Content-disposition",
            "attachment; filename=\"" + filename + "\"");
      }
    }

    // Pass the text along, 128K at a time.
    try
    {
      OutputStream out = response.getOutputStream();
      try
      {
        byte[] buffer = new byte[131072];
        while (true)
        {
          int count = in.read(buffer);
          if (count < 0)
            break;

          out.write(buffer, 0, count);
        }
      }
      // Close up the response
      finally
      {
        // And tell JSF that we handled everything
        context.responseComplete();
        out.close();
      }
    }
    // And make sure the folder got closed
    finally
    {
      _message.getMessage().getFolder().close(false);
    }

    return null;
  }

  private MessageData _message;
  private Part        _attachmentToDownload;

  static private final Logger _LOG =
    Logger.getLogger(ShowMessageBackingBean.class.getName());
}
