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

import java.io.InputStream;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.DataSource;

import javax.mail.BodyPart;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Transport;

import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;


import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.trinidad.model.UploadedFile;

/**
 * Backing bean for the "show message" page.  Provides some
 * getters for things that aren't quite JavaBeans on the Message API.
 */
public class NewMessageBackingBean
{
  public NewMessageBackingBean()
  {
  }

  public void setAccount(AccountData account)
  {
    _account = account;
  }

  public AccountData getAccount()
  {
    return _account;
  }

  public void validateEmailList(FacesContext context,
                                UIComponent  component,
                                Object       value) throws ValidatorException
  {
    if (value == null)
      return;

    try
    {
      _getEmailList(value.toString());
    }
    catch (AddressException ae)
    {
      throw new ValidatorException(
       MessageUtils.getErrorMessage(context,
                                    "EMAIL_LIST_ERROR",
                                    new Object[]{ae.getRef()}));

    }
  }

  public String getTo()
  {
    return _to;
  }

  public void setTo(String to)
  {
    _to = to;
  }


  public String getCc()
  {
    return _cc;
  }

  public void setCc(String cc)
  {
    _cc = cc;
  }

  public String getContent()
  {
    return _content;
  }

  public void setContent(String content)
  {
    _content = content;
  }

  public String getSubject()
  {
    return _subject;
  }

  public void setSubject(String subject)
  {
    _subject = subject;
  }

  public UploadedFile getAttachment1()
  {
    return _attachment1;
  }

  public void setAttachment1(UploadedFile attachment1)
  {
    _attachment1 = attachment1;
  }

  public UploadedFile getAttachment2()
  {
    return _attachment2;
  }

  public void setAttachment2(UploadedFile attachment2)
  {
    _attachment2 = attachment2;
  }


  public UploadedFile getAttachment3()
  {
    return _attachment3;
  }

  public void setAttachment3(UploadedFile attachment3)
  {
    _attachment3 = attachment3;
  }

  public String send()
  {
    Session session = _getSession();
    Message msg = _setupMessage(new MimeMessage(session));
    if (msg == null)
    {
      _LOG.info("Could not create Message object for " + getSubject());
      return null;
    }

    try
    {
      Transport.send(msg);
    }
    catch (MessagingException me)
    {
      _showSendException(me);
      return null;
    }

    _LOG.info("Sent succesfully");

    try
    {
      Store store = _account.getStore();
      // =-=AEW Hardcoding "Sent" as the folder to save "Sent" messages
      Folder folder = store.getFolder("Sent");
      if (folder == null)
      {
        // Can "folder" be null here?
        throw new IllegalStateException("\"Sent\" folder was null");
      }
      else
      {
        if (!folder.exists())
          folder.create(Folder.HOLDS_MESSAGES);

        folder.appendMessages(new Message[]{msg});
      }
    }
    // Need to do something better;  like a "Warning: message sent, but not
    // saved" message for the user?
    catch (Exception e)
    {
      _LOG.log(Level.WARNING, "Couldn't save message in \"Sent\" folder", e);
    }

    return "sentMessage";
  }


  private Session _getSession()
  {
    Properties props = new Properties(System.getProperties());
    if (_account.getSmtpServer() != null)
      props.put("mail.smtp.host", _account.getSmtpServer());
    return Session.getInstance(props, null);
  }

  public String saveAsDraft()
  {
    _LOG.info("Beginning send of message " + getSubject());

    Session session = _getSession();
    Message msg = _setupMessage(new MimeMessage(session));
    if (msg == null)
    {
      _LOG.info("Could not create Message object for " + getSubject());
      return null;
    }

    try
    {
      Store store = _account.getStore();
      // =-=AEW Hardcoding "Drafts" as the folder to save drafts
      Folder folder = store.getFolder("Drafts");
      if (folder == null)
      {
        // Can "folder" be null here?
        throw new IllegalStateException("\"Drafts\" folder was null");
      }
      else
      {
        if (!folder.exists())
          folder.create(Folder.HOLDS_MESSAGES);
        
        folder.appendMessages(new Message[]{msg});
      }
    }
    // Need to do something better...
    catch (Exception e)
    {
      _showSendException(e);
      return null;
    }
    
    // And go back to the current message folder
    // =-=aew Should be a "popView" thing
    return "messages";
  }

  /**
   * Set up a new message.
   */
  private Message _setupMessage(Message msg)
  {
    try
    {
      String username = _account.getUsername();
      String from = username + "@" + _account.getDomain();
      List<InternetAddress> to = _getEmailList(getTo());
      
      List<InternetAddress> cc = null;
      String ccString = getCc();
      if(ccString != null) 
      {
        cc = _getEmailList(ccString);  
      }
      
      msg.setFrom(new InternetAddress(from));
      if ((to != null) && !to.isEmpty())
        msg.setRecipients(Message.RecipientType.TO,
                          to.toArray(new InternetAddress[0]));

      if ((cc != null) && !cc.isEmpty())
        msg.setRecipients(Message.RecipientType.CC,
                          cc.toArray(new InternetAddress[0]));
      msg.setSubject(_subject == null ? "" : _subject);
      if ((_attachment1 == null) &&
          (_attachment2 == null) &&
          (_attachment3 == null))
      {
        msg.setText(_content == null ? "" : _content);
      }
      // Multipart.
      else
      {
        // Create the message part
        BodyPart messageBodyPart = new MimeBodyPart();

        // Fill the message
        messageBodyPart.setText(_content == null ? "" : _content);

        Multipart multipart = new MimeMultipart();
        multipart.addBodyPart(messageBodyPart);

        if (_attachment1 != null)
          _addAttachment(multipart, _attachment1);
        if (_attachment2 != null)
          _addAttachment(multipart, _attachment2);
        if (_attachment3 != null)
          _addAttachment(multipart, _attachment3);

        // Put all the parts in the message
        msg.setContent(multipart);
      }

      String mailer = "OracleAdfEmailDemo";
      msg.setHeader("X-Mailer", mailer);
      msg.setSentDate(new Date());

      return msg;
    }
    catch(AddressException ae)
    {
       _showSendException(ae);
    }
    catch(MessagingException me)
    {
      _showSendException(me);
    }
    catch(Exception e)
    {
      _showSendException(e);
    }

    return null;
  }

  private void _showSendException(Exception e)
  {
    FacesContext context = FacesContext.getCurrentInstance();
    context.addMessage(null,
                       new FacesMessage(FacesMessage.SEVERITY_ERROR,
                                        e.getMessage(), null));
    _LOG.log(Level.WARNING, "Couldn't send message", e);
  }

  private void _addAttachment(Multipart multipart, UploadedFile file)
    throws MessagingException
  {
    BodyPart messageBodyPart = new MimeBodyPart();
    DataSource source = new UploadedFileDataSource(file);
    messageBodyPart.setDataHandler(new DataHandler(source));
    messageBodyPart.setFileName(file.getFilename());
    multipart.addBodyPart(messageBodyPart);
  }

  static private List<InternetAddress> _getEmailList(String values)
    throws AddressException
  {
    ArrayList<InternetAddress> list = new ArrayList<InternetAddress>();
    StringTokenizer tokens = new StringTokenizer(values.toString(), ",");
    while (tokens.hasMoreTokens())
    {
      String token = tokens.nextToken().trim();

      InternetAddress address = new InternetAddress(token);

      // JavaMail 1.3 API:
      //InternetAddress address = new InternetAddress(token, false);
      //address.validate();
      
      list.add(address);
    }

    return list;
  }

  private AccountData _account;

  private String _subject;
  private String _to;
  private String _cc;
  private String _content;
  private UploadedFile _attachment1;
  private UploadedFile _attachment2;
  private UploadedFile _attachment3;

  private static final class UploadedFileDataSource implements DataSource
  {
    public UploadedFileDataSource(UploadedFile file)
    {
      _file = file;
      _LOG.info("Source for uploaded file " + file.getFilename() + " with " + file.getLength() + " bytes");
    }

    public String getContentType()
    {
      return _file.getContentType();
    }

    public InputStream getInputStream() throws IOException
    {
      return _file.getInputStream();
    }

    public String getName()
    {
      return _file.getFilename();
    }

    public java.io.OutputStream getOutputStream()
    {
      return null;
    }

    private final UploadedFile _file;
  }


  static private final Logger _LOG =
    Logger.getLogger(ShowMessageBackingBean.class.getName());

}
