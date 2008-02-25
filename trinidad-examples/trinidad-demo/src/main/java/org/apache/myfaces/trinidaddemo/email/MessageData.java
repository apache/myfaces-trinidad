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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Part;

import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMultipart;
import javax.mail.Flags.Flag;

/**
 * MessageData contains all the information needed to display a UI
 * for a javax.mail.Message object.  There are methods to get the
 * sender, the subject, the size, the date, the to list, the cc list,
 * the content type, the content itself, and the line count of the message.
 *
 * @version 1.0
 */
public class MessageData
{
  /**
   * Create a new MessageData object given the message object.
   */
  public MessageData(Message message) throws MessagingException
  {
    _message = message;

    // cache the size just because it involves a calculation
    _size = message.getSize() / 1024;

    // cache this since the folder has to be open to get this:
    _isRead = _message.isSet(Flag.SEEN); 
    _isDeleted = _message.isSet(Flag.DELETED);
  }

  /**
   * Get the number of the message in its containing folder.
   *
   * @return the message number. this index is based at 1
   */
  public int getMessageNumber()
  {
    return _message.getMessageNumber();
  }

  /**
   * Get the Address object that corresponds to the sender
   */
  public Address getSender() throws MessagingException
  {
    // we assume that the original sender is the first one
    return _message.getFrom()[0];
  }

  /**
   * Get the subject of the message.  If the subject is empty or null
   * we return a string <empty> to indicate the subject did not contain
   * anything.
   */
  public String getSubject() throws MessagingException
  {
    String subject = _message.getSubject();

    // if the subject is empty than make sure we return something so the
    // user can click on it to see its contents!  Need to internationalize this.
    if ( (subject == null) || "".equals(subject) )
      subject = "<empty>";

    return subject;
  }

  /**
   * Get the size of the message in kilobytes.
   *
   * @return size in kilo bytes
   */
  public int getSize()
  {
    return _size;
  }

  /**
   * Get the sent date of the message.
   */
  public Date getSentDate() throws MessagingException
  {
    return _message.getSentDate();
  }

  /**
   * Get an array of Address objects that correspond to the list of
   * addresses the message was sent to.
   */
  public Address[] getTos() throws MessagingException
  {
    Address[] tos = _message.getRecipients(Message.RecipientType.TO);
    return tos;
  }

  /**
   * Get an array of Address objects that correspond to the list of
   * addresses the message was cc'd to.
   */
  public Address[] getCcs() throws MessagingException
  {
    Address[] ccs = _message.getRecipients(Message.RecipientType.CC);
    return ccs;
  }

  /**
   * Get a string representing the content type of the message.  The
   * string returned is truncated to just show the initial part of the type
   * like text/plain.  Also if the original message is a more complex type
   * like multipart, this method will actually search through all the parts
   * and try and find a text/plain or text/html to make the UI easier to
   * handle.
   */
  public String getContentType() throws MessagingException
  {
    // catch IOExceptions and ignore them, because content type really
    // shouldn't care much about io exceptions
    try
    {
      _tryInit();
    }
    catch (IOException ioe)
    {
      ;
    }

    return _contentType;
  }

  /**
   * Get an object representing the content type of the message.  If the
   * original message is a more complex type like multipart, this method
   * will actually search through all the parts and try and find a
   * text/plain or text/html to make the UI easier to handle.
   */
  public Object getContent() throws MessagingException, IOException
  {
    // catch IOExceptions and ignore them, because content type really
    // shouldn't care much about io exceptions
    try
    {
      _tryInit();
    }
    catch (IOException ioe)
    {
      ;
    }

    return _content;
  }

  /**
   * Get the number of lines in the message.  If the original message is a
   * more complex type like multipart, this method will actually search
   * through all the parts and try and find a text/plain or text/html
   * and getLineCount() will return the number of messages in that part.
   */
  public int getLineCount() throws MessagingException
  {
    // catch IOExceptions and ignore them, because content type really
    // shouldn't care much about io exceptions
    try
    {
      _tryInit();
    }
    catch (IOException ioe)
    {
      ;
    }

    return _lineCount;
  }


  /**
   * Return true if the message has been read yet.
   */
  public boolean isRead()
  {
    return _isRead;
  }

  /**
   * Return true if the message has been deleted.
   */
  public boolean isDeleted()
  {
    return _isDeleted;
  }
  
  /**
   * Get the underlying Message Object.
   */
  public Message getMessage()
  {
    return _message;
  }

  /**
   * Get the list of attachments.
   */ 
  public List<BodyPart> getAttachments()
  {
    return _attachments;
  }

  /**
   * Returns true if any attachments are available.
   */
  public boolean isAttachmentPresent()
  {
    return (_attachments != null) && (!_attachments.isEmpty());
  }


  ////////////////////////////////////////////////////////////////////////////
  // private methods
  ////////////////////////////////////////////////////////////////////////////
  /**
   * Initialize the linecount, content type, and content once and for all.
   */
  synchronized private void _tryInit() throws MessagingException, IOException
  {
    // only gather the information if we haven't been initalized yet
    if (_lineCount == -1)
    {
      int count = 0;
      String contentType = null;
      Object content = null;

      // open the folder;  open it read-write if the message hasn't yet
      // been read (in which case we'll need to set it accordingly)
      _message.getFolder().open(_isRead
                                ? Folder.READ_ONLY
                                : Folder.READ_WRITE);
      
      List<BodyPart> attachments = new ArrayList<BodyPart>();

      try
      {
        // get the linecount, content type, and content
        count = _message.getLineCount();
        contentType = _message.getContentType();
        content = _message.getContent();

        // if its a multipart type then lets look through all the parts
        // and see if we can find the more interesting text or html ones
        // so that we can display them better
        if (contentType.startsWith(_MULTIPART))
        {
          if (content instanceof MimeMultipart)
          {
            boolean found = false;
            
            MimeMultipart mm = (MimeMultipart)content;
            for (int i=0;i<mm.getCount();i++)
            {
              BodyPart bp = mm.getBodyPart(i);
              
              if (!found && (bp instanceof MimeBodyPart))
              {
                MimeBodyPart mbp = (MimeBodyPart)bp;
                String type = mbp.getContentType();
                
                if (type.startsWith(_HTML_TEXT) ||
                    type.startsWith(_PLAIN_TEXT) )
                {
                  found = true;
                  count = mbp.getLineCount();
                  content = mbp.getContent();
                  contentType = type;
                  
                  // be happy with the first thing we find either plain
                  // text or html, and skip over it as an attachment
                  continue;
                }
              }

              // OK, now let's see if it's an attachment
              String disp = bp.getDisposition();
              if (disp == null || disp.equalsIgnoreCase(Part.ATTACHMENT))
              {
                attachments.add(bp);
              }
            }
            
            // if we don't find something either text or html, and we don't
            // have any attachments then we are in trouble.  Just throw an
            // exception and let the code below set things up properly.
            if (!found)
            {
              if (!attachments.isEmpty())
                content = "";
              else
                throw new IllegalStateException();
            }
          }
        }

        // strip of the extra parts of the content type so we return something
        // like plain/text or html/text
        int index = contentType.indexOf(';');
        if (index != -1)
          contentType = contentType.substring(0, index);

        // Mark it as seen
        _message.getFlags().add(Flag.SEEN);
        _isRead = true;
      }
      catch (Exception e)
      {
        content = "Error trying to display content";
        count = 1;
        contentType = _PLAIN_TEXT;
        attachments.clear();
      }
      // always make sure to close the message in case there was an error.
      finally
      {
        _message.getFolder().close(false);
      }

      _lineCount = count;
      _contentType = contentType;
      _content = content;
      _attachments = attachments;
    }
  }

  // this implementation tries very poorly to handle attachments by just
  // putting them inline.  If we had more time to figure out java mail we
  // could do something better with attachements
  /**
   * Initialize the linecount, content type, and content once and for all.
  synchronized private void _tryInit() throws MessagingException, IOException
  {
    // only gather the information if we haven't been initalized yet
    if (_lineCount == -1)
    {
      int count = 0;
      String contentType = null;
      Object content = null;

      // open the folder
      _message.getFolder().open(Folder.READ_ONLY);

      try
      {
        // get the linecount, content type, and content
        count = _message.getLineCount();
        contentType = _message.getContentType();
        content = _message.getContent();

        // if its a multipart type then lets look through all the parts
        // and see if we can find the more interesting text or html ones
        // so that we can display them better
        if (contentType.startsWith(_MULTIPART))
        {
            if (content instanceof MimeMultipart)
            {
              MimeMultipart mm = (MimeMultipart)content;
              Object plainContent = null;
              Object htmlContent = null;
              int plainCount = 0;
              int htmlCount = 0;

              // loop through all of the parts.  If we find more than
              // one plain text than append then all together so we can
              // display simple attachements better
              for (int i=0;i<mm.getCount();i++)
              {
                BodyPart bp = mm.getBodyPart(i);

                if (bp instanceof MimeBodyPart)
                {
                  MimeBodyPart mbp = (MimeBodyPart)bp;
                  String type = mbp.getContentType();

                  if (type.startsWith(_PLAIN_TEXT))
                  {
                    plainCount += mbp.getLineCount();
                    Object newContent = mbp.getContent();

                    if (plainContent == null)
                      plainContent = newContent;
                    else
                    {
                      plainContent = plainContent + "\n---------------------------------\n" + newContent;
                      plainCount++;
                    }
                  }
                  else if (type.startsWith(_HTML_TEXT))
                  {
                    htmlCount += mbp.getLineCount();
                    Object newContent = mbp.getContent();

                    if (htmlContent == null)
                      htmlContent = newContent;
                    else
                    {
                      htmlContent = htmlContent + "\n---------------------------------\n" + newContent;
                      htmlCount++;
                    }
                  }
                }
              }

              // if we found any plain content use that first
              if (plainContent != null)
              {
                count = plainCount;
                content = plainContent;
                contentType = _PLAIN_TEXT;
              }
              // then html
              else if (htmlContent != null)
              {
                count = htmlCount;
                content = htmlContent;
                contentType = _HTML_TEXT;
              }
              // if we found neither thrown an exception which we will
              // catch down below to setup state
              else
              {
                throw new IllegalStateException();
              }
            }
        }

        // strip of the extra parts of the content type so we return something
        // like plain/text or html/text
        int index = contentType.indexOf(';');
        if (index != -1)
          contentType = contentType.substring(0, index);
      }
      catch (Exception e)
      {
        content = "Could not display content";
        count = 0;
        contentType = _PLAIN_TEXT;
      }
      // always make sure to close the message in case there was an error.
      finally
      {
        _message.getFolder().close(false);
      }

      _lineCount = count;
      _contentType = contentType;
      _content = content;
    }
  }
   */

  ////////////////////////////////////////////////////////////////////////////
  // private variables
  ////////////////////////////////////////////////////////////////////////////
  private static final String _PLAIN_TEXT = "TEXT/PLAIN";
  private static final String _HTML_TEXT = "TEXT/HTML";
  private static final String _MULTIPART = "multipart";

  private final Message _message;
  private Object _content = null;
  private String _contentType= null;
  private int _lineCount = -1;
  private int _size;
  private boolean _isRead;
  private boolean _isDeleted;
  private List<BodyPart> _attachments;
}

