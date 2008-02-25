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

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.model.DataModel;

import javax.mail.FetchProfile;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;

/**
 * Implementation of DataModel that creates "MessageData" objects
 * (cached MessageData instances).
 * As we don't support sorting, and JavaMail only identifies
 * messages by index, we only need to implement DataModel.
 * @todo We don't currently deliver DataModelEvents.
 */
public class MessageDataModel extends DataModel
{
  public MessageDataModel(
    Folder       folder,
    FetchProfile fetchProfile,
    int          blockSize)
  {
    setWrappedData(folder);
    _blockSize = blockSize;
    _fetchProfile   = fetchProfile;
  }

  @Override
  public int getRowCount()
  {
    return _count;
  }

  @Override
  public boolean isRowAvailable()
  {
    int index = getRowIndex();
    return (index >= 0) && (index < getRowCount());
  }

  @Override
  public Object getRowData()
  {
    if (!isRowAvailable())
      return null;

    int index = getRowIndex();
    
    // Flip the indices around
    pageInRowIndex(index);
    return _loaded[index];
  }

  @Override
  public void setRowIndex(int index)
  {
    if (index < -1)
      throw new IllegalArgumentException();

    _rowIndex = index;
  }

  @Override
  public int getRowIndex()
  {
    return _rowIndex;
  }

  @Override
  public Object getWrappedData()
  {
    return _folder;
  }

  @Override
  public void setWrappedData(Object data)
  {
    Folder newFolder = (Folder) data;
    _folder = newFolder;
    _rowIndex = -1;

    if (newFolder != null)
    {
      try
      {
        _count = _folder.getMessageCount();
      }
      // Need to handle more cleanly
      catch (MessagingException me)
      {
        _count = 0;
        _LOG.log(Level.SEVERE, "Could not get message count",  me);
      }
    }
    else
    {
      _count = 0;
    }

    _loaded = new MessageData[_count];
  }

  private int _getFlippedIndex(int index)
  {
    return getRowCount() - index - 1;
  }

  /**
   * Pages in a row index, making sure that it (and all other
   * messages in its block) are available.
   */
  public void pageInRowIndex(int index)
  {
    if (_loaded[index] == null)
    {

      try
      {
        if (_LOG.isLoggable(Level.FINEST))
        {
          _LOG.finest("total messages before open:"+_folder.getMessageCount());
        }

        _folder.open(Folder.READ_ONLY);
        // after the folder is opened, the count may change:
        _count = _folder.getMessageCount();

        // Calculate "from" and "to", zero-indexed
        // Round down to the start of the block
        int fromIndex = (index / _blockSize) * _blockSize;
        int toIndex = fromIndex + _blockSize - 1;
        if (toIndex >= _count)
          toIndex = _count - 1;

        try
        {
          // Retrieve the messages from the one-indexed Javamail API
          int jmFromIndex = _getFlippedIndex(toIndex) + 1;
          int jmToIndex = _getFlippedIndex(fromIndex) + 1;
          if (_LOG.isLoggable(Level.FINEST))
            _LOG.finest("fetching messages from:"+jmFromIndex+
                        " to:"+jmToIndex+
                        " total:"+ getRowCount() +
                        " actual total:"+_folder.getMessageCount());
          Message[] messages = _folder.getMessages(
                                  jmFromIndex,
                                  jmToIndex);
          _folder.fetch(messages, _fetchProfile);
          for (int i = 0; i < messages.length; i++)
          {
            Message message = messages[messages.length - i - 1];
            _loaded[i + fromIndex] = new MessageData(message);
          }
        }
        finally
        {
          _folder.close(false);
        }
      }
      // This is poor;  for starters, the page is likely
      // already displaying, so it's too late to show an error message.
      // We should try paging in rows up front via a RangeChangeListener to
      // catch the earlier and provide useful errors.
      catch (MessagingException me)
      {
        _LOG.log(Level.SEVERE, me.getMessage(), me);
        FacesMessage errorMessage = new FacesMessage(
                          FacesMessage.SEVERITY_ERROR,
                          me.getMessage(),
                          me.getStackTrace().toString());

        FacesContext context = FacesContext.getCurrentInstance();
        context.addMessage(null, errorMessage);
      }
    }
  }

  private Folder    _folder;
  private int       _rowIndex;
  private int       _count;
  private int       _blockSize;
  private MessageData[] _loaded;
  private FetchProfile _fetchProfile;

  static private final Logger _LOG =
    Logger.getLogger(MessageDataModel.class.getName());
}
