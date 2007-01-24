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

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.event.RangeChangeEvent;

/**
 * Backing bean for the "messages" page.
 */
public class MessagesBackingBean
{
  public MessagesBackingBean()
  {
  }

  public String compact() throws MessagingException
  {
    Folder folder = _folderData.getFolder();
    
    folder.open(Folder.READ_WRITE);
    // It would be much more efficient to simply trim out
    // the list of "expunged" messages from the data model;
    // instead, we're refreshing the list.
    folder.expunge();
    folder.close(true);

    return refresh();
  }


  public String refresh()
  {
    _folderData.flush();
    first();
    return null;
  }


  public void setFolder(FolderData folder)
  {
    _folderData = folder;
  }

  public FolderData getFolder()
  {
    return _folderData;
  }

  public void setMessagesTable(UIXTable table)
  {
    _messagesTable = table;
  }

  public UIXTable getMessagesTable()
  {
    return _messagesTable;
  }

  public boolean isFirstEnabled()
  {
    return _messagesTable.getFirst() > 0;
  }

  public boolean isLastEnabled()
  {
    return (_messagesTable.getFirst() + _messagesTable.getRows()) <  
                _messagesTable.getRowCount();
  }

  public String first()
  {
    _messagesTable.setFirst(0);
    return null;
  }

  public String last()
  {
    // The last row is the row count minus 1
    int lastRow = _messagesTable.getRowCount() - 1;
    if (lastRow >= 0)
    {
      int rows = _messagesTable.getRows();
      _messagesTable.setFirst((lastRow / rows) * rows);
    }

    return null;
  }

  /**
   * Remember the "first" row for the table.  If we didn't
   * do this, there wouldn't be any issue as we page from
   * row to row within this page.  The problem comes when
   * we <em>return</em> to this folder (after showing a message,
   * for example).  There ought to be a better solution to this
   * problem.
   */ 
  public void saveFirst(RangeChangeEvent event)
  {
    _folderData.setStartIndex(event.getNewStart());
  }

  public void performDelete(ActionEvent event)
  {
    Iterator<?> selection = _messagesTable.getSelectedRowKeys().iterator();
    // Nothing was selected
    if (selection.hasNext())
    {
      try
      {
        // Save the old row key
        Object oldRowKey = _messagesTable.getRowKey();

        Folder folder = _folderData.getFolder();
        folder.open(Folder.READ_WRITE);
        List<Message> messageList = new ArrayList<Message>();
        try
        {
          while (selection.hasNext())
          {
            String rowKey = (String) selection.next();
            _messagesTable.setRowKey(rowKey);
            MessageData message = (MessageData) _messagesTable.getRowData();
            if (message == null)
            {
              _LOG.log(Level.WARNING, "Couldn't find message for row {0}",
                       rowKey);
            }
            else
            {
              _LOG.log(Level.FINE, "Attempting to delete message {0}",
                       message.getSubject());
              // Get the actual Message object
              messageList.add(message.getMessage());
            }
          }

          Message[] messages = 
            messageList.toArray(new Message[messageList.size()]);
          folder.setFlags(messages, new Flags(Flags.Flag.DELETED), true);
          // clear the selection:
          _messagesTable.getSelectedRowKeys().clear();
          // refresh the folder so that the little 'deleted' icons show up:
          refresh();
        }
        finally
        {
          // Restore the old key
          _messagesTable.setRowKey(oldRowKey);
          folder.close(false);
        }
      }
      catch (MessagingException me)
      {
        _LOG.log(Level.WARNING, "Couldn't delete", me);
        FacesContext context = FacesContext.getCurrentInstance();
        FacesMessage message =
           MessageUtils.getErrorMessage(context,
                                        "COULD_NOT_DELETE",
                                        new Object[]{me.getMessage()});
        context.addMessage(null, message);

      }
    }
    else
    {
      _LOG.fine("No messages were selected.");
    }
  }

  private UIXTable _messagesTable;
  private FolderData _folderData;

  static private final Logger _LOG =
    Logger.getLogger(MessagesBackingBean.class.getName());
}
