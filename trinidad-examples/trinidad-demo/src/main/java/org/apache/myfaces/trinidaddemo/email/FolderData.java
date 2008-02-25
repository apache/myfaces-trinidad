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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.model.DataModel;

import javax.mail.Folder;
import javax.mail.FetchProfile;
import javax.mail.MessagingException;

/**
 * Reflects the folder information provided by javaMail folder
 * @version 1.0
 */
public class FolderData
{

  /**
   * @param data the user who owns this folder
   * @param folder the folder being reflected
   */
  public FolderData(AccountData data, Folder folder) throws MessagingException
  {
    _folder = folder;
    _accountData = data;
    _holdsMessages  = (_folder.getType() & Folder.HOLDS_MESSAGES) > 0;
  }


  public void flush()
  {
    _messageListModel = null;
  }

  
  /**
   * Returns true if the folder can hold messages.
   */
  public boolean isHoldsMessages()
  {
    return _holdsMessages;
  }

  /**
   * @return the number of messages in this folder
   */
  public int getMessageCount() throws MessagingException
  {
    // remember, we can only get messages if the type of this folder is
    // Folder.HOLDS_MESSAGES
    if (_holdsMessages)
      return _folder.getMessageCount();

    return 0;
  }

  /**
   * @return the number of unread messages in this folder
   */
  public int getUnreadMessageCount() throws MessagingException
  {
    // remember, we can only get messages if the type of this folder is
    // Folder.HOLDS_MESSAGES
    if (_holdsMessages)
      return _folder.getUnreadMessageCount();

    return 0;
  }

  /**
   * @return the index into the list of messages that was last
   *  used by the messages table.  Stored here to keep it
   *  scoped to the folder, instead of the session.
   */
  public int getStartIndex()
  {
    return _startIndex;
  }
  
  /**
   * Store the index into the list of messages.
   */
  public void setStartIndex(int startIndex)
  {
    _startIndex = startIndex;
  }

  /**
   * gets the name of this folder
   */
  public String getName()
  {
    return _folder.getName();
  }

  /**
   * gets the full name of this folder. This reflects the hierarchy of this
   * folder.
   */
  public String getFullName()
  {
    return _folder.getFullName();
  }

  /**
   * @return true if this folder is currently selected
   */
  /* =-=AEW Not used
  public boolean isSelected()
  {
    return _folder.getFullName().equals
      (_accountData.getCurrentFolder()._folder.getFullName());
  }
  */

  /**
   * gets this folder's subfolders
   * @todo Why does this code return "null" instead of the empty list???
   */
  @SuppressWarnings("unchecked")
  public synchronized List<Object> getSubFolders() throws MessagingException
  {
    if (_subFolders == Collections.EMPTY_LIST)
    {
      return null;
    }
    else if (_subFolders == null)
    {
      FolderData[] folders = toFolderData(_accountData, _folder.list());
      if (folders == null)
      {
        _subFolders = Collections.EMPTY_LIST;
        return null;
      }
      else
      {
        _subFolders = Arrays.asList((Object[]) folders);
      }
    }

    return _subFolders;
  }

  /**
   * Get the model for the messages in this folder.
   */
  public Object getMessageListModel()
  {
    if (_holdsMessages)
    {
      if (_messageListModel == null)
      {
        FetchProfile fetchProfile = new FetchProfile();
        fetchProfile.add(FetchProfile.Item.ENVELOPE);
        fetchProfile.add(FetchProfile.Item.FLAGS);
        _messageListModel = new MessageDataModel(_folder,
                                                 fetchProfile,
                                                 _MESSAGE_LOAD_BLOCK_SIZE);
      }

      return _messageListModel;
    }
    else
    {
      return null;
    }
  }

  /**
   * converts {@link Folder}s to {@link FolderData}s.
   */
  public static FolderData[] toFolderData(AccountData data, Folder[] folders)
    throws MessagingException
  {
    int sz = folders.length;
    if (sz > 0)
    {
      FolderData[] subs = new FolderData[sz];
      for(int i=0; i<sz; i++)
      {
        Folder f = folders[i];
        subs[i] = new FolderData(data, f);
      }

      return subs;
    }

    return null;
  }

  /**
   * Get the underlying Folder object.
   */
  public Folder getFolder()
  {
    return _folder;
  }

  /**
   * Action for viewing the messages in this folder.
   */
  public String viewMessages()
  {
    // update the currentFolder on the account to point to this folder
    _accountData.setCurrentFolder(this);
	
    _LOG.log(Level.FINE, 
             "Showing messages for folder named {0} ", getName());
  
    return "messages";
  }

  private List<Object>      _subFolders = null;
  private DataModel         _messageListModel = null;
  private final Folder      _folder;
  private final AccountData _accountData;
  private final boolean     _holdsMessages;
  private       int         _startIndex;
  // Load 100 messages at a time (obviously, should be tuneable)
  private static final int _MESSAGE_LOAD_BLOCK_SIZE = 100;

  static private final Logger _LOG =
    Logger.getLogger(FolderData.class.getName());
}
