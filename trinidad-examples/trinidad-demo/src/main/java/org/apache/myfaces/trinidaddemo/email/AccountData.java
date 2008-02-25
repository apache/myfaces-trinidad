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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.application.FacesMessage;

import javax.faces.context.FacesContext;

import javax.mail.Folder;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;

import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.TreeModel;

import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * Root bean which stores information and actions related to the email account
 *
 * @version 1.0
 */
public final class AccountData implements java.io.Serializable
{

  public AccountData()
  {
    _initializeFromProperties();
  }

  /**
   * Returns the user name.
   */
  public String getUsername()
  {
    return _username;
  }

  /**
   * Returns the domain.
   */
  public String getDomain()
  {
    return _domain;
  }

  /**
   * Returns the IMAP server.
   */
  public String getServer()
  {
    return _server;
  }

  /**
   * Returns the SMTP server.
   */
  public String getSmtpServer()
  {
    return _smtpServer;
  }

  /**
   * Returns the password for the current user.
   */
  public String getPassword()
  {
    return _password;
  }

  /**
   * Returns the JavaMail Store object.
   */
  public Store getStore()
  {
    return _store;
  }

  /**
   * Returns the currently selected folder.
   */
  public FolderData getCurrentFolder()
  {
    RequestContext afContext = RequestContext.getCurrentInstance();
    return (FolderData) afContext.getPageFlowScope().get("currentFolder");
  }

  /**
   * Returns the root folders.
   */
  public FolderData[] getRootFolders()
  {
    return _rootFolders;
  }

  /**
   * Returns the current user's preferences
   */
  public PreferencesData getPreferences()
  {
    return _preferences;
  }

  /**
   * Get the folder model used in the Trinidad tree component showing folders.
   */
  public TreeModel getFolderModel()
  {
    return _folderModel;
  }

  /**
   * Sets the user name.
   */
  public void setUsername(String userName)
  {
    _username = userName;
  }

  /**
   * Sets the domain for the current user.
   */
  public void setDomain(String domain)
  {
    _domain = domain;
  }

  /**
   * Sets the IMAP server for the current user.
   */
  public void setServer(String server)
  {
    _server = server;
  }

  /**
   * Sets the SMTP server.
   */
  public void setSmtpServer(String smtpServer)
  {
    _smtpServer = smtpServer;
  }

  /**
   * Sets the password for the current user.
   */
  public void setPassword(String password)
  {
    _password = password;
  }

  /**
   * Sets the Store.
   */
  public void setStore(Store store)
  {
    _store = store;
  }

  /**
   * Sets the current folder.
   */
  public void setCurrentFolder(FolderData folderData)
  {
    RequestContext afContext = RequestContext.getCurrentInstance();
    afContext.getPageFlowScope().put("currentFolder", folderData);
  }

  /**
   * Sets the root folders.
   */
  public void setRootFolders(FolderData[] rootFolders)
  {
    _rootFolders = rootFolders;

    List<Object> rootFolderList = Arrays.asList((Object[]) _rootFolders);

    TreeModel folderTreeModel = new ChildPropertyTreeModel(rootFolderList, "subFolders");
    setFolderModel(folderTreeModel);
  }

  /**
   * Sets the Preferences for the current user.
   */
   public void setPreferences(PreferencesData preferences)
     throws MessagingException
  {
    _preferences = preferences;

    // Keep the block size in sync with the current preferences
  // refreshPreferences(); ???
  }

  /**
   * Set the folder model used in the Trinidad tree component showing folders.
   */
  public void setFolderModel(TreeModel model)
  {
    _folderModel = model;
  }


  /**
   * Process login. Connect to server and move to folder display page.
   */
  public String login()
  {
    try
    {
      // login to the IMAP server
      Properties props = new Properties();
      Session session = Session.getInstance(props, null);
      Store store = session.getStore("imap");
      store.connect(_server, _username, _password);
      setStore(store);

      setRootFolders(FolderData.toFolderData(this, store.getDefaultFolder().list()));

      // TODO: Add logged in indicator to restrict access

      _gotoFolder(null);

      // Set up the user's preferences;  in a real app, these would
      // be persisted somewhere.
      PreferencesData preferences = new PreferencesData();
      setPreferences(preferences);
    }
    // catch all exceptions and report them as errors
    catch (Exception e)
    {
      FacesMessage errorMessage = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                                                   e.getMessage(), null);

      FacesContext context = FacesContext.getCurrentInstance();
    context.addMessage(null, errorMessage);

      return null;
    }

    return "success";
  }

  /**
   * Log out, and close up everything.
   */
  public String logout()
  {
    destroy();

    setCurrentFolder(null);

    _password = null;
    _folderModel = null;
    _rootFolders = null;
    _preferences = null;

    return "loggedOut";
  }

  /**
   * Frees up resources used by this object.
   */
  public synchronized void destroy()
  {
    if (_store != null)
    {
      try
      {
        _store.close();
        _store = null;
      }
      catch (MessagingException e)
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * Clean up resources
   */
  @Override
  protected void finalize() throws Throwable
  {
    destroy();
    super.finalize();
  }

  /**
   * Go to a specified folder.
   * @param newFolder the new folder to view. Maybe null, in which case INBOX
   * will be opened.
   */
  private void _gotoFolder(String newFolder)
    throws MessagingException
  {
    if (newFolder==null)
      newFolder = "INBOX";

    Folder folder  = getStore().getFolder(newFolder);
    FolderData fdata = new FolderData(this, folder);
    setCurrentFolder(fdata);
  }

  // Initialize everything (but the password!) from a .properties file
  // for convenience
  private void _initializeFromProperties()
  {
    String home = System.getProperty("user.home");
    File file = new File(home, "trinidad-email-demo.properties");
    _LOG.log(Level.FINE, "Loading properties from {0}", file);
    try
    {
      InputStream stream = new FileInputStream(file);
      Properties properties = new Properties();
      properties.load(stream);

      setUsername(properties.getProperty("username"));
      setDomain(properties.getProperty("domain"));
      setServer(properties.getProperty("server"));
      String smtp = properties.getProperty("smtpserver");
      if (smtp == null)
        smtp = System.getProperty("mail.smtp.host");
      setSmtpServer(properties.getProperty("smtpserver"));
    }
    catch (IOException ioe)
    {
      // The file doesn't have to be there.
      ;
    }
  }

  // No real attempt to support failover here, but just trying
  // not to fail a Serialization test
  private String _username, _password, _domain, _server;
  private String _smtpServer;
  private transient Store _store;
  private transient TreeModel _folderModel;
  private transient FolderData[] _rootFolders;
  private transient PreferencesData _preferences;      // User preferences

  static private final Logger _LOG =
    Logger.getLogger(AccountData.class.getName());
}
