/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.config.upload;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.portlet.PortletRequest;

import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;


/**
 * UploadedFiles defines the set of files that have been uploaded
 * to the server.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/UploadedFiles.java#0 $) $Date: 10-nov-2005.18:49:05 $
 */
final public class UploadedFiles
{
  /**
   * Returns the map of uploaded files for the current request.
   */
  @SuppressWarnings("unchecked")
  static public UploadedFiles getUploadedFiles(FacesContext context)
  {
    return getUploadedFiles(context.getExternalContext());
  }
  
  /**
   * Returns the map of uploaded files for the current request.
   */
  @SuppressWarnings("unchecked")
  static public UploadedFiles getUploadedFiles(ExternalContext context)
  {
    Map<String, Object> requestMap = context.getRequestMap();
    return (UploadedFiles) requestMap.get(_UPLOADED_FILES_KEY);
  }
  
  /**
   * Returns the map of uploaded files for the current request.
   */
  @SuppressWarnings("unchecked")
  static public UploadedFiles getSessionUploadedFiles(FacesContext context)
  {
    return getSessionUploadedFiles(context.getExternalContext());
  }
  
  /**
   * Returns the map of uploaded files for the current session.
   */
  @SuppressWarnings("unchecked")
  static public UploadedFiles getSessionUploadedFiles(ExternalContext context)
  {
    Map<String, Object> sessionMap = context.getSessionMap();
    UploadedFiles files = (UploadedFiles) sessionMap.get(_UPLOADED_FILES_KEY);
    if (files == null)
    {
      files = new UploadedFiles();
      sessionMap.put(_UPLOADED_FILES_KEY, files);
    }
    return files;
  }

  /**
   * Store the character encoding for the current request.
   */
  static public void setCharacterEncoding(
    ExternalContext externalContext,
    String         encoding)
  {
    UploadedFiles files = getUploadedFiles(externalContext);
    _setCharacterEncoding(files, encoding);
  }

  static public void setCharacterEncoding(
      HttpServletRequest req,
      String encoding)
  {
    UploadedFiles files = (UploadedFiles)
      req.getAttribute(_UPLOADED_FILES_KEY);
    _setCharacterEncoding(files, encoding);
  }

  static public void setCharacterEncoding(
      PortletRequest req,
      String encoding)
  {
    UploadedFiles files = (UploadedFiles)
      req.getAttribute(_UPLOADED_FILES_KEY);
    _setCharacterEncoding(files, encoding);
  }
  
  static private void _setCharacterEncoding(UploadedFiles files, String encoding)
  {
    if(files != null)
    {
      files._characterEncoding = encoding;
    }
  }

  /**
   * Returns a single uploaded file.
   * @param name the name under which the file is stored.  In HTML forms,
   *   this will be derived from the "name" set on the &lt;input&gt; tag.
   */
  public UploadedFile getUploadedFile(String name)
  {
    List<UploadedFile> files = getUploadedFileList(name);
    if (files == null || files.isEmpty())
      return null;

    for (UploadedFile file : files)
    {
      if (file != null)
      {
        return file;
      }
    }
    
    return null;
  }

  /**
   * Returns a list of uploaded files.
   * @param name the name under which the files are stored.  In HTML forms,
   *   this will be derived from the "name" set on the &lt;input&gt; tag.
   */
  public List<UploadedFile> getUploadedFileList(String name)
  {
    List<UploadedFile> files = _map.get(name);
    if (files == null || files.isEmpty())
      return null;
    
    return files;
  }

  /**
   * Retrieves files uploaded during a multiple file upload and copies those
   * files to the requestMap so they get processed and cleaned up as usual
   * @param name the name under which the files are stored.  In HTML forms,
   *   this will be derived from the "name" set on the &lt;input&gt; tag.
   */
  public static void retrieveSessionUploadedFiles(ExternalContext context, String name)
  {
    UploadedFiles sessionFiles = (UploadedFiles) context.getSessionMap().get(_UPLOADED_FILES_KEY);
    if (sessionFiles != null)
    {
      List<UploadedFile> sessionFileList = sessionFiles.getUploadedFileList(name);
      if (sessionFileList != null && !sessionFileList.isEmpty())
      {
        UploadedFiles requestFiles = (UploadedFiles) context.getRequestMap().get(_UPLOADED_FILES_KEY);
        if (requestFiles == null)
        {
          requestFiles = new UploadedFiles(context);
        }
        for (UploadedFile sessionFile: sessionFileList)
        {
          requestFiles.__put(name, sessionFile);
        }
        // clear it in the sessionMap
        sessionFiles.getUploadedFileMap().remove(name);
      }
    }
  }

  /**
   * Returns an Iterator of the names of all uploaded files.
   */
  public Iterator<String> getUploadedNames()
  {
    return _map.keySet().iterator();
  }
  
  /**
   * Returns an Map of all of the uploaded files and names
   */
  public Map<String, List<UploadedFile>> getUploadedFileMap()
  {
    return _map;
  }

  /**
   * Dispose of all UploadedFiles.  This will happen automatically
   * when the current request ends, so clients do not need to
   * call this method.  However, if a developer is finished with
   * processing files, this will free up resources earlier.
   */
  public void dispose()
  {    
    Iterator<List<UploadedFile>> iterator = _map.values().iterator();
    while (iterator.hasNext())
    {
      List<UploadedFile> files = iterator.next();
      if (files != null)
      {
        for (UploadedFile file: files)
        {
          if (file != null)
          {
            file.dispose();
          }
        }
      }
    }

    _map.clear();

    _totalMemory    = 0;
    _totalDiskSpace = 0;
  }
  
  /**
   * Creates an UploadedFiles.
   */
  @SuppressWarnings("unchecked")
  UploadedFiles(ExternalContext externalContext)
  {
    _map = new HashMap<String, List<UploadedFile>>();
    externalContext.getRequestMap().put(_UPLOADED_FILES_KEY, this);
  }
  
  /**
   * Creates an UploadedFiles.
   */
  UploadedFiles()
  {
    _map = new HashMap<String, List<UploadedFile>>();
  }
  
  /**
   * Store a single UploadedFile.
   */
  void __put(String name, UploadedFile file)
  {
    List<UploadedFile> files = _map.get(name);
    if (files == null)
    {
      files = new ArrayList<UploadedFile>();
    }
    files.add(new FixFilename(file, _characterEncoding));
    _map.put(name, files);
  }

  /**
   * Return the tally of total memory used.
   */
  public long getTotalMemory()
  {
    return _totalMemory;
  }


  /**
   * Return the tally of total disk space used.
   */
  public long getTotalDiskSpace()
  {
    return _totalDiskSpace;
  }

  private long   _totalMemory;
  private long   _totalDiskSpace;
  private String _characterEncoding;
  private final Map<String, List<UploadedFile>> _map;

  private static final String _UPLOADED_FILES_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.UploadedFiles";

  static public class FixFilename implements UploadedFile, Serializable
  {
    /**
     *
     */
    private static final long serialVersionUID = -8586594511769079566L;

    public FixFilename()
    {
      // For serialization
    }

    public FixFilename(UploadedFile file, String encoding)
    {
      _file = file;
      _encoding = encoding;
    }

    public String getFilename()
    {
      String filename = _file.getFilename();
      if (_encoding == null)
        return filename;

      try
      {
        return CaboHttpUtils.decodeRequestParameter(filename,
                                                    _encoding,
                                                    null);
      }
      catch (UnsupportedEncodingException uee)
      {
        // Should never happen, since the encoding should have
        // already been vetted in UploadedRequestWrapper
        assert false;
        return filename;
      }
    }

    public String getContentType()
    {
      return _file.getContentType();
    }

    public long getLength()
    {
      return _file.getLength();
    }

    public Object getOpaqueData()
    {
      return _file.getOpaqueData();
    }

    public InputStream getInputStream() throws IOException
    {
      return _file.getInputStream();
    }


    public void dispose()
    {
      _file.dispose();
    }

    private UploadedFile _file;
    private String       _encoding;
  }
}
