/*
 * Copyright  2002-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.webapp;

import java.io.InputStream;
import java.io.IOException;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.FacesContext;

import javax.servlet.ServletRequest;

import org.apache.myfaces.trinidad.model.UploadedFile;

import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;

/**
 * UploadedFiles defines the set of files that have been uploaded
 * to the server.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/UploadedFiles.java#0 $) $Date: 10-nov-2005.18:49:05 $
 * @author The Oracle ADF Faces Team
 */
final public class UploadedFiles
{
  /**
   * Returns the map of uploaded files for the current request.
   */
  @SuppressWarnings("unchecked")
  static public UploadedFiles getUploadedFiles(FacesContext context)
  {
    Map<String, Object> requestMap = 
      context.getExternalContext().getRequestMap();
    return (UploadedFiles) requestMap.get(_UPLOADED_FILES_KEY);
  }

  /**
   * Store the character encoding for the current request.
   */
  static public void setCharacterEncoding(
    ServletRequest request,
    String         encoding)
  {
    UploadedFiles files = (UploadedFiles)
      request.getAttribute(_UPLOADED_FILES_KEY);
    if (files != null)
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
    UploadedFile file = _map.get(name);
    if (file == null)
      return null;

    return new FixFilename(file, _characterEncoding);
  }


  /**
   * Returns an Iterator of the names of all uploaded files.
   */
  public Iterator<String> getUploadedNames()
  {
    return _map.keySet().iterator();
  }

  /**
   * Dispose of all UploadedFiles.  This will happen automatically
   * when the current request ends, so clients do not need to
   * call this method.  However, if a developer is finished with
   * processing files, this will free up resources earlier.
   */
  public void dispose()
  {
    Iterator<UploadedFile> iterator = _map.values().iterator();
    while (iterator.hasNext())
    {
      UploadedFile file = iterator.next();
      file.dispose();
    }

    _map.clear();

    _totalMemory    = 0;
    _totalDiskSpace = 0;
  }

  /**
   * Creates an UploadedFiles.
   */
  UploadedFiles(ServletRequest request)
  {
    request.setAttribute(_UPLOADED_FILES_KEY, this);
    _map = new HashMap<String, UploadedFile>();
  }

  /**
   * Store a single UploadedFile.
   */
  void __put(String name, UploadedFile file)
  {
    _map.put(name, file);
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
  private final Map<String, UploadedFile> _map;

  private static final String _UPLOADED_FILES_KEY = 
    "org.apache.myfaces.trinidadinternal.webapp.UploadedFiles";

  static public class FixFilename implements UploadedFile, Serializable
  {
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
