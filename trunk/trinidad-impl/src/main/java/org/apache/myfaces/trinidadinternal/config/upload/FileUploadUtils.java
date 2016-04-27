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

import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;

/**
 * Set of utilities which allows for future "plugability" of the FileUpload procedures.
 * This will allow other filters to be added to the trinidad implementation which may
 * provide better multi-form processing.
 * <p/>
 * This may eventualy become a good candidate for API.
 */
public class FileUploadUtils
{
  private FileUploadUtils()
  {
    super();
  }
  
  /**
   * Sets up a request for handling by the UploadedFileProcessor.  Currently this
   * copies a number of attributes from session to the request so that the
   * UploadedFileProcessor can function accordingly.
   * 
   * @param req
   */
  public static void setupRequest(HttpServletRequest req)
  {
    //Check for Session.  If we don't have one, there is no need to set up request
    //attributes
    HttpSession session = req.getSession(false);
    if(null != session)
    {
      for(String attribute:_COPIED_ATTRIBUTES)
      {
        req.setAttribute(attribute, session.getAttribute(attribute));
      }
    }
    
  }
  
  /**
   * Sets up a request for handling by the UploadedFileProcessor.  Currently this
   * copies a number of attributes from session to the request so that the
   * UploadedFileProcessor can function accordingly.
   * 
   * @param req
   */
  public static void setupRequest(ExternalContext ec)
  {
    //Check for Session.  If we don't have one, there is no need to set up request
    //attributes
    if(null != ec.getSession(false))
    {
      Map<String, Object> sessionMap = ec.getSessionMap();
      Map<String, Object> requestMap = ec.getRequestMap();
      
      for(String attribute:_COPIED_ATTRIBUTES)
      {
        requestMap.put(attribute, sessionMap.get(attribute));
      }
    }
  }
  
  /**
   * Marks a multipart form as having been processed.  This tells Trinidad's file 
   * upload procedures that the multipart form processing has already been handled.
   * There is still some post processing that may be performed, but it is assumed
   * that the request and externalContext already has needed parameters and that
   * the Uploaded files have been added using {@link #addFile}.
   * 
   * @param req
   */
  public static void markProcessed(HttpServletRequest req)
  {
    req.setAttribute(_PROCESSED_PARAM, Boolean.TRUE);
  }
  
  /**
   * Marks a multipart form as having been processed.  This tells Trinidad's file 
   * upload procedures that the multipart form processing has already been handled.
   * There is still some post processing that may be performed, but it is assumed
   * that the request and externalContext already has needed parameters and that
   * the Uploaded files have been added using {@link #addFile}.
   */ 
  public static void markProcessed(ExternalContext ec)
  {
    ec.getRequestMap().put(_PROCESSED_PARAM, Boolean.TRUE);
  }
  
  public static boolean isProcessed(HttpServletRequest req)
  {
    return Boolean.TRUE.equals(req.getAttribute(_PROCESSED_PARAM));
  }
  
  public static boolean isProcessed(ExternalContext ec)
  {
    return Boolean.TRUE.equals(ec.getRequestMap().get(_PROCESSED_PARAM));
  }
  
  /**
   * Adds a file to the UploadedFiles queue given a request
   * 
   * @param req
   * @param id
   * @param file
   */
  public static void addFile(HttpServletRequest req, String id, UploadedFile file)
  {
    if(null != file)
    {
      UploadedFiles files = UploadedFiles.getUploadedFiles(req);
      if( null == files )
      {
        files = new UploadedFiles(req);
      }
    
      __addFile(files, id, file);
    }
  }
  
  static void __addFile(UploadedFiles files, String id, UploadedFile file)
  {
    if (file != null)
    {
      // Store the file.
      files.__put(id, file);

      if (_LOG.isFine())
      {
        _LOG.fine("Uploaded file " + file.getFilename() + "(" +
            file.getLength() + " bytes) for ID " + id);
      }
    }
  }
  
  private static final String         _PROCESSED_PARAM = FileUploadUtils.class.getName() + ".PROCESSED";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileUploadUtils.class);
  private static final String[] _COPIED_ATTRIBUTES = 
  {
    UploadedFileProcessor.MAX_MEMORY_PARAM_NAME,
    UploadedFileProcessor.MAX_DISK_SPACE_PARAM_NAME,
    UploadedFileProcessor.TEMP_DIR_PARAM_NAME,
    UploadedFileProcessor.MAX_FILE_SIZE_PARAM_NAME,
    UploadedFileProcessor.MAX_CHUNK_SIZE_PARAM_NAME
  };
}
