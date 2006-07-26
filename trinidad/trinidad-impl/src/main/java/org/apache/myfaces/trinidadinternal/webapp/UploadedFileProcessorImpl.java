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
package org.apache.myfaces.adfinternal.webapp;

import java.io.File;
import java.io.IOException;
import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import org.apache.myfaces.adf.model.UploadedFile;
import org.apache.myfaces.adf.webapp.UploadedFileProcessor;

public class UploadedFileProcessorImpl implements UploadedFileProcessor
{
  public UploadedFileProcessorImpl()
  {
  }

  public void init(Object c)
  {
    if (!(c instanceof ServletContext))
      return;

    ServletContext context = (ServletContext) c;
    //
    // Get MaxMemory and TempDir properties from servlet init params
    //
    if (_maxMemory == -1)
    {
      String maxMemory = context.getInitParameter(
                                 MAX_MEMORY_PARAM_NAME);
      if (maxMemory != null)
      {
        try
        {
          _maxMemory = Integer.parseInt(maxMemory);
        }
        catch (NumberFormatException nfe)
        {
          _maxMemory = _DEFAULT_MAX_MEMORY;
        }
      }
      else
      {
        _maxMemory = _DEFAULT_MAX_MEMORY;
      }
    }
      
    if (_maxDiskSpace == -1)
    {
      String maxDiskSpace = context.getInitParameter(
                                                  MAX_DISK_SPACE_PARAM_NAME);
      if (maxDiskSpace != null)
      {
        try
        {
          _maxDiskSpace = Integer.parseInt(maxDiskSpace);
        }
        catch (NumberFormatException nfe)
        {
          _maxMemory = _DEFAULT_MAX_DISK_SPACE;
        }
      }
      else
      {
        _maxDiskSpace = _DEFAULT_MAX_DISK_SPACE;
      }
    }
    
    if (_tempDir == null)
    {
      _tempDir = context.getInitParameter(TEMP_DIR_PARAM_NAME);
      // Use the webapp temporary directory if the temporary directory
      // has not been explicitly set.
      if (_tempDir == null)
      {
        File tempDirFile = (File) 
          context.getAttribute("javax.servlet.context.tempdir");
        if (tempDirFile != null)
          _tempDir = tempDirFile.getAbsolutePath();
      }
    }
  }

  public UploadedFile processFile(Object       request,
                                  UploadedFile tempFile)
    throws IOException
  {
    RequestInfo info = _getRequestInfo(request);

    // Process one new file, loading only as much as can fit
    // in the remaining memory and disk space.
    UploadedFileImpl file = new UploadedFileImpl();
    file.loadFile(tempFile,
                  _maxMemory - info.totalBytesInMemory,
                  _maxDiskSpace - info.totalBytesOnDisk,
                  _tempDir);

    // Keep a tally of how much we've stored in memory and on disk.
    long length = file.getLength();
    if (file.__isInMemory())
    {
      info.totalBytesInMemory += length;
    }
    else
    {
      info.totalBytesOnDisk += length;
    }

    return file;
  }

  private RequestInfo _getRequestInfo(Object r)
  {
    if (!(r instanceof ServletRequest))
      throw new IllegalArgumentException(
        "Only servlet-based file upload is supported");

    ServletRequest request = (ServletRequest) r;
    RequestInfo info = (RequestInfo) request.getAttribute(_REQUEST_INFO_KEY);
    if (info == null)
    {
      info = new RequestInfo();
      request.setAttribute(_REQUEST_INFO_KEY, info);
    }
    
    return info;
  }


  static private class RequestInfo
  {
    public long totalBytesInMemory;
    public long totalBytesOnDisk;
  }

  private long   _maxMemory = -1;
  private long   _maxDiskSpace = -1;
  private String _tempDir = null;

  private static final long _DEFAULT_MAX_MEMORY = 102400;
  private static final long _DEFAULT_MAX_DISK_SPACE = 2048000;

  private static final String _REQUEST_INFO_KEY = 
    "org.apache.myfaces.adfinternal.webapp.UploadedFilesInfo";

}
