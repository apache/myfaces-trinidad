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
package org.apache.myfaces.trinidad.util;

import java.io.File;
import java.io.IOException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * File-related utilities.
 */
public final class FileUtils
{
  /**
   * Converts the specified directory path to a File
   * instance representing an existing, writable directory.
   * 
   * This method will attempt to create a directory on the
   * file system if it does not already exist.
   * 
   * It does not, however, attempt to modify write permissions.
   * 
   * This method is thread safe.  However, there is no way to
   * prevent other threads from modifying the file system, which
   * can lead to unexpected results.  For example, toWritableDirectory
   * may return a File that it thinks is writable, but which has
   * actually been removed from the file system by another thread.
   * 
   * @param path the path to convert to a File
   * @return a File corresponding to the specified path
   * @throws IOException if directory at the specified path
   *   does not exist/cannnot be created, or is not writable.
   */
  public static File toWritableDirectory(String path)
    throws IOException
  {
    File directory = new File(path);
    
    synchronized (FileUtils.class)
    {
      if (!directory.exists()  && !directory.mkdirs())
      {
        _fail("CANNOT_CREATE_DIRECTORY", path);
      }
    }

    if (!directory.canWrite())
    {
      _fail("DIRECTORY_NOT_WRITABLE", path);
    }

    return directory;
  }
  
  private static void _fail(String messageKey, String path)
    throws IOException
  {
    String message = _LOG.getMessage(messageKey, path);
    throw new IOException(message);
  }

  private FileUtils()
  {
  }
  
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(FileUtils.class);  
}
