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
package org.apache.myfaces.trinidad.model;

import java.io.InputStream;
import java.io.IOException;

/**
 * Interface that describes the contents of a single file.
 * 
 * @see org.apache.myfaces.trinidad.component.core.input.CoreInputFile
 */
public interface UploadedFile
{
  /**
   * Returns the filename reported by the client.
   */
  public String getFilename();

  /**
   * Returns the MIME type of the file.
   */
  public String getContentType();

  /**
   * Returns the total length (in bytes) of the file.
   */
  public long getLength();


  /**
   * Return opaque data associated with the file when it was
   * processed by the
   * {@link org.apache.myfaces.trinidad.webapp.UploadedFileProcessor}.
   * This is always null with the default <code>UploadedFileProcessor</code>,
   * but custom implementations may use this to pass additional information
   * needed for later processing.  (Note that Apache Trinidad cannot
   * cannot guarantee that the  instance returned from
   * code>UploadedFileProcessor</code> is the same one made available
   * later.
   */
  public Object getOpaqueData();

  /**
   * Returns an InputStream that can be used to read the file.
   * This method can be called repeatedly.
   */
  public InputStream getInputStream() throws IOException;

  /**
   * Disposes all resources allocated for this file.
   */
  public void dispose();
}
