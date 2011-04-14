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
package org.apache.myfaces.trinidaddemo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.webapp.ChainedUploadedFileProcessor;

/**
 * This class checks if the uploaded file contains the word "crap" in it and
 * rejects if found.
 * 
 */
public class ProfanityScanner implements ChainedUploadedFileProcessor
{

  public void init(Object context)
  {
    _LOG.log(Level.INFO, "ProfanityScanner just got initialized...");
  }

  public UploadedFile processFile(Object request, UploadedFile file) throws IOException
  {
    _LOG.log(Level.INFO, "Scanning for profanity...");

    BufferedReader br = new BufferedReader(new InputStreamReader(file.getInputStream()));
    String line;
    try
    {
      while ((line = br.readLine()) != null)
      {
        if (line.indexOf("crap") != -1)
        {
          throw new IOException("ProfanityScanner rejected this file as " +
                  "it contained the word \"crap\" in it!!!\n" + request);
        }
      }
    }
    finally
    {
      br.close();
    }
    
    _LOG.log(Level.INFO, "Done Scanning for profanity...");
    
    /**
     * Since we did not change anything in the Inputstream we got from the parameter
     * its ok to return the same object. The file argument is backed by the buffer
     * hence subsequent processors will be able to access the stream again.
     */
    return file;
  }

  static private final Logger _LOG =
          Logger.getLogger(ProfanityScanner.class.getName());
}
