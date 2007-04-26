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
package org.apache.myfaces.trinidadinternal.image.encode;

import java.awt.Image;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ImageEncoder implementation for the Oracle GIF encoder.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/OracleGIFEncoder.java#0 $) $Date: 10-nov-2005.19:05:20 $
 */
public class OracleGIFEncoder implements ImageEncoder
{

  /**
   * Implementation of ImageEncoder.encodeImage()
   */
  public void encodeImage(Image image, OutputStream out)
    throws IOException
  {
    // We need to create a new GIFEncoder instance for each encoding
    if(_REPORT_TIMING)
    {
	   System.gc();
      _lastTime = System.currentTimeMillis();
    }

    GifEncoder.encode(image, out); // pass through the encoder

    if (_REPORT_TIMING && _LOG.isInfo())
    {
      // System.out.println(Timing.getElapsedString(_lastTime) +
      //                    " secs to encode gif");
      _LOG.info("ELAPSED_TIME_ENCODING_GIF", (System.currentTimeMillis()-_lastTime) / 1000.0);

    }
  }

  private static final boolean _REPORT_TIMING = false;
  private long _lastTime=0;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(OracleGIFEncoder.class);
}
