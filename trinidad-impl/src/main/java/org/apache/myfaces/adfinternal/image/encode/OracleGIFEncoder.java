/*
 * Copyright 2006 The Apache Software Foundation.
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
 

package org.apache.myfaces.adfinternal.image.encode;

import java.awt.Image;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.myfaces.adf.logging.ADFLogger;

/**
 * ImageEncoder implementation for the Oracle GIF encoder.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/OracleGIFEncoder.java#0 $) $Date: 10-nov-2005.19:05:20 $
 * @author The Oracle ADF Faces Team
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
      _LOG.info("Elapsed time:" + (System.currentTimeMillis()-_lastTime) / 1000.0 + " seconds" +
                " secs to encode gif");

    }
  }

  private static final boolean _REPORT_TIMING = false;
  private long _lastTime=0;
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(OracleGIFEncoder.class);
}
