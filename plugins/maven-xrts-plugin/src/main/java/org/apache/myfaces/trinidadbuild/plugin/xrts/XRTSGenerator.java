/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadbuild.plugin.xrts;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.xml.parsers.SAXParser;

import org.xml.sax.InputSource;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * The <code>XRTSGenerator</code> class is a online facility (as opposed to the
 * command line utility <code>XRTSMakeBundle</code>) for parsing and building
 * XML-based RTS files to the given implementation output. <p>
 *
 * @version $Name:  $ ($Revision: 1.1 $) $Date: 2001/07/09 18:15:53 $
 * @author Craig R. Cummings
 * @since RTS 2.0
 */
public final class XRTSGenerator
{

  /**
   * Parse the XML-based RTS source and create output according to an
   * <code>RTSWriter</code> implementation.
   *
   * @param parser see class description.
   * @param is the XML-based RTS source file.
   * @param rtsw an <code>RTSWriter</code> implementation.
   * @param parms a <code>Dictionary</code> of command line parameters.
   *
   */
  public static void generate(SAXParser parser, InputSource is, RTSWriter rtsw,
    Dictionary parms) throws Throwable
  {
    XRTSParser rtsp = new XRTSParser(rtsw, parms);

    try
    {
      parser.parse(is, rtsp);
    }
    catch (IOException ioe)
    {
      System.err.println(ioe);
      System.exit(1);
    }
    catch (SAXParseException spe)
    {
      System.exit(1);
    }
    catch (SAXException se)
    {
      System.err.println(se);
      System.exit(1);
    }
  }

  //
  // No need to instantiate this class
  //
  private XRTSGenerator()
  {
  }
}
