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
package org.apache.myfaces.adfbuild.plugin.xrts;

import java.util.Dictionary;
import java.util.Enumeration;

/**
 * The <code>RTSWriter</code> interface defines methods for writing the
 * header, body, and footer of various output implementations. <p>
 *
 * The methods <code>startBundle</code> and <code>endBundle</code> write the
 * header and footer portions to the output.  The methods for writing the
 * body are <code>writeString</code>, <code>writeStringArray</code>, and
 * <code>writeDictionary</code>. These methods are supplied with data parsed
 * by the XML-based RTS file <code>XRTSParser</code> or the
 * <code>RTSProperties</code> parser.<p>
 *
 * RTS provides two basic implementations of RTSWriter --
 * <code>ListRTSWriter</code> and <code>DefaultSubkeyRTSWriter</code>. <p>
 *
 * <code>ListRTSWriter</code> writes parsed data into a Sun
 * <code>ListResourceBundle</code> file.  It utilizes the <code>writeString</code>
 * and <code>writeStringArray</code> methods.  Data stored in Dictionary format
 * cannot be processed by <code>ListResourceBundle</code>s and
 * <code>ListRTSWriter</code> will throw an exception. <p>
 *
 * Those consumers with a requirement to store and process
 * <code>Dictionary</code> format information should use
 * <code>DefaultSubkeyResourceBundle</code>. <code>DefaultSubkeyRTSWriter</code>
 * writes parsed data to an Oracle RTS <code>DefaultSubkeyResourceBundle</code>
 * file. <code>DefaultSubkeyResourceBundle</code> is capable of storing not only
 * <code>String</code> and <code>String</code> array data as
 * <code>ListResourceBundle</code>, but also <code>Dictionary</code> data that
 * is stored as subkey/value pairs as well. <p>
 *
 * These two implementations of <code>RTSWriter</code> provided by RTS only
 * write Java output files.  However, <code>RTSWriter</code> is an extremely
 * powerful interface and is designed to support the output of any file type.
 * That is, RTS consumers can write their own implementations of
 * <code>RTSWriter</code> to output files type such as HTML, XSL, CSS, XML, etc.
 * Moreover, the output of <code>RTSWriter</code> is not limited to just
 * files.  For example, <code>RTSWriter</code> could be used to implement
 * writing parsed data to the database using SQLJ, JDBC, or even a SQL loader.
 * <p>
 *
 * Consumers developing their own implementations of <code>RTSWriter</code> will
 * need to make sure that the implementation is registered in the Config.txt
 * file.  By default, Config.txt contains registrations for the list and subkey
 * implementations.  The format of Config.txt matches that of a text-based RTS
 * file.  That is, a key, followed by an equal sign '=', followed by the value.
 * The key is the command line parameter that XXXMakeBundle calls.  The value is
 * the fully qualified class name of the <code>RTSWriter</code> implementation.
 *  The following is the text contained in the default Config.txt:
 * <blockquote>
 * <pre>
 * subkey = oracle.bali.rts.tools.DefaultSubkeyRTSWriter
 * list = oracle.bali.rts.tools.ListRTSWriter
 * </pre>
 * </blockquote>
 *
 * Each method in this interface includes a <code>parms</code> and
 * <code>meta Dictionary</code>.  The <code>parms dictionary</code> contains
 * <code>String</code> keys and values representing the command line parameters
 * for the make tool. <p>
 *
 * In this release, the <code>meta Dictionary</code> is not fully utilized.  It
 * is intended to contain information that contained in the XML-based RTS source
 * file, but external to the resource element information.  The final release of
 * RTS 2.0 will better define this structure's contents.
 *
 * @see ListRTSWriter
 * @see DefaultSubkeyRTSWriter
 * @see RTSMakeBundle
 * @see XRTSMakeBundle
 *
 * @version $Name:  $ ($Revision: 1.7 $) $Date: 2001/04/09 23:22:22 $
 * @author Craig R. Cummings (crcummin@us.oracle.com)
 * @since RTS 1.5
 */
public interface RTSWriter
{

  /**
   * Writes the <file or other output header(s).
   *
   * @param parms a <code>Dictionary</code> of command line parameters.
   * @param meta a <code>Dictionary</code> of parsed non-resource data
   * (e.g., package).
   */
  public void startBundle(Dictionary parms, Dictionary meta)
    throws Throwable;

  /**
   * Writes a <code>String</code> to the implementation output. <p>
   *
   * This is the typical case use.  A single <code>String</code> key associated
   * with a single <code>String</code> value forming a key/value pair (kvp).
   * Note that the other two body methods, <code>writeStringArray</code> and
   * <code>writeDictionary</code> both support a single <code>String</code>
   * key associated with multiple values. <p>
   *
   * A key/value pair may have associated attributes in the RTS file.
   * In this method, these are received from the XML parser as a
   * <code>Dictionary</code>. <p>
   *
   * @param parms a <code>Dictionary</code> of command line parameters
   * @param meta a <code>Dictionary</code> of parsed non-resource data
   * (e.g., package).
   * @param key a <code>String</code> key.
   * @param value a <code>String</code> value.
   * @param attr a <code>Dictionary</code> containing any XML attributes
   * assciated with the resource element.
   *
   * The <code>attr Dictionary</code> contains a <code>String</code> key which
   * is the attribute name and a <code>String</code> value for the given
   * attribute name (typically the text in the source RTS file that
   * appears to the right of the equals '=' sign of an attribute key).
   */
  public void writeString(Dictionary parms, Dictionary meta, String key,
    String value, Dictionary attr) throws Throwable;

  /**
   * Writes an array of <code>String</code> to the implementation body output.
   * <p>
   *
   * If the <code>String</code> array <code>kvpArr</code> has n values then the
   * array of <code>Dictionary attrs</code> will have n+1 values.  Each
   * <code>Dictionary</code> in the array with an index from 0-n will be
   * the attributes for the <code>String</code> in the <code>String</code> array
   * of the same index.  The <code>Dictionary</code> of attributes at index n+1
   * will be that of the 'values' element from the RTS file. <p>
   *
   * @param parms a <code>Dictionary</code> of command line parameters
   * @param meta a <code>Dictionary</code> of parsed non-resource data
   * (e.g., package).
   * @param key a <code>String</code> key.
   * @param kvpArr an array of <code>String</code> values
   * pairs.
   * @param attrs an array of <code>Dictionary</code> where each
   * <code>Dictionary</code> contains any XML attributes assciated with a
   * given <code>String</code> in the kvpArr array.
   *
   * The contents of each attribute <code>Dictionary</code> in the array is
   * a <code>String</code> key which is the attribute name and a value which is
   * the <code>String</code> value for the given attribute name (typically the
   * text that appears to the right of the equals '=' sign of an attribute key.
   */
  public void writeStringArray(Dictionary parms, Dictionary meta, String key,
    String[] kvpArr, Dictionary[] attrs) throws Throwable;

  /**
   * Writes a <code>Dictionary</code> to the implementation body output. <p>
   *
   * If the <code>Dictionary kvps</code> contains subkeys which are
   * <code>String</code> element names of the child elements of 'set' in the
   * RTS file.  The <code>Dictionary kvps</code> also contains values
   * associated the the various subkeys which are <code>String</code> values
   * from the XML file -- the string that appears between child start and end
   * elements. <p>
   *
   * @param parms a <code>Dictionary</code> of command line parameters.
   * @param meta a <code>Dictionary</code> of parsed non-resource data
   * (e.g., package).
   * @param key a <code>String</code> key.
   * @param kvps a <code>Dictionary</code> with ordered subkey/
   * <code>String</code> value pairs.
   * @param attrs a <code>Dictionary</code> of XML attributes assciated with a
   * given subkey. 
   *
   * The contents of the <code>attrs Dictionary</code> are keys that match the
   * name of the child elements or subkey.  The values of the
   * <code>attrs Dictionary</code> are <code>Dictionary</code> objects
   * themselves.  These nested Dictionary value objects contain key/value pairs.
   * The key is a <code>String</code> attribute name and the value is a
   * <code>String</code> the value for the given attribute name.
   *
   * Typical use cases will probably not require attributes for any of
   * the child elements.  Just the same, RTS is able to support attributes for
   * these subkey/child elements.
   */
  public void writeDictionary(Dictionary parms, Dictionary meta, String key,
    Dictionary kvps, Dictionary attrs) throws Throwable;

  /**
   * Writes the file or other output footer(s).
   *
   * @param parms a <code>Dictionary</code> of command line parameters.
   * @param meta a <code>Dictionary</code> of parsed non-resource data
   * (e.g., package).
   */
  public void endBundle(Dictionary parms, Dictionary meta) throws Throwable;

}
