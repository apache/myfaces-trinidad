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

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;

import org.xml.sax.AttributeList;
import org.xml.sax.HandlerBase;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 *
 * Data stored in the XML-based RTS format is parsed by <code>XRTSParser</code>.
 * The data in the resource elements is parsed into a Vector of Tuples.  The
 * data for other elements that do not have &lt;resource&gt; as their element
 * name (e.g., &lt;authors&gt;) are parsed into a <code>Hashtable</code> of
 * <code>Tuple</code>s. <p>
 *
 * The key attribute of the resource element is set to the key of the Tuple.
 * Any other attributes
 * for the resource element are set in the attribute Hashtable for the Tuple.
 * The translatable value of the resource element is the text between the start
 * of the element and the end of the element.  If the value is simply a String,
 * then it is set in the value of the Tuple.  If the value is a String array,
 * the value of the Tuple is set to a Vector of tuples that are built through
 * subsequent parsing.  If the value is a Dictionary, then the value of the
 * tuple is set to a Dictionary of Tuples that are built through subsequent
 * parsing. <p>
 *
 * @version $Name:  $ ($Revision: 1.1 $) $Date: 2001/04/09 23:23:22 $
 * @author Craig R. Cummings
 */
final class XRTSParser extends HandlerBase
{

  /**
   * Empty constructor for the XRTSParser
   *
   */
  public XRTSParser()
  {
  }

  public XRTSParser(RTSWriter bw, Dictionary parms)
  {
    _bundleWriter = bw;
    _parms = (Hashtable)parms;
  }

  /**
   * Receive a Locator object for document events.
   * Setting Locator ensures the SAX parser can find a given XML-based RTS
   * document.  This method is closely related to the _fileToURL in
   * <code>XRTSMakeBundle</code>.
   *
   * @param locator A locator for all SAX document events.
   */
  public void setDocumentLocator (Locator locator)
  {
    _locator = locator;
  }

  /**
   * Receive notification of the beginning of the document.
   * SAX Parser event for finding the beginning of an XML-based RTS document
   */
  public void startDocument() throws SAXException
  {
//    System.out.println("StartDocument");
    _xmlTagStack = new Stack();
    if (_metaHt == null)
      _metaHt = new Hashtable();
    _metaHt.put("fileType", "xrts");
    _startDoc = true;

  }

  /**
   * Receive notification of the end of the document.
   * SAX Parser event for finding the end of an XML-based RTS document
   */
  public void endDocument() throws SAXException
  {
//    System.out.println("EndDocument");
    try
    {
      _bundleWriter.endBundle(_parms, _metaHt);
    }
    catch (Throwable th)
    {
      throw new SAXException("endBundle Exception: " + th.getMessage());
    }
  }

  /**
   * Receive notification of the start of an element.
   *
   * @param name the element type name.
   * @param attributes the specified or defaulted attributes.
   */
  public void startElement(String name, AttributeList atts) throws SAXException
  {
//    System.out.println("StartElement:"+name);
    Hashtable _attsHt = new Hashtable();
    _nestingLevel++;

    if (name.equals("values"))
      _inValues = true;
    else if (name.equals("set"))
      _inSet = true;

    // startBundle doesn't go in the startDocument method because it
    // requires metadata to be passed and in particular, requires
    // at least the package for any implementation of startBundle to work
    else if (name.equals("resource") && _startDoc)
      try
      {
        _bundleWriter.startBundle(_parms, _metaHt);
        _startDoc = false;
      }
      catch (Throwable th)
      {
        throw new SAXException("startBundle Exception: " + th.getMessage());
      }

    if (name.equals("resources"))
    {
      if (_metaHt == null)
        _metaHt = new Hashtable();
      _metaHt.put("package", atts.getValue("package"));
      if (atts.getValue("version") != null)
        _metaHt.put("version", atts.getValue("version"));
      if (atts.getValue("baseversion") != null)
        _metaHt.put("baseVersion", atts.getValue("baseversion"));
    }

    if (atts.getLength() > 0)
      for (int i=0;i < atts.getLength(); i++)
        _attsHt.put(atts.getName(i), atts.getValue(i));

    if (name.equals("value") && _inSet)
    {
      System.err.println("String array structure (\'values/value\') not " +
        "allowed within Dictionary (\'set\') XML-based RTS structure.  Please" +
        "correct the XML-based RTS or DTD file and rebuild." +
        " Line:" + _locator.getLineNumber());
      System.exit(1);
    }
    else if (name.equals("set") && _inValues)
    {
      System.err.println("Dictionary structure (\'set\') not allowed within " +
        "String array (\'values/value\') XML-based RTS structure.  Please" +
        "correct the XML-based RTS or DTD file and rebuild." +
        " Line:" + _locator.getLineNumber());
      System.exit(1);
    }
    else
    {
      Tuple t = new Tuple();
      t.key = name;
      if (_attsHt.size() > 0)
        t.attributes = _attsHt;
      else
        t.attributes = null;
      t.nesting = _nestingLevel;
      _xmlTagStack.push(t);
    //  _printStack(_xmlTagStack);
    }
  }

  /**
   * Receive notification of the end of an element.
   * @param name the element type name.
   */
  public void endElement(String name) throws SAXException
  {
//    System.out.println("EndElement:"+name);

    String key = name;
    _nestingLevel--;

    // Vector of Tuples for "values" String array
    if (name.equals("values"))
    {
      Vector values = new Vector();
      Vector dicts = new Vector();

      Tuple t = (Tuple)_xmlTagStack.pop();
      while (!t.key.equals("values"))
      {
        if (t.value != null)
        {
          if (t.key.equals("value"))
          {
            if (t.nesting <= 4)
            {
              values.addElement(t.value);
              dicts.addElement(t.attributes);
            }
            else
            {
              System.err.println("Nesting within values not permitted." +
                " Exiting.");
              System.exit(1);
            }
          }
          else
          {
            System.err.println("Non value element \'" + t.key +
              "\' found. Discarding.");
          }
        }
        else
        {
          System.err.println("Null value found. Discarding.");
        }
        t = (Tuple)_xmlTagStack.pop();
      }
      // last element in the vector will always be the 'values' Tuple (with any
      // associated attributes e.g., DNT=)

      //      v.addElement(t.value);
      // need to save attributes Dictionary for 'values'

      // pop off the resource or metadata Tuple, set its value to the Vector,
      // and push it back on the stack

      t = (Tuple)_xmlTagStack.pop();
      // add the attributes for 'values' element itself
      dicts.addElement(t.attributes);

      if (t.key.equals("resource"))
      {
        key = (String)t.attributes.get("key");

        String[] strArr = new String[values.size()];
        int j = values.size()-1;
        for (int i = 0; i < values.size(); i++)
        {
          strArr[j] = (String)values.elementAt(i);
          j--;
        }

        Hashtable[] attsArr = new Hashtable[dicts.size()];
        j = dicts.size()-1;
        for (int i = 0; i < dicts.size(); i++)
        {
          attsArr[j] = (Hashtable)dicts.elementAt(i);
          j--;
        }

        if (_uniqKeys == null)
          _uniqKeys = new Hashtable();
        if (_uniqKeys.get(key) == null)
        {
          _uniqKeys.put(key, "OK");
        }
        else
        {
          System.err.println("Duplicate key found for " + key);
          System.exit(1);
        }

        try
        {
          _bundleWriter.writeStringArray(_parms, _metaHt, key, strArr, attsArr);
        }
        catch (Throwable th)
        {
          throw new SAXException("writeStringArray Exception: " +
            th.getMessage());
        }
      }
      else
      {
        key = t.key;
        //consider handling as meta data if enough demand
      }
      // debugging
//      _printStack(_xmlTagStack);
    }

    // Hashtable of Tuples for "set" String array
    else if (name.equals("set"))
    {
      Vector setVals = new Vector();
      Hashtable setAttrHt = new Hashtable();
      Tuple t = (Tuple)_xmlTagStack.pop();
      while (!t.key.equals("set"))
      {
        // a bit of redundancy here with Hashtable key = t.key and the Tuple
        // itself containing t.key again.  They could be converted to a pair
        // here with the loss of some performance.  Alternatively, the pair
        // could originate in the start element event through some detection,
        // but that code does not appear to be that trivial an effort.  Note
        // this duplication also occurs in the meta data handling.
        setVals.addElement(t);
        if (t.attributes != null)
          setAttrHt.put(t.key, t.attributes);
        t = (Tuple)_xmlTagStack.pop();
      }
      // pop off the resource or metadata Tuple, set its value to the Vector,
      // and push it back on the stack
      t = (Tuple)_xmlTagStack.pop();
      // add the attributes for the 'set' element itself
      setAttrHt.put(t.key, t.attributes);
      if (t.key.equals("resource"))
      {
        key = (String)t.attributes.get("key");

        OrderedDictionary setHt = new OrderedDictionary();
        for (int i = setVals.size() - 1; i >= 0; i--)
        {
          Tuple tVals = (Tuple)setVals.elementAt(i);
          if (tVals.nesting <= 4)
          {
            try
            {
              setHt.put(tVals.key, tVals.value);
            }
            catch (NullPointerException npe)
            {
              System.err.println("Value for subkey \'" + tVals.key +
                "\' is null. Discarding.");
            }
          }
          else
          {
            System.err.println("Nesting within set elements not permitted." +
              " Exiting.");
            System.exit(1);
          }
        }

        if (_uniqKeys == null)
          _uniqKeys = new Hashtable();
        if (_uniqKeys.get(key) == null)
        {
          _uniqKeys.put(key, "OK");
        }
        else
        {
          System.err.println("Duplicate key found for " + key);
          System.exit(1);
        }

        try
        {
          _bundleWriter.writeDictionary(_parms, _metaHt, key, setHt, setAttrHt);
        }
        catch (Throwable th)
        {
          throw new SAXException("writeDictionary Exception: " +
            th.getMessage());
        }
      }
      else
      {
        key = t.key;
        //consider handling as meta data if enough demand
      }


      // debugging
//      _printStack(_xmlTagStack);
    }
    else if (name.equals("author"))
    {
      if (_authors == null)
        _authors = new Vector();
      Tuple t = (Tuple)_xmlTagStack.pop();
      if (t.value != null)
      {
        _authors.addElement(t.value);
        _metaHt.put("authors", _authors);
      }
      else
      {
        System.err.println("Null author value.  Discarding.");
      }
    }
    else if (name.equals("resource"))
    {
//      _printStack(_xmlTagStack);
//      System.out.println(name + ":" + _inValues + ":" + _inSet);
      if (_inValues)
      {
        _inValues = false;
      }
      else if (_inSet)
      {
        _inSet = false;
      }

      else
      {
        Tuple t = (Tuple)_xmlTagStack.pop();
        try
        {
          Hashtable ht = t.attributes;
          key = (String)ht.get("key");

          if (_uniqKeys == null)
            _uniqKeys = new Hashtable();
          if (_uniqKeys.get(key) == null)
          {
            _uniqKeys.put(key, "OK");
          }
          else
          {
            System.err.println("Duplicate key found for " + key);
            System.exit(1);
          }

          if (t.value != null)
          _bundleWriter.writeString(_parms, _metaHt, key,
            (String)t.value, t.attributes);
          else
            System.err.println("Value for key \'" + key + "\' is null." +
              " Discarding.");
        }
        catch (Throwable th)
        {
          throw new SAXException("writeString Exception: " + th.getMessage());
        }
      }
    }

//    else
//    {
//      consider handling as meta data if enough demand
//    }
  }

  /**
   * Receive notification of ignorable whitespace in element content.
   *
   * @param ch the whitespace characters.
   * @param start the start position in the character array.
   * @param length the number of characters to use from the character array.
   */
  public void ignorableWhitespace(char[] cbuf, int start, int len)
    throws SAXException
  {
//    System.out.println("IgnorableWhiteSpace" + len);
  }

  /**
   * Receive notification of character data inside an element.
   *
   * @param ch the characters.
   * @param start the start position in the character array.
   * @param length the number of characters to use from the character array.
   */
  public void characters(char[] cbuf, int start, int len) throws SAXException
  {
//    System.out.println("Characters:" + len);

    // Keep track of when resource elements start so we can be sure to
    // collect and concatenate any initial whitespace for the value.  These are
    // the pitfalls of a validating parser and I was unable to find a way to
    // toggle separation of initial whitespace off.  Perhaps this could
    // eventually be an enhancement request to the Oracle XML parser team.

    // If it is text in a resource tag, concatenate it as it must be part of the
    // value string.  The concatenation is required with a validating parser as
    // the parser considers any leading spaces to be a string of characters
    // separate of the string consisting of non-space and any following space
    // characters
    Tuple t = (Tuple)_xmlTagStack.pop();
    String newStr = new String(cbuf,start,len);
    if (t.value instanceof String)
    {
      String tStr = (String)t.value;
      if (tStr != null && !tStr.equals(""))
        t.value = tStr.concat(newStr);
    }
    else
    {
      t.value = newStr;
    }

    _xmlTagStack.push(t);

  }

  /**
   * Receive notification of a parser warning.
   *
   * @param spe the warning information encoded as an exception.
   */
  public void warning(SAXParseException spe) throws SAXException
  {
    System.err.println(spe + " Line:" + spe.getLineNumber());
  }

  // validity
  /**
   * Receive notification of a recoverable parser error.
   *
   * @param spe the error information encoded as an exception.
   */
  public void error(SAXParseException spe) throws SAXException
  {
    System.err.println(spe + " Line:" + spe.getLineNumber());
  }

  // well-formedness
  /**
   * Receive notification of a fatal XML-based RTS parsing error.
   *
   * @param spe the fatal error information encoded as an exception.
   */
  public void fatalError(SAXParseException spe) throws SAXException
  {
    System.err.println(spe + " Line:" + spe.getLineNumber());
  }

  private final class Tuple
  {

    private String key;
    /**
     * The value is an <code>Object</code> that is either a <code>String</code>,
     * an array of <code>String</code>s (stored as a <code>Vector</code>), or a
     * <code>Dictionary</code> of key/value pairs where the values in the
     * <code>Dictionary</code> are <code>String</code>s.
     */
    private Object value;
    private Hashtable attributes;
    private int nesting;

    /**
     * Create and empty <code>Tuple</code> object.
     */
    private Tuple()
    {
    }

    /**
     * Create a <code>Tuple</code> object with key, value, attributes, and type.
     *
     * @param key name of key
     * @param value value <code>Object</code>
     * @param attributes typically a <code>Hashtable</code> of attributes
     * @param type see the field section for possible values
     */
    private Tuple(String key, Object value, Hashtable attributes, int nesting)
    {
      this.key = key;
      this.value = value;
      this.attributes = attributes;
      this.nesting = nesting;
    }
  }

  // For debugging purposes only
  private void _printStack(Stack s)
  {
    Enumeration e = s.elements();
    while (e.hasMoreElements())
    {
      Tuple t = (Tuple)e.nextElement();
      System.out.println(t.key);
      System.out.println("->" + t.value);
      System.out.println("->" + t.attributes);
    }
    System.out.println();
  }

  // Store the locator
  private Locator _locator;

  private Stack _xmlTagStack;
  private Hashtable _metaHt;
  private Hashtable _uniqKeys;
  private Vector _authors;

  private RTSWriter _bundleWriter;
  private Hashtable _parms;

  private int _nestingLevel = 0;
  private boolean _inValues = false;
  private boolean _inSet = false;
  private boolean _startDoc = false;
}
