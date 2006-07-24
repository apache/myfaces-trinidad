/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.io;

import java.io.IOException;
import java.io.Writer;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Stack;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.logging.ADFLogger;



/**
 * ResponseWriter that decorates another and checks for common
 * mistakes in HTML output.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/io/DebugHtmlResponseWriter.java#1 $) $Date: 11-nov-2005.14:59:40 $
 * @author The Oracle ADF Faces Team
 */
public class DebugHtmlResponseWriter extends ResponseWriterDecorator
{
  /**
   * Creates a DebugHtmlResponseWriter.
   */
  public DebugHtmlResponseWriter(ResponseWriter decorated)
  {
    super(decorated);
    _elementStack = new Stack();
  }

  /**
   * Creates a new instance of this DebugHtmlResponseWriter, using a different
   * Writer.
   */
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    DebugHtmlResponseWriter cloned =
      new DebugHtmlResponseWriter(getResponseWriter().cloneWithWriter(writer));
    cloned._elementStack.addAll(_elementStack);
    return cloned;
  }



  public void startElement(String name, UIComponent component) throws IOException
  {
    String lowerName = name.toLowerCase();
    // check for nested <form> tag, nested <html> tag,
    // and nested <body> tag
    if ("form".equals(lowerName) ||
        "body".equals(lowerName) ||
        "html".equals(lowerName))
    {
      if(_elementStack.contains(lowerName))
        _errorWithComment("Illegal HTML: Cannot nest <" + lowerName + "> elements");
    }


    // Check for elements that are not allowed inside each other
    if (!_elementStack.empty())
    {
      String parent = (String) _elementStack.peek();
      Collection allowedParents = (Collection) _sAllowedParents.get(lowerName);
      Collection allowedChildren = (Collection) _sAllowedChildren.get(parent);
      if (((allowedParents != null)  &&
           !allowedParents.contains(parent)) ||
          ((allowedChildren != null)  &&
           // In practice, <script> can be embedded anywhere
           !"script".equals(lowerName) &&
           !allowedChildren.contains(lowerName)))
      {
        _errorWithComment("Illegal HTML: cannot put a <" + lowerName + "> element in " +
                          "a <" + parent + "> element.");
      }
    }

    _elementStack.push(lowerName);

    super.startElement(name, component);
  }


  public void endElement(String name) throws IOException
  {
    if (!_elementStack.empty())
      _elementStack.pop();

    super.endElement(name);
  }

  public void writeAttribute(String name,
                             Object value,
                             String componentPropertyName)
        throws IOException
  {
    if ("name".equals(name))
    {
      // =-=AEW. Review this. Try checking against a regular expression instead
      if ((value != null) &&
          value.toString().indexOf(' ') >= 0)
      {
        // Because of the fix for bug 2944473, in some rare
        // cases UIX generates spaces in the "name" attribute of anchor tags.
        if (!"a".equals(_elementStack.peek()))
          _LOG.warning("Illegal character (space) in \"name\" attribute");
      }
      // =-=AEW.  Some browsers are very unhappy when the "name"
      // attribute is set to "name" itself; we have gotten many emails and bugs
      // from people who make this mistake.
      else if ("name".equals(value))
      {
        _LOG.warning("\"name\" attribute incorrectly set to \"name\"");
      }
      // And "target" causes problems too - see bug 2693457
      else if ("target".equals(value))
      {
        _LOG.warning("\"name\" attribute set to \"target\", which will cause " +
               "Javascript errors.");
      }
    }
    // Javascript "onXXXX" handlers never need to start with
    // javascript:;  people constantly do this, which eventually causes
    // problems!
    else if ((name != null) && name.startsWith("on") && (value != null))
    {
      String valueStr = value.toString();
      if (valueStr.startsWith("javascript:") ||
          valueStr.startsWith("Javascript:"))
      {
        _LOG.info("The value of the \"" + name + "\" attribute starts with " +
                  "\"javascript:\"; this is unnecessary, and in fact can " +
                  "lead to Javascript errors.");
      }
    }

    super.writeAttribute(name, value, componentPropertyName);
  }

  private void _errorWithComment(String text) throws IOException
  {
    _LOG.warning(text);
    writeComment("INVALID HTML:");
  }


  // Yes, Stack is slow and lame.  This code is used for debugging
  // only, so that is of little concern.
  private Stack        _elementStack;

  static private final HashMap _sAllowedParents = new HashMap(13);
  static private final HashMap _sAllowedChildren  = new HashMap(13);

  static
  {
    // Allowed children of "table".  "tr" isn't legit as far
    // as the HTML 4.0 spec is concerned, but reality says something
    // else.  Ditto for "script", which basically can go anywhere,
    // but we hardcode that rule.
    _sAllowedChildren.put("table",
       Arrays.asList(
          new Object[]{"tr", "caption",
                       "thead", "tfoot", "tbody", "col", "colgroup"}));

    _sAllowedChildren.put("tr",
       Arrays.asList(
          new Object[]{"th", "td"}));

    _sAllowedChildren.put("select",
       Arrays.asList(
          new Object[]{"option", "optgroup"}));

    _sAllowedChildren.put("ol",
       Arrays.asList(
          new Object[]{"li"}));

    _sAllowedChildren.put("ul",
       Arrays.asList(
          new Object[]{"li"}));

    _sAllowedChildren.put("input",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("hr",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("br",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("area",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("link",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("img",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("col",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("frame",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("base",
       Arrays.asList(new Object[0]));

    _sAllowedChildren.put("meta",
       Arrays.asList(new Object[0]));

    _sAllowedParents.put("tr",
       Arrays.asList(
          new Object[]{"table", "thead", "tfoot", "tbody"}));

    _sAllowedParents.put("td",
       Arrays.asList(
          new Object[]{"tr"}));

    _sAllowedParents.put("th",
       Arrays.asList(
          new Object[]{"tr"}));

    _sAllowedParents.put("option",
       Arrays.asList(
          new Object[]{"select", "optgroup"}));
  }

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(DebugHtmlResponseWriter.class);
}
