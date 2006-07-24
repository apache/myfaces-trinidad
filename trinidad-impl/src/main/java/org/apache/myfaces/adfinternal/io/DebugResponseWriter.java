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

 import java.util.Stack;
 import java.util.HashMap;
 import java.util.HashSet;
 import java.util.Set;
 import java.util.Map;

 import javax.faces.component.UIComponent;
 import javax.faces.context.ResponseWriter;

 import org.apache.myfaces.adf.logging.ADFLogger;


 /**
  * ResponseWriter that decorates another and checks for common
  * mistakes, like unbalanced elements.
  * <p>
  * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/io/DebugResponseWriter.java#0 $) $Date: 10-nov-2005.19:03:48 $
  * @author The Oracle ADF Faces Team
  */
 public class DebugResponseWriter extends ResponseWriterDecorator
 {
   /**
    * Creates a DebugResponseWriter.
    */
   public DebugResponseWriter(ResponseWriter decorated)
   {
     super(decorated);
     _elementStack = new Stack();
     _idMap = new HashMap();
     _attributes = new HashSet();
   }

   /**
    * Creates a new instance of this DebugResponseWriter, using a different
    * Writer.
    */
   public ResponseWriter cloneWithWriter(Writer writer)
   {
     return new DebugResponseWriter(
       getResponseWriter().cloneWithWriter(writer));
   }


   public void endDocument() throws IOException
   {
     super.endDocument();
     if (!_elementStack.empty())
     {
       _LOG.warning("Elements not closed:");
       for (int i = _elementStack.size() - 1; i >=0; i--)
         _LOG.warning(_elementStack.elementAt(i).toString());
     }
   }

   /**
    * Writes a comment.
    */
   public void writeComment(Object text) throws IOException
   {
     if ((text != null) && (text.toString().indexOf("--") >= 0))
       _LOG.warning("Comments cannot include \"--\"");

     _inElement = false;

     super.writeComment(text);
   }

   /**
    * Writes a String, escaped properly for this method.
    */
   public void writeText(Object text, String componentPropertyName) throws IOException
   {
     _inElement = false;
     super.writeText(text, componentPropertyName);
   }

   /**
    * Writes a character array, escaped properly for this method.
    */
   public void writeText(
     char[]      text,
     int         start,
     int         length) throws IOException
   {
     _inElement = false;
     super.writeText(text, start, length);
   }


   /**
    * Writes a string, without performing any escaping.
    */
   public void write(String text) throws IOException
   {
     _inElement = false;
     super.write(text);
   }

   /**
    * Writes a character array, without performing any escaping.
    */
   public void write(
     char[]      text,
     int         start,
     int         length) throws IOException
   {
     _inElement = false;
     super.write(text, start, length);
   }


   /**
    * Writes a character, without performing any escaping.
    */
   public void write(
     int ch
     ) throws IOException
   {
     _inElement = false;
     super.write(ch);
   }


   public void startElement(String name, UIComponent component) throws IOException
   {
     if ((component != null) && (_lastComponentStarted != component))
     {
       String componentAsString = component.getFamily();
       String id = component.getId();
       if (id != null)
       {
         componentAsString = componentAsString + "[\"" + id + "\"]";
       }

       writeComment("Start: " + componentAsString);

       _lastComponentStarted = component;
     }

     super.startElement(name, component);
     _inElement = true;
     _elementStack.push(name);
     _attributes.clear();
   }


   public void endElement(String name) throws IOException
   {
     _inElement = false;
     _lastComponentStarted = null;
     Object topElement = _elementStack.peek();
     if (!name.equals(topElement))
     {
       if (_LOG.isWarning())
       {
         _LOG.warning("Ending " + name + " when " +
                       topElement + " expected. Passes:"+_endElementCount);
       }

       for (int i = _elementStack.size() - 1; i >=0; i--)
       {
         if (name.equals(_elementStack.elementAt(i)))
         {
           _elementStack.setSize(i);
           break;
         }
       }
     }
     else
     {
       _elementStack.pop();
     }

     _endElementCount++;
     super.endElement(name);
   }

   public void writeAttribute(String name,
                              Object value,
                              String componentPropertyName)
         throws IOException
   {
     if (!_inElement)
     {
       _LOG.warning("Writing attribute outside of element");
     }

     if ("id".equals(name))
     {
      // For non HTML mark up, do not check for duplicate ids Bug#4340857
          if (_elementStack.size() > 0)
          {
            if(_elementStack.peek().toString().indexOf(":") == -1)
             _checkDuplicateIds(value);
          }
          else
           _checkDuplicateIds(value);
     }

     if (value != null)
       name = _checkDuplicateAttribute(name);

     super.writeAttribute(name, value, componentPropertyName);
   }

   public void writeURIAttribute(
     String     name,
     Object     value,
     String     componentPropertyName) throws IOException
   {
     if (!_inElement)
     {
       _LOG.warning("Writing attribute outside of element");
     }

     if (value != null)
       name = _checkDuplicateAttribute(name);

     super.writeURIAttribute(name, value, componentPropertyName);
   }


   /**
    * Check for duplicate ids. When a duplicate id is found,
    * log a message to the error log once for that id.
    */
   private String _checkDuplicateAttribute(String name)
   {
     if (_attributes.contains(name))
     {
       _LOG.warning("Attribute \"" + name + "\" output twice;  " +
          "writing attribute as \"duplicate_" + name + "\" instead.");
       return "duplicate_" + name;
     }
     else
     {
       _attributes.add(name);
       return name;
     }
   }

   /**
    * Check for duplicate ids. When a duplicate id is found,
    * log a message to the error log once for that id.
    */
   private void _checkDuplicateIds(Object value)
   {
     if (value != null)
     {
       Object exists = _idMap.get(value);
       if (exists != null)
       {
         // If we have not logged the duplicate id already, log it,
         // and note we logged it so we only log it once.
         if (Boolean.FALSE == exists)
         {
           _logDuplicateId(value);
           _idMap.put(value, Boolean.TRUE);
         }
       }
       else
       {
         // id was not already found. put it in the hashmap.
         _idMap.put(value, Boolean.FALSE);
       }
     }
   }
   /**
    * Log a message to the error log that the id already exists.
    */
   private void _logDuplicateId(Object value)
   {
     if (_LOG.isWarning())
     {
       String logString = "The id \"" + value + "\" is used more than once.";
       _LOG.warning(logString);
     }
   }

   private boolean      _inElement;
   // Yes, Stack is slow and lame.  This code is used for debugging
   // only, so that is of little concern.
   private Stack        _elementStack;
   // the following is used to keep track of how many time endElement is called.
   // This is to help in debuggin. An often error is that some element was
   // too many times or too few times. But by the time the error is detected
   // it is too late. Now we can print out this count, and you can set the
   // debugger to stop when the count-1 'th endElement is called:
   private int          _endElementCount = 0;
   private Map          _idMap;
   private Set          _attributes;
   // Last component that had a "start" component output;  used
   // to avoid multiple "start" comments
   private UIComponent  _lastComponentStarted;

   static private final ADFLogger _LOG = ADFLogger.createADFLogger(DebugResponseWriter.class);
 }

