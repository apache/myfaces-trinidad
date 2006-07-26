/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core;

import javax.faces.application.StateManager;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.ResponseStateManager;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.OptionalDataException;
import java.io.StringReader;
import java.io.BufferedReader;
import java.io.StringWriter;
import java.io.BufferedWriter;


import java.util.zip.GZIPOutputStream;
import java.util.zip.GZIPInputStream;

import java.util.Map;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adf.util.Base64InputStream;
import org.apache.myfaces.adf.util.Base64OutputStream;

/**
 * ResponseStateManager implementation for the Core RenderKit.
 * <p>
 * @author The Oracle ADF Faces Team
 */
public class CoreResponseStateManager extends ResponseStateManager
{
  /**
   * Name of the form field that encodes the UI state.
   */
  static public final String FORM_FIELD_NAME = "org.apache.myfaces.adf.faces.FORM";

  /**
   * Write the state into the page.
   * @todo Stream the resulting state into the page's content
   *       instead of buffering it.
   * @todo Don't directly write out hidden input field;  use an abstraction
   */
  public void writeState(
    FacesContext context,
    StateManager.SerializedView serializedView) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("input", null);
    rw.writeAttribute("type", "hidden", null);
    rw.writeAttribute("name", _STATE_FIELD_NAME, null);

    String s = encodeSerializedViewAsString(serializedView);
    rw.writeAttribute("value", s, null);

    rw.endElement("input");
  }


  protected String encodeSerializedViewAsString(
    StateManager.SerializedView serializedView) throws IOException
  {
    if ((serializedView.getState() == null) &&
        (serializedView.getStructure() instanceof String))
      return _TOKEN_PREFIX + serializedView.getStructure();

    StringWriter sw = new StringWriter();
    BufferedWriter bw = new BufferedWriter(sw);
    Base64OutputStream b64_out = new Base64OutputStream(bw);
    GZIPOutputStream zip = new GZIPOutputStream(b64_out, _BUFFER_SIZE);
    ObjectOutput output = new ObjectOutputStream(zip);

    output.writeObject(serializedView.getStructure());
    output.writeObject(serializedView.getState());

    zip.finish();

    b64_out.finish();
    bw.flush();

    String retVal = sw.toString();

    assert(!retVal.startsWith(_TOKEN_PREFIX));
    return retVal;
  }

  public Object getTreeStructureToRestore(FacesContext context,
                                          String viewId)
  {
    Object[] view = _restoreSerializedView(context);
    if (view == null)
      return null;

    return view[0];
  }

  public Object getComponentStateToRestore(FacesContext context)
  {
    Object[] view = _restoreSerializedView(context);
    if (view == null)
      return null;

    return view[1];
  }


  /**
   * Restore the serialized view from the incoming request.
   * @todo ensure that the caching never gets confused by two
   *       different views being reconstituted in the same request?
   */
  private Object[] _restoreSerializedView(
     FacesContext context)
  {
    Map requestMap = context.getExternalContext().getRequestMap();
    Object[] view = (Object[]) requestMap.get(_CACHED_SERIALIZED_VIEW);
    if (view == null)
    {
      Map requestParamMap =
         context.getExternalContext().getRequestParameterMap();

      String stateString = (String) requestParamMap.get(_STATE_FIELD_NAME);
      if (stateString == null)
        return null;

      // First see if we've got a token;  that'll be the case if we're
      // prefixed by _TOKEN_PREFIX
      if (stateString.startsWith(_TOKEN_PREFIX))
      {
        String tokenString = stateString.substring(_TOKEN_PREFIX.length());
        view = new Object[]{tokenString, null};
      }
      // Nope, let's look for a regular state field
      else
      {
        StringReader sr = new StringReader(stateString);
        BufferedReader br = new BufferedReader(sr);
        Base64InputStream b64_in = new Base64InputStream(br);


        try
        {
          ObjectInputStream ois;
          ois = new ObjectInputStream( new GZIPInputStream( b64_in,
                                                            _BUFFER_SIZE ));

          Object structure = ois.readObject();
          Object state = ois.readObject();
          ois.close();
          view = new Object[]{structure, state};
        }
        catch (OptionalDataException ode)
        {
          _LOG.severe(ode);
        }
        catch (ClassNotFoundException cnfe)
        {
          _LOG.severe(cnfe);
        }
        catch (IOException ioe)
        {
          _LOG.severe(ioe);
        }
      }

      if (view != null)
        requestMap.put(_CACHED_SERIALIZED_VIEW, view);
    }

    return view;
  }

  static private final String _STATE_FIELD_NAME = "org.apache.myfaces.adf.faces.STATE";


  /* Test code for dumping out the page's state
  static private void _dump(Object o)
  {
    System.out.println("DUMPING STATE");
    _dump(o, 0);
  }

  static private void _dump(Object o, int depth)
  {
    if (o instanceof Object[])
    {
      Object[] array = (Object[]) o;
      _spaces(depth);
      System.out.println("array of length " + array.length);
      for (int i = 0; i < array.length; i++)
        _dump(array[i], depth + 1);
    }
    else
    {
      _spaces(depth);
      System.out.println(o);
    }
  }

  static private void _spaces(int count)
  {
    int i = 0;
    for (; i + 5 < count; i += 5)
      System.out.print("     ");
    for (; i < count; i++)
      System.out.print(" ");
  }

  */

  static private final int _BUFFER_SIZE = 2048;
  static private final String _CACHED_SERIALIZED_VIEW =
    "org.apache.myfaces.adfinternal.renderkit.CACHED_SERIALIZED_VIEW";

  // Exclamation marks are not legit Base64 characters;  only
  // A-Z, a-z, 0-9, +, /, and = can ever show up.
  static private final String _TOKEN_PREFIX = "!";

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(CoreResponseStateManager.class);
}
