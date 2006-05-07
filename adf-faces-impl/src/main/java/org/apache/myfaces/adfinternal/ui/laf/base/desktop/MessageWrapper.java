/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.adf.util.LabeledFacesMessage;

/**
 * A thin wrapper around a {@Link javax.faces.application.FacesMessage}.
 * This wrapper offers up the message along with the ID of the component,
 * and a generic text.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/MessageWrapper.java#0 $) $Date: 10-nov-2005.18:55:27 $
 * @author The Oracle ADF Faces Team
 */
public class MessageWrapper extends FacesMessage
{
  /**
   * Creates a {@Link javax.faces.application.FacesMessage} wrapper
   *
   * @param message The base {@Link javax.faces.application.FacesMessage}.
   * @param id      The ID of the component for which this message was queued.
   *
   */
  public MessageWrapper(FacesMessage message, String id)
  {
    setSeverity(message.getSeverity());
    setSummary(message.getSummary());
    setDetail(message.getDetail());
    _id = id;
    _msg = message;
  }

  /**
   * Returns the label of the component if it was saved with the message.
   */
  public String getLabel()
  {
    return getLabel(null);
  }

  /**
   * Returns the label of the component if it was saved with the message.
   *
   * @param dflt A default string to return of no label is found
   */
  public String getLabel(String dflt)
  {
    if (Boolean.TRUE.equals(_labelSearch))
      return (_label == null) ? dflt : _label;

    _labelSearch = Boolean.TRUE;

    if (_msg instanceof LabeledFacesMessage)
    {
      Object label = ((LabeledFacesMessage) _msg).getLabel();
      if (label != null)
      {
        _label = label.toString();
        return _label;
      }
    }
    return dflt;
  }

  /**
   * Returns true if there is a label attached to this message
   */
  public boolean hasLabel()
  {
    return (getLabel(null) != null);
  }

  /**
   * Returns the ID of the component that queued this message. Will return null
   * for global messages.
   */
  public Object getId()
  {
    return _id;
  }

  /**
   * Returns a text string suitable for a description field.
   */
  public String getText()
  {
    String txt = getDetail();
    if (txt == null)
      txt = getSummary();
    return txt;
  }

  private String       _id;
  private FacesMessage _msg;
  private Boolean      _labelSearch = null;
  private String       _label = null;
}
