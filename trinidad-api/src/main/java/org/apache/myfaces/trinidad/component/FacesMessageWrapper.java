/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidad.component;

import java.io.Externalizable;

import java.io.IOException;
import java.io.InvalidClassException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.trinidad.util.LabeledFacesMessage;

/**
 * Wraps faces messages thrown by third party converters so that they appear as
 * LabeledFacesMessages.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/FacesMessageWrapper.java#0 $) $Date: 10-nov-2005.19:09:45 $
 */
class FacesMessageWrapper extends LabeledFacesMessage implements Externalizable
{
  public FacesMessageWrapper()
  {
    this(new FacesMessage(), null);
  }
  
  public FacesMessageWrapper(FacesMessage wrapped, Object label)
  {
    _wrapped = wrapped;
    setLabel(label);
  }

  @Override
  public String getDetail()
  {
    return _wrapped.getDetail();
  }

  @Override
  public FacesMessage.Severity getSeverity()
  {
    return _wrapped.getSeverity();
  }

  @Override
  public String getSummary()
  {
    return _wrapped.getSummary();
  }

  @Override
  public Object getLabel()
  {
    if(_wrapped instanceof LabeledFacesMessage)
    {
      return ((LabeledFacesMessage)_wrapped).getLabel();
    }
    
    return _label;
  }

  @Override
  public void setDetail(String detail)
  {
    _wrapped.setDetail(detail);
  }

  @Override
  public void setSeverity(FacesMessage.Severity severity)
  {
    _wrapped.setSeverity(severity);
  }

  @Override
  public void setSummary(String summary)
  {
    _wrapped.setSummary(summary);
  }

  @Override
  public void setLabel(Object label)
  {
    if(_wrapped instanceof LabeledFacesMessage)
    {
      ((LabeledFacesMessage)_wrapped).setLabel(label);
    }
    else
    {
      _label = label;
    }
  }  

  /**
   * Writes this object to the serialized stream.
   * 
   * @param objectOutput
   * @throws IOException
   */
  public void writeExternal(ObjectOutput objectOutput)
    throws IOException
  {
    objectOutput.writeLong(serialVersionUID);
    objectOutput.writeObject(_wrapped);
    objectOutput.writeObject(_label);
  }

  /**
   * Reads this object from the serialized stream.
   * 
   * @param objectInput
   * @throws IOException
   * @throws ClassNotFoundException
   */
  public void readExternal(ObjectInput objectInput)
    throws IOException, ClassNotFoundException
  {
    Long version = objectInput.readLong();
    
    if(version != serialVersionUID)
    {
      throw new InvalidClassException("Classes are not compatible.  Received serialVersionUID = " + version + ", local serialVersionUID = "+ serialVersionUID);
    }

    _wrapped = (FacesMessage)objectInput.readObject();
    _label = objectInput.readObject();
  }

  private FacesMessage _wrapped;
  private Object _label;
  private static final long serialVersionUID = 1L;

}
