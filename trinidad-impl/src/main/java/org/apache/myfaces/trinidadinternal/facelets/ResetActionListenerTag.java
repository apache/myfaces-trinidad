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
package org.apache.myfaces.trinidadinternal.facelets;

import java.io.IOException;

import javax.el.ELException;
import javax.faces.FacesException;
import javax.faces.component.ActionSource;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidadinternal.taglib.listener.ResetActionListener;

import com.sun.facelets.FaceletContext;
import com.sun.facelets.FaceletException;
import com.sun.facelets.tag.TagConfig;
import com.sun.facelets.tag.TagHandler;
import com.sun.facelets.tag.jsf.ComponentSupport;

/**
 *
 */
public class ResetActionListenerTag extends TagHandler
{

  public ResetActionListenerTag(TagConfig tagConfig)
  {
    super(tagConfig);
  }

  public void apply(FaceletContext faceletContext,
		  UIComponent parent) throws IOException, FacesException, FaceletException, ELException
  {
    if(ComponentSupport.isNew(parent))
    {
      ActionSource actionSource = (ActionSource)parent;
      ResetActionListener listener = new ResetActionListener();
      actionSource.addActionListener(listener);
    }
  }
}