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
package org.apache.myfaces.trinidadbuild.test;

import javax.el.ELContext;

import javax.faces.application.Application;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.lifecycle.Lifecycle;

import org.apache.shale.test.mock.MockFacesContext;

public class MockFacesContext12 extends MockFacesContext
{
  public MockFacesContext12(ExternalContext ec,
                            Lifecycle   lifecycle,
                            Application application)
  {
    super(ec, lifecycle);
    elContext = createELContext(application);
    elContext.putContext(FacesContext.class, this);
  }

  public MockFacesContext12(Application application)
  {
    elContext = createELContext(application);
    elContext.putContext(FacesContext.class, this);
  }

  public ELContext getELContext()
  {
    return elContext;
  }

  protected MockELContext createELContext(Application application)
  {
    return new MockELContext(application);
  }

  protected MockELContext elContext;
}