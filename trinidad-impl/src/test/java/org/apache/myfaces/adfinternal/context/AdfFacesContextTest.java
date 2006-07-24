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
package org.apache.myfaces.adfinternal.context;

import junit.framework.TestCase;

import org.apache.myfaces.adf.context.AdfFacesContext;

public class AdfFacesContextTest extends TestCase
{
  public AdfFacesContextTest(
    String testName)
  {
    super(testName);
  }

  public void testPageResolver()
  {
    AdfFacesContext context = _createContext();
    try
    {
      assertTrue(context.getPageResolver() instanceof TestPageResolver);
    }
    finally
    {
      context.release();
    }
  }

  public void testPageFlowScopeProvider()
  {
    AdfFacesContext context = _createContext();
    try
    {
      assertTrue(context.getPageFlowScopeProvider()
                   instanceof TestPageFlowScopeProvider);
    }
    finally
    {
      context.release();
    }
  }

  private AdfFacesContext _createContext()
  {
    // =-=AEW Would be better to create it with a mock context so we
    // can test parsing
    return (new AdfFacesContextFactoryImpl()).createContext(null, null);
  }
}
