/*
 * Copyright 2004,2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfbuild.test;

import java.io.IOException;

import java.util.Iterator;

import javax.faces.FactoryFinder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKitFactory;

import junit.framework.Test;
import junit.framework.TestCase;

import javax.faces.render.MockRenderKitFactory;

/**
 * Base class for JavaServer Faces unit tests.
 *
 * @author John Fallows
 */
public class FacesTestCase extends TestCase
{
  /**
   * Creates a new FacesTestCase.
   *
   * @param testName  the unit test name
   */
  public FacesTestCase(
    String testName)
  {
    super(testName);
  }

  protected void setUp()
  {
    setupFactoryFinder();
  }

  protected void tearDown()
  {
    tearDownFactoryFinder();
  }

  /**
   * Configures the FactoryFinder RenderKitFactory implementation class.
   *
   * @param renderkitFactoryClass  the render kit factory class
   */
  protected RenderKitFactory setupRenderKitFactory(
    Class renderkitFactoryClass)
  {
    setupFactoryFinder();
    FactoryFinder.setFactory(FactoryFinder.RENDER_KIT_FACTORY,
                             renderkitFactoryClass.getName());
    return (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
  }

  /**
   * Configures the FactoryFinder MockRenderKitFactory implementation class.
   */
  protected MockRenderKitFactory setupMockRenderKitFactory()
  {
    return (MockRenderKitFactory)
      setupRenderKitFactory(MockRenderKitFactory.class);
  }

  /**
   * Ensures that this test case uses its own FactoryFinder to avoid
   * side effects between tests.
   */
  protected void setupFactoryFinder()
  {
    Thread t = Thread.currentThread();
    ClassLoader ccl = t.getContextClassLoader();
    t.setContextClassLoader(TestClassLoader.getTestClassLoader(ccl, this));
  }

  /**
   * Tears down the FactoryFinder for this test case to avoid
   * side effects between tests.
   */
  protected void tearDownFactoryFinder()
  {
    FactoryFinder.releaseFactories();
    Thread t = Thread.currentThread();
    ClassLoader ccl = t.getContextClassLoader();
    while (ccl instanceof TestClassLoader)
    {
      ccl = ccl.getParent();
    }
    t.setContextClassLoader(ccl);
  }

  /**
   * Renders the component tree.
   *
   * @param context    the faces context
   * @param component  the component tree to render
   *
   * @throws IOException  when the render fails
   */
  protected void doRenderResponse(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    component.encodeBegin(context);
    if (component.getRendersChildren())
    {
      component.encodeChildren(context);
    }
    else
    {
      Iterator children = component.getChildren().iterator();
      while (children.hasNext())
      {
        UIComponent child = (UIComponent)children.next();
        doRenderResponse(context, child);
      }
    }
    component.encodeEnd(context);
  }

  public void setCurrentContext(
      FacesContext context)
  {
    TestFacesContext.setCurrentInstance(context);
  }

  /**
   * @todo add this code to MockFacesContext
   */
  public static abstract class TestFacesContext extends FacesContext
  {
    public static void setCurrentInstance(
      FacesContext context)
    {
      FacesContext.setCurrentInstance(context);
    }
  }

  /**
   * Internal marker to determine if we have already created
   * a test context class loader.
   */
  private static class TestClassLoader extends ClassLoader
  {
    public TestClassLoader(
      ClassLoader parent,
      Test        test)
    {
      super(parent);
      _test = test;
    }

    public static ClassLoader getTestClassLoader(
      ClassLoader cl,
      Test        test)
    {
      if (cl instanceof TestClassLoader)
      {
        TestClassLoader tcl = (TestClassLoader)cl;

        // if this is the right test class loader, return it
        if (test == tcl._test)
          return tcl;

        // strip off any TestClassLoader parents
        cl = tcl.getParent();
        while (cl instanceof TestClassLoader)
        {
          cl = cl.getParent();
        }
      }

      // return the new TestClassLoader for this test
      return new TestClassLoader(cl, test);
    }

    private final Test _test;
  }
}
