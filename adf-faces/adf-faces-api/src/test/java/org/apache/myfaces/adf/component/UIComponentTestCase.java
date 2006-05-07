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
package org.apache.myfaces.adf.component;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.IOException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.Map;
import javax.faces.application.MockViewHandler;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.event.FacesEvent;

import javax.faces.application.MockApplication;
import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;
import javax.faces.convert.MockConverter;
import javax.faces.el.MockValueBinding;
import javax.faces.event.MockValueChangeListener;
import javax.faces.render.MockRenderKit;
import javax.faces.render.MockRenderKitFactory;
import javax.faces.render.MockRenderer;
import javax.faces.validator.MockValidator;

import org.apache.myfaces.adf.event.AttributeChangeEvent;
import org.apache.myfaces.adfbuild.test.FacesTestCase;

/**
 * Base class for JavaServer Faces UIComponent unit tests.
 *
 * @author John Fallows
 */
public class UIComponentTestCase extends FacesTestCase
{
  /**
   * Creates a new UIComponentTestCase.
   *
   * @param testName  the unit test name
   */
  public UIComponentTestCase(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   *
   * @param component   the component with attribute map
   * @param attrName    the attribute name to test
   * @param attrValue   the value for use by the attribute map mutator
   * @param propValue   the value for use by the bean mutator
   */
  protected void doTestAttributeTransparency(
    UIComponent component,
    String      attrName,
    Object      attrValue,
    Object      propValue)
  {
    assertFalse("Test values for attribute \"" + attrName + "\" must differ",
                (attrValue == propValue ||
                 (attrValue != null &&
                  attrValue.equals(propValue))));

    Map attrMap = component.getAttributes();
    try
    {
      boolean foundProperty = false;
      // bean info is cached
      BeanInfo info = Introspector.getBeanInfo(component.getClass());
      PropertyDescriptor[] pds = info.getPropertyDescriptors();
      for (int i=0; i < pds.length; i++)
      {
        String propName = pds[i].getName();
        if (attrName.equals(propName))
        {
          if (pds[i].getPropertyType().isPrimitive())
          {
            assertNotNull("Primitive \"" + attrName +
                          "\" attribute value must be non-null",
                          attrValue);
            assertNotNull("Primitive \"" + propName +
                          "\" property value must be non-null",
                          propValue);
          }

          foundProperty = true;

          Method reader = pds[i].getReadMethod();
          Method writer = pds[i].getWriteMethod();
          writer.invoke(component, new Object[] { propValue });
          assertEquals("Property set not visible in attribute map",
                       attrMap.get(attrName), propValue);
          attrMap.put(attrName, attrValue);
          assertEquals("Attribute put not visible in property value",
                       reader.invoke(component, new Object[0]), attrValue);
          break;
        }
      }

      if (!foundProperty)
        fail("Unable to find attribute property \"" + attrName + "\"");
    }
    catch (IntrospectionException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
    catch (InvocationTargetException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
    catch (IllegalAccessException e)
    {
      e.printStackTrace();
      fail("Unable to access attribute property \"" + attrName + "\"");
    }
  }

  /**
   * Tests the transparency of the component facet by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   *
   * @param component   the component with attribute map
   * @param facetName   the facet name to test
   * @param facetValue  the value for use by the facet map mutator
   * @param propValue   the value for use by the bean mutator
   */
  protected void doTestFacetTransparency(
    UIComponent component,
    String      facetName)
  {
    MockUIComponent facetValue = new MockUIComponent();
    facetValue.setupGetParent(null);
    MockUIComponent propValue = new MockUIComponent();
    propValue.setupGetParent(null);

    Map facetMap = component.getFacets();
    try
    {
      // bean info is cached
      BeanInfo info = Introspector.getBeanInfo(component.getClass());
      PropertyDescriptor[] pds = info.getPropertyDescriptors();
      boolean foundProperty = false;
      for (int i=0; i < pds.length; i++)
      {
        String propName = pds[i].getName();
        if (facetName.equals(propName))
        {
          assertTrue("Facet bean accessor must return UIComponent or subclass",
            UIComponent.class.isAssignableFrom(pds[i].getPropertyType()));

          foundProperty = true;

          Method reader = pds[i].getReadMethod();
          Method writer = pds[i].getWriteMethod();
          writer.invoke(component, new Object[] { propValue });
          assertEquals("Property set not visible in facet map",
                       facetMap.get(facetName), propValue);
          facetMap.put(facetName, facetValue);
          assertEquals("Facet put not visible in property value",
                       reader.invoke(component, new Object[0]), facetValue);
          break;
        }
      }

      if (!foundProperty)
        fail("Unable to find facet property \"" + facetName + "\"");
    }
    catch (IntrospectionException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    catch (InvocationTargetException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    catch (IllegalAccessException e)
    {
      e.printStackTrace();
      fail("Unable to access facet property \"" + facetName + "\"");
    }
    finally
    {
      facetValue.verify();
      propValue.verify();
    }
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestApplyRequestValues(
    UIComponent component)
  {
    UIViewRoot root = new UIViewRoot();
    doTestApplyRequestValues(root, component);
  }
  
  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestApplyRequestValues(
    UIViewRoot  root,
    UIComponent component)
  {
    MockRenderKitFactory factory = setupMockRenderKitFactory();
    MockRenderKit renderkit = new MockRenderKit();
    MockRenderer renderer = new MockRenderer();
    MockFacesContext context = createMockFacesContext(component);
    setCurrentContext(context);

    factory.setupGetRenderKit(renderkit);
    renderkit.setupGetRenderer(renderer);
    context.setupGetRenderResponse(false);
    context.setupGetRenderKit(renderkit);

    context.setupGetViewRoot(root);

    if (isRendererUsed() && component.isRendered())
    {
      renderer.addExpectedDecodeValues(context, component);
      renderer.setExpectedDecodeCalls(1);
    }
    else
    {
      renderer.setExpectedDecodeCalls(0);
    }

    doTestApplyRequestValues(context, root, component);

    factory.verify();
    renderkit.verify();
    renderer.verify();
    context.verify();

    setCurrentContext(null);
  }


  protected void doTestApplyRequestValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    MockUIComponent child = createMockUIComponent();
    // JavaServer Faces 1.0 Specification, section 2.2.2
    // During the apply-request-values phase,
    // only the processDecodes lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      child.setExpectedProcessDecodesCalls(1);

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);
      
    component.getChildren().add(child);

    AttributeChangeTester attributeChangeTester = null;
    if (component instanceof UIXComponent)
    {
      attributeChangeTester = new AttributeChangeTester();
      ((UIXComponent) component).setAttributeChangeListener(attributeChangeTester);
      ((UIXComponent) component).addAttributeChangeListener(attributeChangeTester);
      AttributeChangeEvent ace =
        new AttributeChangeEvent(component, "testProperty",
                                 Boolean.FALSE, Boolean.TRUE);
      ace.queue();
    }

    root.processDecodes(context);

    if (attributeChangeTester != null)
      attributeChangeTester.verify();

    child.verify();
  }

  /**
   * Create the MockFacesContext to use for testing.
   */
  protected MockFacesContext createMockFacesContext(UIComponent comp)
  {
    MockFacesContext context = new MockFacesContext();
    return context;
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIComponent component)
  {
    doTestProcessValidations(component, "submittedValue", "convertedValue");
  }

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIComponent component,
    Object      submittedValue,
    Object      convertedValue)
  {
    UIViewRoot root = new UIViewRoot();
    doTestProcessValidations(root, component, submittedValue, convertedValue);
  }
  
  /**
   * Tests the process-validations lifecycle phase.
   */
  protected void doTestProcessValidations(
    UIViewRoot  root,
    UIComponent component,
    Object      submittedValue,
    Object      convertedValue)
  {
    MockRenderKitFactory factory = setupMockRenderKitFactory();
    MockRenderKit renderkit = new MockRenderKit();
    MockRenderer renderer = new MockRenderer();
    MockFacesContext context = createMockFacesContext(component);
    MockConverter converter = new MockConverter();
    MockValidator validator = new MockValidator();
    MockValueChangeListener listener = new MockValueChangeListener();

    setCurrentContext(context);

    factory.setupGetRenderKit(renderkit);
    renderkit.setupGetRenderer(renderer);
    context.setupGetRenderKit(renderkit);
    context.setupGetRenderResponse(false);

    context.setupGetViewRoot(root);

    // if the component is an EditableValueHolder, then the submitted value
    // must be converted and validated before this phase completes.
    if (component instanceof EditableValueHolder)
    {
      EditableValueHolder editable = (EditableValueHolder)component;
      renderer.setupGetConvertedValue(convertedValue);
      converter.setExpectedGetAsObjectCalls(0);
      converter.setExpectedGetAsStringCalls(0);
      renderer.setExpectedGetConvertedValueCalls(1);
      editable.setConverter(converter);
      editable.setSubmittedValue(submittedValue);
      editable.addValidator(validator);
      editable.addValueChangeListener(listener);

      validator.addExpectedValidateValues(context, component,
                                          convertedValue);
      validator.setExpectedValidateCalls(1);
      listener.setExpectedProcessValueChangeCalls(1);
    }
    // if the component is a ValueHolder, then the value is not updated or
    // validated and no value change event occurs.
    else if (component instanceof ValueHolder)
    {
      ValueHolder holder = (ValueHolder)component;
      holder.setConverter(converter);
      converter.setExpectedGetAsStringCalls(0);
      converter.setExpectedGetAsObjectCalls(0);
    }

    doTestProcessValidations(context, root, component);

    factory.verify();
    renderkit.verify();
    renderer.verify();
    context.verify();
    converter.verify();
    validator.verify();
    listener.verify();

    setCurrentContext(null);
  }


  protected void doTestProcessValidations(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    MockUIComponent child = createMockUIComponent();
    // JavaServer Faces 1.0 Specification, section 2.2.3
    // During the process-validations phase,
    // only the processValidators lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      child.setExpectedProcessValidatorsCalls(1);

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);
    component.getChildren().add(child);
    root.processValidators(context);

    child.verify();
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  protected void doTestUpdateModelValues(
    UIComponent component)
  {
    UIViewRoot root = new UIViewRoot();
    doTestUpdateModelValues(root, component);
  }
  
  /**
   * Tests the update-model-values lifecycle phase.
   */
  protected void doTestUpdateModelValues(
    UIViewRoot  root,
    UIComponent component)
  {
    MockRenderKitFactory factory = setupMockRenderKitFactory();
    MockRenderKit renderkit = new MockRenderKit();
    MockRenderer renderer = new MockRenderer();
    MockFacesContext context = createMockFacesContext(component);
    MockValueBinding binding = new MockValueBinding();

    factory.setupGetRenderKit(renderkit);
    renderkit.setupGetRenderer(renderer);
    context.setupGetRenderResponse(false);
    context.setupGetRenderKit(renderkit);

    setCurrentContext(context);

    context.setupGetViewRoot(root);

    // if the component is an EditableValueHolder, then the value binding
    // must be updated with the new value before this phase completes.
    if (component instanceof EditableValueHolder)
    {
      EditableValueHolder editable = (EditableValueHolder)component;
      component.setValueBinding("value", binding);
      editable.setValue("newValue");
      binding.addExpectedSetValueValues(context, "newValue");

      assertEquals(true, editable.isLocalValueSet());
    }

    doTestUpdateModelValues(context, root, component);

    setCurrentContext(null);

    factory.verify();
    renderkit.verify();
    renderer.verify();
    context.verify();
    binding.verify();
  }


  protected void doTestUpdateModelValues(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component)
  {
    MockUIComponent child = createMockUIComponent();

    // JavaServer Faces 1.0 Specification, section 2.2.4
    // During the update-model-values phase,
    // only the processUpdates lifecycle method may be called.
    if (willChildrenBeProcessed(component))
      child.setExpectedProcessUpdatesCalls(1);

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    if (component.getParent() == null)
      root.getChildren().add(component);
    component.getChildren().add(child);
    root.processUpdates(context);

    child.verify();
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  protected void doTestInvokeApplication(
    UIComponent   component,
    FacesEvent    event)
  {
    MockApplication application = new MockApplication();
    MockFacesContext context = new MockFacesContext();

    try
    {
      setCurrentContext(context);

      UIViewRoot root = new UIViewRoot();
      context.setupGetViewRoot(root);

      context.setupGetApplication(application);
      application.setupGetActionListener(null);

      doTestInvokeApplication(context, root, component, event);

      application.verify();
      context.verify();
    }
    finally
    {
      setCurrentContext(null);
    }
  }



  protected void doTestInvokeApplication(
    FacesContext context,
    UIViewRoot   root,
    UIComponent  component,
    FacesEvent   event)
  {
    MockUIComponent child = createMockUIComponent();
    // JavaServer Faces 1.0 Specification, section 2.2.5
    // During the invoke-application phase,
    // no per-component lifecycle methods may be called.

    // construct the UIComponent tree and
    // execute the apply-request-values lifecycle phase
    root.getChildren().add(component);
    if (event != null)
      event.queue();

    component.getChildren().add(child);
    root.processApplication(context);

    child.verify();
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  protected void doTestRenderResponse(
    UIComponent component) throws IOException
  {
    MockRenderKitFactory factory = setupMockRenderKitFactory();
    MockRenderKit renderkit = new MockRenderKit();
    MockRenderer renderer = new MockRenderer();
    MockFacesContext context = new MockFacesContext();
    MockValidator validator = new MockValidator();
    MockUIComponent child = new MockUIComponent();

    factory.setupGetRenderKit(renderkit);
    renderkit.setupGetRenderer(renderer);
    context.setupGetRenderKit(renderkit);
    context.setupGetRenderResponse(false);

    child.setupGetParent(null);
    child.setupGetRendersChildren(true);
    child.setupIsTransient(false);
    child.setupProcessSaveState("ChildState");

    UIViewRoot root = new UIViewRoot();
    context.setupGetViewRoot(root);

    renderer.setupGetRendersChildren(false);
    renderer.setExpectedDecodeCalls(0);
    renderer.setExpectedGetConvertedValueCalls(0);

    renderer.setExpectedEncodeChildrenCalls(0);
    if (isRendererUsed())
    {
      renderer.setExpectedEncodeBeginCalls(1);
      renderer.setExpectedEncodeEndCalls(1);
    }
    else
    {
      renderer.setExpectedEncodeBeginCalls(0);
      renderer.setExpectedEncodeEndCalls(0);
    }

    // JavaServer Faces 1.0 Specification, section 2.2.6
    // During the render-response phase,
    // only the encodeBegin, encodeEnd, encodeChildren
    // and processSaveState lifecycle methods may be called.
    child.setExpectedProcessRestoreStateCalls(0);
    child.setExpectedProcessDecodesCalls(0);
    child.setExpectedProcessValidatorsCalls(0);
    child.setExpectedProcessUpdatesCalls(0);
    child.setExpectedProcessSaveStateCalls(1);
    int encodeCalls = willChildrenBeRendered(component) ? 1 : 0;
    child.setExpectedEncodeBeginCalls(encodeCalls);
    child.setExpectedEncodeChildrenCalls(encodeCalls);
    child.setExpectedEncodeEndCalls(encodeCalls);

    root.getChildren().add(component);
    component.getChildren().add(child);

    FacesContext current = FacesContext.getCurrentInstance();
    try
    {
      TestFacesContext.setCurrentInstance(context);
      root.processSaveState(context);
      doRenderResponse(context, root);
    }
    finally
    {
      TestFacesContext.setCurrentInstance(current);
    }

    factory.verify();
    renderkit.verify();
    renderer.verify();
    context.verify();
    validator.verify();
    child.verify();
  }

  protected void doTestValidateFailure(
    UIViewRoot root)
  {
    MockRenderKitFactory factory = setupMockRenderKitFactory();
    MockRenderKit renderkit = new MockRenderKit();
    MockRenderer renderer = new MockRenderer();
    MockApplication application = new MockApplication();
    MockViewHandler viewhandler = new MockViewHandler();
    MockFacesContext context = new MockFacesContext();

    setCurrentContext(context);

    factory.setupGetRenderKit(renderkit);
    renderer.setupConvertClientId("clientId");
    renderkit.setupGetRenderer(renderer);
    viewhandler.setupCalculateLocale(Locale.getDefault());
    application.setupGetViewHandler(viewhandler);
    context.setupGetViewRoot(root);
    context.setupGetApplication(application);
    context.setupGetRenderKit(renderkit);
    context.setupGetRenderResponse(false);
    
    // these are called because of validation failure for required
    context.setExpectedAddMessageCalls(1);
    context.setExpectedRenderResponseCalls(1);

    root.processValidators(context);

    factory.verify();
    renderkit.verify();
    renderer.verify();
    context.verify();

    setCurrentContext(null);
  }

  /**
   * Creates a MockUIComponent that does not expect to have
   * any of its lifecycle methods invoked;  if you expect to
   * have any invoked, override the "expected calls" for
   * that lifecycle method.
   */
  protected MockUIComponent createMockUIComponent()
  {
    MockUIComponent child = new MockUIComponent();
    child.setupGetParent(null);
    child.setExpectedProcessRestoreStateCalls(0);
    child.setExpectedProcessDecodesCalls(0);
    child.setExpectedProcessValidatorsCalls(0);
    child.setExpectedProcessUpdatesCalls(0);
    child.setExpectedProcessSaveStateCalls(0);
    child.setExpectedEncodeBeginCalls(0);
    child.setExpectedEncodeChildrenCalls(0);
    child.setExpectedEncodeEndCalls(0);
    return child;
  }

  protected boolean willChildrenBeProcessed(UIComponent component)
  {
    return (component.isRendered());
  }

  protected boolean willChildrenBeRendered(UIComponent component)
  {
    return true;
  }

  protected boolean isRendererUsed()
  {
    return _isRendererUsed;
  }
  
  protected void setRendererUsed(boolean isRendererUsed)
  {
    _isRendererUsed = isRendererUsed;
  }
  
  private boolean _isRendererUsed = true;
}
