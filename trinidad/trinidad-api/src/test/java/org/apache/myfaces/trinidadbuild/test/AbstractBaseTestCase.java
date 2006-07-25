package org.apache.myfaces.trinidadbuild.test;

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.shale.test.jmock.AbstractJmockJsfTestCase;
import org.jmock.Mock;

public abstract class AbstractBaseTestCase extends AbstractJmockJsfTestCase
{

  public AbstractBaseTestCase(String name)
  {
    super(name);
  }
  
  protected void setFacesContext(FacesContext context)
  {
    FacesTestCase.TestFacesContext.setCurrentInstance(context);
  }

  protected Mock buildMockUIComponent()
  {
    return buildMockUIComponent(1);
  }

  protected Mock buildMockUIComponent(
    int iterations
    )
  {
    return buildMockUIComponent(iterations, new String[] {"label"});
  }

  /**
   * Builds a MockUIComponent with attributes setup for the requested number of
   * test iterations.
   */
  protected Mock buildMockUIComponent(
    int iterations,
    String attributeNames[]
     )
  {
    int i;
    Mock c = mock(UIComponent.class);
    Map attrs = new HashMap();
    for (i = 0; i < attributeNames.length; i++)
      attrs.put(attributeNames[i], attributeNames[i]);
    for (i = 0; i < iterations; i++)
    {
      c.stubs().method("getAttributes").will(returnValue(attrs));
      c.stubs().method("getId").will(returnValue("mockId"));
    }

    return c;
  }
}