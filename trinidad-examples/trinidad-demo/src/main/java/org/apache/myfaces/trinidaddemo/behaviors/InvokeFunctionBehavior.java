package org.apache.myfaces.trinidaddemo.behaviors;

import javax.faces.component.behavior.ClientBehaviorBase;

// Not using annotation so that Jetty can be used un-exploded
//@FacesBehavior("invoke-function")
public class InvokeFunctionBehavior
  extends ClientBehaviorBase
{
  @Override
  public String getRendererType()
  {
    return "invoke-function";
  }

  public String getFunction()
  {
    return _function;
  }

  public void setFunction(String function)
  {
    this._function = function;
  }

  private String _function;
}
