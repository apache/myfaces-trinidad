package org.apache.myfaces.trinidaddemo.behaviors.render;

import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.behavior.ClientBehaviorContext;
import javax.faces.render.ClientBehaviorRenderer;

import org.apache.myfaces.trinidaddemo.behaviors.InvokeFunctionBehavior;


// Not using annotation so that Jetty can be used un-exploded
//@FacesBehaviorRenderer(
//  renderKitId = "org.apache.myfaces.trinidad.core",
//  rendererType = "invoke-function")
public class InvokeFunctionBehaviorRenderer
  extends ClientBehaviorRenderer
{
  @Override
  public String getScript(
    ClientBehaviorContext clientBehaviorContext,
    ClientBehavior        clientBehavior)
  {
    InvokeFunctionBehavior behavior = (InvokeFunctionBehavior)clientBehavior;
    String function = behavior.getFunction();
    return new StringBuilder(10 + function.length())
      .append("return ")
      .append(function)
      .append("();")
      .toString();
  }
}
