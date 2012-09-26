package org.apache.myfaces.trinidaddemo.tagDemos;

import javax.faces.component.NamingContainer;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroupLayout;

public class TestNamingContainerComponent
  extends CorePanelGroupLayout
  implements NamingContainer
{
  public TestNamingContainerComponent(String string)
  {
    super(string);
  }

  public TestNamingContainerComponent()
  {
    super();
  }
}
