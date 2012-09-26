package org.apache.myfaces.trinidaddemo.tagDemos;

import java.util.HashMap;
import java.util.Map;

public class ForEachComponentBindingBean
{
  public ForEachComponentBindingBean()
  {
    _componentMap = new HashMap<String, TestNamingContainerComponent>();
    for (int i = 1; i <= 5; ++i)
    {
      String key = Integer.toString(i);
      TestNamingContainerComponent comp = new TestNamingContainerComponent();
      _componentMap.put(key, comp);
    }
  }

  public Map<String, TestNamingContainerComponent> getComponentMap()
  {
    return _componentMap;
  }

  private final Map<String, TestNamingContainerComponent> _componentMap;
}
