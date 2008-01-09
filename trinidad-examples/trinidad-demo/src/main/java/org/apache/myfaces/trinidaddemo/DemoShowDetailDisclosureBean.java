package org.apache.myfaces.trinidaddemo;

import java.util.ArrayList;
import java.util.Arrays;

import java.util.List;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.util.ComponentUtils;


public class DemoShowDetailDisclosureBean
{
  private String _disclosureKey = "ItemA";
  private final static List<String> KEYS = new ArrayList<String>(
    Arrays.asList(new String[] {"ItemA","ItemB","ItemC","ItemD"}));
  
  public List<String> getKeys()
  {
    return KEYS;
  }

  public void handleDisclosure(DisclosureEvent event)
  {
    String disclosureKey = (String)event.getComponent().getAttributes().get("disclosureKey");

    if (event.isExpanded())
    {
      setDisclosureKey(disclosureKey);
    }
    else if (disclosureKey != null && disclosureKey.equals(_disclosureKey))
    {
      // ensure 1 is selected
      setDisclosureKey("ItemA");
    }
  }

  public void setDisclosureKey(String disclosureKey)
  {
    if (KEYS.contains(disclosureKey))
    {
      this._disclosureKey = disclosureKey;
      System.out.println("Disclosure key set to: " + disclosureKey);
      RequestContext.getCurrentInstance().addPartialTarget(
        FacesContext.getCurrentInstance().getViewRoot().findComponent("panelHeader"));
    }
  }

  public String getDisclosureKey()
  {
    return _disclosureKey;
  }
}
