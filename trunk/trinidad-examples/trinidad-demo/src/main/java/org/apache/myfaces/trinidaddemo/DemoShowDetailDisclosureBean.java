/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
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
