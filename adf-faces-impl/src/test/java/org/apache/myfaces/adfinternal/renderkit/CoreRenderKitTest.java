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
package org.apache.myfaces.adfinternal.renderkit;

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Test;

import org.xml.sax.SAXException;


public class CoreRenderKitTest extends RenderKitTestCase
{
  public static Test suite() throws Throwable
  {
    CoreRenderKitTest suite =
      new CoreRenderKitTest(CoreRenderKitTest.class.getName());
    return suite;
  }

  public CoreRenderKitTest(String testName) throws IOException, SAXException
  {
    super(testName);
  }


  protected Iterable<SuiteDefinition> getSuiteDefinitions()
  {
    return _definitions;
  }

  protected String getRenderKitId()
  {
    return "org.apache.myfaces.adf.core";
  }

  static private List<SuiteDefinition> _definitions =
    new ArrayList<SuiteDefinition>();

  static
  {
    _definitions.add(new SuiteDefinition("minimal",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalIE",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getIEAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalIERtl",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getIEAgent(),
                                         true));
    _definitions.add(new SuiteDefinition("minimalPPC",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getPocketPCAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalSaf",
                                         "minimal",
                                         null,
                                         RenderKitBootstrap.getSafariAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalScrRdr",
                                         "minimal",
                                         "screenReader",
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));
    _definitions.add(new SuiteDefinition("minimalInacc",
                                         "minimal",
                                         "inaccessible",
                                         RenderKitBootstrap.getGeckoAgent(),
                                         false));
  }
}
