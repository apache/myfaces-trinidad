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
package org.apache.myfaces.adf.config;

/**
 * Provides metadata about region definitions.
 * Here's a simple example of a region definition in region-metadata.xml:
 * 
 * <pre> 
 *  &lt;component&gt;
 *    &lt;component-type&gt;
 *      org.apache.myfaces.adfdemo.view.faces.region.stock
 *    &lt;/component-type&gt;
 *    &lt;component-extension&gt;
 *      &lt;region-jsp-ui-def&gt;/regions/stock.jspx&lt;/region-jsp-ui-def&gt;
 *    &lt;/component-extension&gt;
 *  &lt;/component&gt;
 * </pre>
 * The regionType of a region is defined to be the 
 * componentType of that component.  
 * @author The Oracle ADF Faces Team
 */
public abstract class RegionConfig extends ComponentConfig
{
  protected RegionConfig()
  {
  }

  /**
   * Gets the URL of the jsp file that is used to render this region
   */
  public abstract String getJspUIDef();  
}
