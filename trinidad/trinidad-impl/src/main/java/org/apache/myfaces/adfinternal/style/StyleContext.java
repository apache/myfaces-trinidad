/*
 * Copyright  2000-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.style;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.share.nls.LocaleContext;

/**
 * The StyleContext interface is used to provide information
 * about the target end user environment.  It also provides
 * access to general-purpose facilities.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/StyleContext.java#0 $) $Date: 10-nov-2005.18:57:57 $
 * @author The Oracle ADF Faces Team
 */
public interface StyleContext
{
  /**
   * Returns the end user's locale.
   */
  public LocaleContext getLocaleContext();

  /**
   * Returns the end user's Agent.
   */
  public AdfFacesAgent getAgent();

  public String getGeneratedFilesPath();
  public boolean checkStylesModified();

  public boolean disableStandardsMode();

  public StyleProvider getStyleProvider();
  public StyleMap getStyleMap();
}
