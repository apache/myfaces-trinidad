/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.laf.base;

import java.io.IOException;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * Interface for tagging a Renderer as one that can be executed
 * in separate pre- and post-rendering steps, with children rendered
 * independently of the parent.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/PreAndPostRenderer.java#0 $) $Date: 10-nov-2005.18:53:06 $
 * @author The Oracle ADF Faces Team
 */
public interface PreAndPostRenderer
{
  public void prerender(
    RenderingContext context,
    UINode           node) throws IOException;

  public void postrender(
    RenderingContext context,
    UINode           node) throws IOException;
}
