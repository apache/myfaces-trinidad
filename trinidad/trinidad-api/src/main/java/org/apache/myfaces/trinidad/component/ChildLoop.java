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
package org.apache.myfaces.trinidad.component;
import java.util.Collection;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * @author The Oracle ADF Faces Team
 */
abstract class ChildLoop 
{
  public ChildLoop()
  {
  }

  public final void run(FacesContext context, UIXCollection comp)
  {
    run(context, comp, comp);
  }

  public final void run(FacesContext context, 
                        UIXCollection comp, UIComponent child)
  {
    if (shouldRun(comp))
      runAlways(context, child);
  }
  
  public final void runAlways(FacesContext context, UIComponent comp)
  {
    runAlways(context, comp.getChildren());
  }

  public final void runAlways(FacesContext context, Collection kids)
  {
    Iterator children = kids.iterator();
    while (children.hasNext())
    {
      UIComponent child = (UIComponent) children.next();
      process(context, child);
    }
  }
  
  /**
   * See if we need to run at all (which is usually only
   * if there is available data on this row)
   */
  protected boolean shouldRun(UIXCollection comp)
  {
    return comp.isRowAvailable();
  }

  protected abstract void process(FacesContext context, 
                                  UIComponent child);
  
  static final class Update extends ChildLoop
  {
    protected void process(FacesContext context, UIComponent child)
    {
      child.processUpdates(context);
    }
  }

  static final class Validate extends ChildLoop
  {
    protected void process(FacesContext context, UIComponent child)
    {
      child.processValidators(context);
    }
  }

  static final class Decode extends ChildLoop
  {
    protected void process(FacesContext context, UIComponent child)
    {
      child.processDecodes(context);
    }
  }
                                  
}
