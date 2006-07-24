/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.uix;

import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.uinode.DecodingUINodeRenderer;

/**
 * Renderer for input components in general.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/InputRenderer.java#0 $) $Date: 10-nov-2005.19:00:29 $
 * @author The Oracle ADF Faces Team
 */
public class InputRenderer extends DecodingUINodeRenderer
{
  protected boolean wasSubmitted(
      FacesContext context,
      UIComponent  component)
  {
    Map attrs = component.getAttributes();

    if (Boolean.TRUE.equals(attrs.get("readOnly")) ||
        Boolean.TRUE.equals(attrs.get("disabled")))
      return false;

    ValueBinding vb = component.getValueBinding("value");
    if ((vb != null) && vb.isReadOnly(context))
    {
      if (_LOG.isFiner())
      {
        _LOG.finer("Value expression {0} was read-only",
                   vb.getExpressionString());
      }

      return false;
    }

    return true;
  }

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(InputRenderer.class);
}
