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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.io.IOException;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FormInputRenderer.java#0 $) $Date: 10-nov-2005.18:53:50 $
 * @author The Oracle ADF Faces Team
 */
abstract public class FormInputRenderer extends FormElementRenderer
{
  protected void prerender(
    RenderingContext context,
    UINode node
    ) throws IOException
  {
    super.prerender(context, node);

    // Register a postback form element name with the FormEncoder
    if (!Boolean.TRUE.equals(getReadOnly(context, node)) &&
        !Boolean.TRUE.equals(getDisabled(context, node)))
    {
      Object transName = getTransformedName(context, node);
      context.getFormEncoder().registerFormParameter(transName);
    }
  }

  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    // render the disabled attribute
    if (supportsDisabledFormElements(context))
      renderAttribute(context, "disabled", getDisabled(context, node));

    // render the name attribute
    renderAttribute(context, "name", getTransformedName(context, node));
  }

  /**
   * Renders event handlers for the node.
   */
  protected void renderEventHandlers(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderEventHandlers(context, node);

    // render the onblur event handler
    renderAttribute(context, "onblur",  getOnBlur(context, node));

    // render the onfocus event handler
    renderAttribute(context, "onfocus", getOnFocus(context, node));
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return INPUT_ELEMENT;
  }
}

