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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;


/**
 * Renderer for button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ButtonRenderer.java#0 $) $Date: 10-nov-2005.18:53:45 $
 * @author The Oracle ADF Faces Team
 */
public class ButtonRenderer extends LinkRenderer
{
  /**
   * Returns true if button tags are being used.  Provides a hook for
   * subclassers to override
   */
  protected boolean useButtonTags(
    RenderingContext context
    )
  {
    // button tags are used if we support advanced forms and
    // we support events for handling the onclick and
    // we support javascript
    return supportsAdvancedForms(context) &&
           supportsIntrinsicEvents(context)  &&
           supportsScripting(context);
  }

  protected boolean doRenderStyleAttrs(
    RenderingContext context,
    UINode           node
    )
  {
    // If we're rendering button tags, render the style class.
    // Otherwise LinkRenderer.prerender() handles this for us.
    return (useButtonTags(context));
  }

  protected void renderDestination(
    RenderingContext context,
    UINode           node,
    String           destination
    ) throws IOException
  {
    // if we don't support advanced forms, a link is as good as we can do
    if (!useButtonTags(context))
    {
      super.renderDestination(context, node, destination);
    }
  }

  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    if (useButtonTags(context))
    {
      renderAttribute(context, "type", getButtonType());

      if (!supportsAdvancedButtons(context))
      {
        renderAttribute(context, "value", getText(context, node));
      }

      renderAttribute(context, node, "disabled", DISABLED_ATTR);
    }


  }

  /**
   * Override to change the type of the button
   */
  protected String getButtonType()
  {
    return "button";
  }


  /**
   * The ID and the naem aren't the same for buttons and button subclasses
   */
  protected boolean makeNameAndIDSame(
    RenderingContext context
    )
  {
    return false;
  }


  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    if (useButtonTags(context))
    {
      if (supportsAdvancedButtons(context))
      {
        return "button";
      }
      else
      {
        return "input";
      }
    }
    else
    {
      // link support is as good as we can do
      return super.getElementName(context, node);
    }
  }

  /**
   * Return true if this link is empty ... has no children, text,
   * destination, or node name. We render nothing.
   */
  protected boolean isEmpty(
    RenderingContext context,
    UINode           node
    )
  {
    // buttons are never empty
    return false;
  }

  protected void prerender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (useButtonTags(context))
    {
      String elementName = getElementName(context, node);

      if (elementName != null)
      {
        UIComponent component = NodeUtils.getUIComponent(context, node);
        context.getResponseWriter().startElement(elementName, component);
        renderAttributes(context, node);

        // If we've got a ClientAction, let it write its dependencies
        // before we start rendering the link
        ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                       node);
        if (action != null)
          action.writeDependencies(context, node);
      }
    }
    else
    {
      // use link behavior
      super.prerender(context, node);
    }
  }

  protected void postrender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    if (useButtonTags(context))
    {
      String elementName = getElementName(context, node);

      if (elementName != null)
      {
        context.getResponseWriter().endElement(elementName);
      }
    }
    else
    {
      // use link behavior
      super.postrender(context, node);
    }
  }

  protected void renderContent(
    RenderingContext context,
    UINode           node
    )
    throws IOException
  {
    if (useButtonTags(context))
    {
      if (supportsAdvancedButtons(context))
      {
        renderAccessKeyText(context, node, getText(context, node), "u");
      }
    }
    else
    {
      // a link is as well as we can do here
      super.renderContent(context, node);
    }
  }

  protected Object getText(
    RenderingContext context,
    UINode           node
    )
  {
    Object text = super.getText(context, node);

    // If the text is null, create a button with an empty label
    if (text == null)
      return _EMPTY_LABEL;

    return text;
  }

  /**
   * Override to provide Javascript for moving to the destination
   */
   protected Object getOnClick(
      RenderingContext context,
      UINode           node
      )
    {
      Object clientOnClick = getClientOnClick(context, node);

      if (useButtonTags(context))
      {
        String destination
          = BaseLafUtils.getStringAttributeValue(context, node,
                                                 DESTINATION_ATTR);

        if (destination != null)
        {
          destination = encodeURL(context, destination);
          Object targetFrame = getTargetFrame(context, node);

          // if destination starts with "javascript:",
          // then just use the destination as is
          String onClickJS = destination;

          // if there's a target frame the destination should be an url
          if (targetFrame != null)
          {
            onClickJS = "top["         +
                        targetFrame    +
                        "].location='" +
                        destination    +
                        "'";
          }
          else if ( destination.length() < 11 ||
                    !"javascript:".equalsIgnoreCase(destination.substring(0,11)))
          {
            onClickJS = "document.location='" + destination + "'";
          }

          clientOnClick = XhtmlLafUtils.getChainedJS(clientOnClick,
                                                     onClickJS,
                                                     true);
        }
      }

      return clientOnClick;
  }

  protected final Object getClientOnClick(
    RenderingContext context,
    UINode           node
    )
  {
    return super.getOnClick(context, node);
  }

  private static final String _EMPTY_LABEL = "";
}
