/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.composite;

import java.io.IOException;

import org.apache.myfaces.adfinternal.util.OptimisticHashMap;

import org.apache.myfaces.adfinternal.util.IntegerUtils;

import org.apache.myfaces.adfinternal.ui.AttributeKey;
import org.apache.myfaces.adfinternal.ui.BaseUINode;
import org.apache.myfaces.adfinternal.ui.NodeRole;
import org.apache.myfaces.adfinternal.ui.Renderer;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.RoledRenderer;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.data.DataObject;
import org.apache.myfaces.adfinternal.ui.laf.base.*;

/**
 * Renderer used by composite UINode renderers to render content.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/ContextPoppingUINode.java#0 $) $Date: 10-nov-2005.18:56:51 $
 * @author The Oracle ADF Faces Team
 */
public class ContextPoppingUINode extends BaseUINode
{
  public static ContextPoppingUINode getUINode(
    String childName
    )
  {
    ContextPoppingUINode node =
                         (ContextPoppingUINode)_sContextPoppers.get(childName);

    if (node == null)
    {
      node = new ContextPoppingUINode(childName, -1);

      _sContextPoppers.put(childName, node);
    }

    return node;
  }


  public static ContextPoppingUINode getUINode(
    int childIndex
    )
  {
    Integer key = IntegerUtils.getInteger(childIndex);

    ContextPoppingUINode node =
                               (ContextPoppingUINode)_sContextPoppers.get(key);

    if (node == null)
    {
      node = new ContextPoppingUINode(null, childIndex);

      _sContextPoppers.put(key, node);
    }

    return node;
  }


  private ContextPoppingUINode(
    String childName,
    int    childIndex
    )
  {
    super(MARLIN_NAMESPACE, CONTEXT_POPPING_NAME);

    if ((childName == null) && (childIndex < 0))
      throw new IllegalArgumentException();

    _childName  = childName;
    _childIndex = childIndex;
  }


  public int getIndexedChildCount(
    RenderingContext context
    )
  {
    if (getPoppedNode(context) != null)
    {
      return 1;
    }
    else
    {
      return 0;
    }
  }


  public UINode getIndexedChild(
    RenderingContext context,
    int              childIndex
    )
  {
    if (childIndex == 0)
    {
      UINode poppedNode = getPoppedNode(context);

      if (poppedNode != null)
      {
        return poppedNode;
      }
    }

    throw new IndexOutOfBoundsException();
  }

  protected UINode getPoppedNode(
    RenderingContext context
    )
  {
    if (context == null)
      return null;

    context = context.getParentContext();
    if (context == null)
      return null;

    UINode rootNode = context.getAncestorNode(0);

    if (_childName != null)
    {
      return rootNode.getNamedChild(context, _childName);
    }
    else
    {
      return rootNode.getIndexedChild(context, _childIndex);
    }
  }

  protected Object getAttributeValueImpl(
    RenderingContext context,
    AttributeKey     attrKey,
    boolean          returnBoundValue
    )
  {
    if (attrKey == UIConstants.RENDERED_ATTR)
    {
      UINode poppedNode = getPoppedNode(context);

      if (poppedNode != null)
      {
        // ask the popped node for the rendered attribute in its context
        return poppedNode.getAttributeValue(context.getParentContext(),
                                            attrKey);
      }
      else
      {
        return Boolean.FALSE;
      }
    }
    else
    {
      return null;
    }
  }

  protected Renderer getRenderer(
    RenderingContext context,
    UINode           dataNode
    )
  {
    // get the renderer for ourselves
    return _RENDERER;
  }

  String __getChildName()
  {
    return _childName;
  }

  int __getChildIndex()
  {
    return _childIndex;
  }

  public String toString()
  {
    String text = super.toString();
    if (_childName != null)
      return text + "[name=" + _childName + "]";
    else
      return text + "[index=" + _childIndex + "]";
  }

  private static class ContextPoppingRenderer implements RoledRenderer
  {
    public void render(
      RenderingContext context,
      UINode           node
      )
      throws IOException
    {
      ContextPoppingUINode poppingNode =
                              (ContextPoppingUINode)context.getAncestorNode(0);
      DataObject poppingCurrentDataObject  = context.getCurrentDataObject();

      RenderingContext poppedContext = context.getParentContext();

      UINode poppedNode = poppingNode.getPoppedNode(context);

      // push on the child's path information
      poppedContext.pushChild(poppedNode,
                              poppingNode.__getChildName(),
                              poppingNode.__getChildIndex());
      poppedContext.pushRenderedChild(poppedContext, poppedNode);

      DataObject poppedCurrentDataObject =
        poppedContext.setCurrentDataObject(poppingCurrentDataObject);

      try
      {
        poppedNode.render(poppedContext);
      }
      finally
      {
        poppedContext.setCurrentDataObject(poppedCurrentDataObject);
        poppedContext.popRenderedChild(poppedContext);
        poppedContext.popChild();
      }
    }

    public NodeRole getNodeRole(
      RenderingContext context,
      UINode           node)
    {
      return STATE_ROLE;
    }
  }

  private static final Renderer _RENDERER = new ContextPoppingRenderer();

  private static OptimisticHashMap _sContextPoppers =
                                                    new OptimisticHashMap(203);

  private String _childName;
  private int    _childIndex;
}
