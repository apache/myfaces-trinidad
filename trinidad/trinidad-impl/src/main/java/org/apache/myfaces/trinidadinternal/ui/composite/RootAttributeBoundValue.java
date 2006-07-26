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
package org.apache.myfaces.trinidadinternal.ui.composite;

import org.apache.myfaces.trinidadinternal.util.OptimisticHashMap;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * BoundValue that retrieves its value
 * from the <code>attrKey</code> attribute of the parent
 * RenderingContext's currentUINode.
 * <p>
 * This BoundValue is typically used to implement UINodeRenderers.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/composite/RootAttributeBoundValue.java#0 $) $Date: 10-nov-2005.18:56:52 $
 * @author The Oracle ADF Faces Team
 */
public class RootAttributeBoundValue implements BoundValue
{
  /**
   * Factory method for getting cached instances of
   * RootAttributeBoundValue.
   */
  public static RootAttributeBoundValue getBoundValue(
    AttributeKey attrKey
    )
  {
    RootAttributeBoundValue boundValue =
      (RootAttributeBoundValue)_boundValues.get(attrKey);

    if (boundValue == null)
    {
      boundValue = new RootAttributeBoundValue(attrKey);

      _boundValues.put(attrKey, boundValue);
    }

    return boundValue;
  }


  /*
   * BoundValue that retrieves its value
   * from the <code>attrKey</code> attribute of the parent
   * RenderingContext's currentUINode.
   * <p>
   * This BoundValue is typically used to implement UINodeRenderers.
   * from.
   * @param attrKey AttribtueKey of attribute to retrieve.
   */
  protected RootAttributeBoundValue(
    AttributeKey attrKey
    )
  {
    if (attrKey == null)
      throw new IllegalArgumentException();

    _attrKey = attrKey;
  }

  /**
   * Called to retrieve a value based on the current rendering
   * context.
   * @param context the rendering context
   */
  public Object getValue(
    RenderingContext context
    )
  {
    RenderingContext parentContext = context.getParentContext();

    if (parentContext != null)
    {
      UINode baseNode = parentContext.getAncestorNode(0);

      if (baseNode != null)
      {
        return baseNode.getAttributeValue(parentContext, _attrKey);
      }
    }

    return null;
  }

  private static OptimisticHashMap _boundValues = new OptimisticHashMap();

  private AttributeKey _attrKey;
}
