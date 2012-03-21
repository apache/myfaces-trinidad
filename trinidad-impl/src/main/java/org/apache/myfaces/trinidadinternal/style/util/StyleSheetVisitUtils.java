/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.myfaces.trinidadinternal.style.util;

import java.util.Collection;

import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * A utility class that supports traversal/visiting of a collection of StyleSheetNodes.
 */
public final class StyleSheetVisitUtils
{
  /**
   * StyleSheetNode visitor contract.
   */
  public interface StyleSheetVisitor
  {
    /**
     * Invoked when a new style sheet is visited.
     */
    public void visit(StyleSheetNode styleSheet);
  }

  /**
   * Creates a StyleSheetVisitor that will invoke an arbitrary number
   * of visitors
   * 
   * @param visitors a non-null collection of StyleSheetVisitors to encapsulate.
   */
  public static StyleSheetVisitor compoundStyleSheetVisitor(
    Collection<? extends StyleSheetVisitor> visitors
    )
  {
    return new CompoundVisitor(visitors);  
  }
  
  /**
   * Traverses the specified style sheets and invokes a visitor for each.
   * 
   * @param styleSheets the non-null style sheets to visit
   * @param visitor the non-null visitor to invoke
   */
  public static void visitStyleSheets(
    Iterable<StyleSheetNode> styleSheets,
    StyleSheetVisitor        visitor
    )
  {
    assert(styleSheets != null);
    assert(visitor != null);

    for (StyleSheetNode styleSheet : styleSheets)
    {
      visitor.visit(styleSheet);
    }
  }

  private static final class CompoundVisitor implements StyleSheetVisitor
  {
    public CompoundVisitor(Collection<? extends StyleSheetVisitor> visitors)
    {
      assert(visitors != null);
      _visitors = visitors;
    }

    @Override
    public void visit(StyleSheetNode styleSheet)
    {
      for (StyleSheetVisitor visitor : _visitors)
      {
        visitor.visit(styleSheet);
      }
    }
    
    private final Collection<? extends StyleSheetVisitor> _visitors;
  }

  private StyleSheetVisitUtils()
  {
  }
}
