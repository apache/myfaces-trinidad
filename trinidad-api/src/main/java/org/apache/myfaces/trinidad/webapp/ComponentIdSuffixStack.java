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
package org.apache.myfaces.trinidad.webapp;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Queue;

import javax.servlet.jsp.PageContext;


/**
 * Internal class to assist the {@link TrinidadIterationTag} and the
 * {@link UIXComponentELTag} to communicate with each other to maintain
 * the component ID suffixes.
 */
final class ComponentIdSuffixStack
{
  /**
   * Private as this class is implemented as a singleton.
   * @see #getInstance(PageContext)
   */
  private ComponentIdSuffixStack()
  {
    super();
  }

  /**
   * Get (or create) a component ID suffix stack from the page context.
   *
   * @param pageContext The JSP page context
   * @return the existing, or new, instance of the stack
   */
  final static ComponentIdSuffixStack getInstance(PageContext pageContext)
  {
    ComponentIdSuffixStack stack =
      (ComponentIdSuffixStack) pageContext.getAttribute(_PAGE_CONTEXT_KEY,
        PageContext.REQUEST_SCOPE);

    if (stack == null)
    {
      stack = new ComponentIdSuffixStack();
      pageContext.setAttribute(_PAGE_CONTEXT_KEY, stack,
          PageContext.REQUEST_SCOPE);
    }

    return stack;
  }

  /**
   * Get the current ID suffix.
   * @return the current suffix or null if no suffix has been pushed.
   */
  String getSuffix()
  {
    Suffix currentSuffix = _getCurrentSuffix();
    return currentSuffix == null ? null : currentSuffix.getValue();
  }

  /**
   * Push a suffix onto the stack.
   * @param suffix an non-null, non-empty suffix to append to components
   * created within the calling tag.
   * @see #pop()
   */
  void push(String suffix)
  {
    Suffix currentSuffix = _getCurrentSuffix();

    if (currentSuffix == null)
    {
      currentSuffix = new Suffix();
      _stack.offer(currentSuffix);
    }

    currentSuffix.push(suffix);
  }

  /**
   * Pop the current suffix.
   * @see #push(String)
   */
  void pop()
  {
    Suffix suffix = _getCurrentSuffix();

    assert suffix !=
      null: "Illegal call to pop an empty or suspended stack";

    if (0 == suffix.pop())
    {
      // If the new length is zero, we need to pop the entire stack
      _stack.poll();
    }
  }

  /**
   * Suspend the suffix stack. This is used when a naming container
   * component is encountered to ensure that children of the naming
   * container do not have modified IDs.
   * @see #resume()
   */
  void suspend()
  {
    _stack.offer(_NULL_SUFFIX);
  }

  /**
   * Resume a suspended suffix.
   * @see #suspend()
   */
  void resume()
  {
    Object o = _stack.poll();
    if (o != _NULL_SUFFIX)
    {
      throw new IllegalStateException("Illegal call to resume the stack when not suspended");
    }
  }

  /**
   * Get the current {@link Suffix} object. Returns null for suspended
   * suffixes or an empty stack.
   * @return The current suffix or null.
   */
  private Suffix _getCurrentSuffix()
  {
    Object o = _stack.peek();
    return (o == null || o == _NULL_SUFFIX) ? null : (Suffix) o;
  }

  /**
   * Inner class used to track the current suffix up to a naming container.
   */
  private static class Suffix
  {
    /**
     * Get the value of the suffix.
     * @return the string value.
     */
    String getValue()
    {
      return _suffix.toString();
    }

    /**
     * Push (append) to the suffix.
     * @param suffix the value to append
     */
    void push(String suffix)
    {
      assert suffix != null && suffix.length() > 0:
        "Illegal suffix, it must be non-null and not an empty string";

      // Remember the length before this value is pushed so that
      // the suffix can revert to the previous value on pop
      _pushedLengths.add(_suffix.length());
      _suffix.append(suffix);
    }

    /**
     * Pop the suffix.
     * @return the new length of the suffix. Will return 0 for an empty
     * suffix.
     */
    int pop()
    {
      assert _suffix.length() >
      0: "Illegal call to pop the suffix stack, there is no current suffix";

      Integer previousLength = _pushedLengths.remove();
      int len = previousLength;

      _suffix.setLength(len);

      return len;
    }

    private StringBuilder _suffix = new StringBuilder();
    private Queue<Integer> _pushedLengths =
      Collections.asLifoQueue(new ArrayDeque<Integer>(5));
  }

  private Queue<Object> _stack =
    Collections.asLifoQueue(new ArrayDeque<Object>());

  /** Used to determine when the stack has been suspended */
  private static final Object _NULL_SUFFIX = new Object();
  private final static String _PAGE_CONTEXT_KEY =
    ComponentIdSuffixStack.class.getName() + ".PAGE_CONTEXT";
}
