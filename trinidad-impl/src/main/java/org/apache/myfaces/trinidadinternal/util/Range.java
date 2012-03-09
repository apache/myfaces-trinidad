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
package org.apache.myfaces.trinidadinternal.util;

import java.util.Collection;


/**
 * Represents a range of values with a specific start and end.
 *
 * Used for SelectRangeChoiceBar to allow the app developer to customize
 * the labels to not use numbers, but to use the data model.
 *
 * Also used by skinning to represent agent version ranges.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/Range.java#0 $) $Date: 10-nov-2005.18:49:12 $
 */
public final class Range<T>
{
  /**
   * Returns a new range consisting of the specified start and end points.
   */
  public static <T> Range<T> of(T start, T end)
  {
    Range<T> range = new Range<T>();
    range.setStart(start);
    range.setEnd(end);
    return range;
  }

  public T getStart()
  {
    return _start;
  }
  
  public void setStart(T start)
  {
    _start = start;
  } 
  
  public T getEnd()
  {
    return _end;
  }
  
  public void setEnd(T end)
  {
    _end = end;
  }

  @Override  
  public String toString()
  {
    return getClass().getName() + "[" + _start + " - " + _end + "]";
  }

  /**
   * Computes the intersection of the specified range with a
   * collection of other ranges.  The results of the intersection
   * are stored in the provided range.
   * 
   * In the event that there is no intersection, the range will
   * be left in a state where range.getStart() > range.getEnd().
   * 
   * @param range the (non-null) range to intersect/update
   * @param ranges the ranges to include in the intersection.
   */
  public static <C extends Comparable> void intersect(
    Range<C>             range,
    Collection<Range<C>> ranges
    )
  {
    // I would have preferred to implement this as a non-static
    // method.  However, we cannot change the Range class type
    // parameter to extend Comparable since we have one use case
    // where T == Object.  Thus, we're stuck with a static method.
    
    assert(range != null);
    
    if (ranges != null)
    {
      for (Range<C> otherRange : ranges)
      {
        _intersect(range, otherRange);
      }
    }
  }

  // Computes the intersection of two ranges, updating the
  // first range.
  private static <C extends Comparable> void _intersect(
    Range<C> range,
    Range<C> otherRange
    )
  {
    C otherStart = otherRange.getStart();
    if (otherStart.compareTo(range.getStart()) > 0)
    {
      range.setStart(otherStart);
    }
    
    C otherEnd = otherRange.getEnd();
    if (otherEnd.compareTo(range.getEnd()) < 0)
    {
      range.setEnd(otherEnd);
    }
  }

  private T _start;
  private T _end;
}
