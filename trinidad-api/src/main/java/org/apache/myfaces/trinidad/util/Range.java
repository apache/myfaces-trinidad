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
package org.apache.myfaces.trinidad.util;

/**
 * Represents a range of comparable values with a specific start and end.
 * 
 * Range instances are constructed using the of() factory method.
 * 
 * Range instances are immutable.
 */
public final class Range<C extends Comparable>
{
  /**
   * Returns a new range consisting of the specified start and end points.
   * 
   * If start > end, an empty range is returned.
   * 
   * @throws IllegalArgumentException if start or end is null
   */
  public static <C extends Comparable> Range<C> of(C start, C end)
  {
    Args.notNull(start, "start");
    Args.notNull(end, "end");

    if (start.compareTo(end) > 0)
    {
      return emptyRange();
    }
    
    return new Range<C>(start, end);
  }

  /**
   * Returns an empty range.
   */
  public static <C extends Comparable> Range<C> emptyRange()
  {
    return (Range<C>)_EMPTY_RANGE;    
  }

  /**
   * Returns the start of the range.
   */
  public C getStart()
  {
    return _start;
  }

  /**
   * Returns the end of the range.
   */
  public C getEnd()
  {
    return _end;
  }
  
  /**
   * Tests whether this range is empty.
   * @return true if empty, false otherwise
   */
  public boolean isEmpty()
  {
    // Both start and end are null in the empty range (and
    // non-null in all other ranges).
    if (_start == null)
    {
      assert(_end == null);
      return true;
    }
    
    return false;
  }

  /**
   * Tests whether the specified value is contained within this range.
   * @param value the value to test
   * @return true if the value is within this range, false otherwise.
   */
  public boolean contains(C value)
  {
    if (isEmpty())
    {
      return false;
    }
    
    return (_start.compareTo(value) <= 0) && (_end.compareTo(value) >= 0);
  }

  /**
   * Computes the intersection of the this range with another range
   * and returns the result in a new Range.
   *
   * If there is no intersection between the two ranges, the empty
   * range (EMPTY_RANGE) is returned.
   * 
   * @param otherRange the (non-null) range to intersect/update
   */
  public Range<C> intersect(Range<C> otherRange)
  {
    Args.notNull(otherRange, "otherRange");
    
    if (isEmpty() || (otherRange.isEmpty()))
    {
      return emptyRange();
    }

    C start = _max(this.getStart(), otherRange.getStart());
    C end = _min(this.getEnd(), otherRange.getEnd());
    
    // start > end means there was no intersection
    if (start.compareTo(end) > 0)
    {
      return emptyRange();
    }
    
    return new Range<C>(start, end);
  }
  
  private static <C extends Comparable> C _min(C c1, C c2)
  {
    return (c1.compareTo(c2) > 0 ? c2 : c1);
  }

  private static <C extends Comparable> C _max(C c1, C c2)
  {
    return (c1.compareTo(c2) <  0 ? c2 : c1);
  }

  @Override  
  public String toString()
  {
    return getClass().getName() + "[" + _start + " - " + _end + "]";
  }

  @Override
  public boolean equals(Object o)
  {
    if (this == o)
    {
      return true;
    }
    
    if (!(o instanceof Range))
    {
      return false;
    }

    Range<C> otherRange = (Range<C>)o;

    // Short-circuit to simplify subsequent equals comparisons.
    if (isEmpty() || otherRange.isEmpty())
    {
      // Note: this assumes that empty ranges will be properly
      // resolved to the _EMPTY_RANGE instance during deserialization.
      // Which is something to keep in mind if we ever implement
      // Serializable.
      return (this == o);
    }

    return (getStart().equals(otherRange.getStart()) &&
              getEnd().equals(otherRange.getEnd()));
    
  }
  
  @Override
  public int hashCode()
  {
    int result = 17;
    
    if (!isEmpty())
    {
      assert(_start != null);
      assert(_end != null);

      result = 31 * result + _start.hashCode();
      result = 31 * result + _end.hashCode();
    }

    return result;
  }

  private Range(C start, C end)
  {
    _start = start;
    _end = end;
  }

  private final C _start;
  private final C _end;

  // Note: if we decide to make Range implement Serializable, be sure
  // that we properly handle deserialization to this singleton instance.
  private static final Range<? extends Comparable> _EMPTY_RANGE = 
    new Range<Comparable>(null, null);
}
