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
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * Trivial SkinVariantExtractor implementation that returns a fixed
 * value.
 * 
 * Used by SkinVariants in cases where the variants to pregenerate
 * are explicitly configured.
 */
final class FixedVariantExtractor <T> implements SkinVariantExtractor
{

  /**
   * Returns a SkinVariantExtractor for single value.
   * 
   * @param defaultValue the value that the created SkinVariantExtractor
   *   will return from getVariants().
   */
  public static <T> SkinVariantExtractor<T> extractor(T defaultValue)
  {
    return new FixedVariantExtractor<T>(Arrays.asList(defaultValue));
  }

  /**
   * Returns a SkinVariantExtractor for a collection of values.
   *
   * @param values the values that the created SkinVariantExtractor
   *   will return from getVariants.
   */
  public static <T> SkinVariantExtractor<T> extractor(Collection<T> values)
  {
    return new FixedVariantExtractor<T>(new ArrayList<T>(values));
  }

  @Override
  public List<T> getVariants()
  {
    return _values;
  }

  @Override
  public void visit(StyleSheetNode styleSheet)
  {
    // This is a no-op since we're just going to return the fixed
    // set of values that was passed into the constructor.
  }

  private FixedVariantExtractor(
    List<T> values)
  {
    _values = Collections.unmodifiableList(values);
  }
  
  private final List<T> _values;
}
