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
package org.apache.myfaces.trinidad.model;

import java.text.Collator;

/**
 * This class provides an enumeration to work with the integer values of the
 * {@link Collator} strength values.
 */
public enum SortStrength
{
  /** @see Collator#IDENTICAL */
  IDENTICAL(Collator.IDENTICAL),
  /** @see Collator#PRIMARY */
  PRIMARY(Collator.PRIMARY),
  /** @see Collator#SECONDARY */
  SECONDARY(Collator.SECONDARY),
  /** @see Collator#TERTIARY */
  TERTIARY(Collator.TERTIARY);

  private SortStrength(int strength)
  {
    _strength = strength;
  }

  /**
   * retrieves the strength, the integer strength value matches that of the {@link Collator}
   * @return strength value which matches {@link Collator} strength values.
   */
  public int getStrength()
  {
    return _strength;
  }

  private final int _strength;
}
