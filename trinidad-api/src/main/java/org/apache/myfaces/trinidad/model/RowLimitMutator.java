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

/**
 * Identifies a collection whose row limit can be mutated.
 */
public interface RowLimitMutator
{
  /**
   * Mutate the row limit of this collection, it returns the old row limit.
   * @param newLimit the new limit of this collection to set to.
   *   A positive number: the maximum number of rows the collection can hold.
   *   CollectionModel.UNKNOWN_ROW_LIMIT: row limit is unknown.
   *   CollectionModel.UNLIMITED_ROW: there is no limit
   * @returns the old row limit.
   * @throws IllegalArgumentException if <em>newLimit</em> is 0 or any negative number
   *   other than CollectionModel.UNKNOWN_ROW_LIMIT and CollectionModel.UNLIMITED_ROW.
   */
  public int setRowLimit(int newLimit);
}
