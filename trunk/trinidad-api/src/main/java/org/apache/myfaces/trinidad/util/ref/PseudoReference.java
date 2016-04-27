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
package org.apache.myfaces.trinidad.util.ref;

/**
 * <p>
 * Interface for dealing with both strong references and java.lang.ref.Reference Objects
 * interchangably.  If Java supported a StrongReference subclass of Reference, this interface would
 * not be necessary.
 * </p>
 */
public interface PseudoReference<T>
{
  /**
   * Returns this reference object's referent.  If this reference object has
   * been cleared, either by the program or by the garbage collector, then
   * this method returns <code>null</code>.
   *
   * @return   The object to which this reference refers, or
   *           <code>null</code> if this reference object has been cleared
   */
  public T get();

  /**
   * Clears this reference object.  Invoking this method will not cause this
   * object to be enqueued.
   *
   * <p> This method is invoked only by Java code; when the garbage collector
   * clears references it does so directly, without invoking this method.
   */
  public void clear();
  
  /**
   * Tells whether or not this reference object has been enqueued, either by
   * the program or by the garbage collector.  If this reference object was
   * not registered with a queue when it was created, then this method will
   * always return <code>false</code>.
   *
   * @return   <code>true</code> if and only if this reference object has
   *           been enqueued
   */
  public boolean isEnqueued();
  
  /**
   * Adds this reference object to the queue with which it is registered,
   * if any.
   *
   * <p> This method is invoked only by Java code; when the garbage collector
   * enqueues references it does so directly, without invoking this method.
   *
   * @return   <code>true</code> if this reference object was successfully
   *           enqueued; <code>false</code> if it was already enqueued or if
   *           it was not registered with a queue when it was created
   */
  public boolean enqueue();
}
