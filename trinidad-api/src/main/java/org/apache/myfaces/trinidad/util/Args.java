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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Argument processing utilities.
 */
public final class Args
{
  /**
   * Ensures that the specified argument is not null.  If
   * the value is null, an IllegalArgumentException is thrown.
   * @param arg the value to test
   * @param argName the name of the argument (for logging purposes)
   * @return the argument value, if not null
   */
  public static <T> T notNull(T arg, String argName)
  {
    if (arg == null)
    {
      throw new IllegalArgumentException(_getNotNullMessage(argName));
    }
    
    return arg;
  }

  private static String _getNotNullMessage(String argName)
  {
    return _LOG.getMessage("ILLEGAL_NULL_ARGUMENT", argName);
  }

  private Args() { }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(Args.class);
}