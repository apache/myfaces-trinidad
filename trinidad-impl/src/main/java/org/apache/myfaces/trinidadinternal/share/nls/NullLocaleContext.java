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
package org.apache.myfaces.trinidadinternal.share.nls;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.TimeZone;

import org.apache.myfaces.trinidad.context.LocaleContext;

/**
 * LocaleContext implementation for cases where the Locale is 
 * null/unknown.
 */
public final class NullLocaleContext extends LocaleContext
{
  public static LocaleContext getLeftToRightContext()
  {
    return _LTR_INSTANCE;      
  }

  public static LocaleContext getRightToLeftContext()
  {
    return _RTL_INSTANCE;
  }

  @Override
  public Locale getFormattingLocale()
  {
    return null;
  }

  @Override
  public String getFormattingIANALocaleString()
  {
    return null;
  }

  @Override
  public Locale getTranslationLocale()
  {
    return null;
  }

  @Override
  public String getTranslationIANALocaleString()
  {
    return null;
  }

  @Override
  public boolean isRightToLeft()
  {
    return _rightToLeft;
  }

  @Override
  public TimeZone getTimeZone()
  {
    return null;
  }

  @Override
  public ResourceBundle getBundle(String baseBundleName)
  {
    return null;
  }

  @Override
  public int getTwoDigitYearStart()
  {
    return 0;
  }

  @Override
  public char getGroupingSeparator()
  {
    return 0;
  }

  @Override
  public char getDecimalSeparator()
  {
    return 0;
  }
  
  private NullLocaleContext(boolean rightToLeft)
  {
    _rightToLeft = rightToLeft;
  }
  
  private boolean _rightToLeft;
  
  private static final LocaleContext _RTL_INSTANCE = new NullLocaleContext(true);
  private static final LocaleContext _LTR_INSTANCE = new NullLocaleContext(false);
}
