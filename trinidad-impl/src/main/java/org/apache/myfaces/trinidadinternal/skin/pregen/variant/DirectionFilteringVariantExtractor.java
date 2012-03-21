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
import java.util.List;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * A SkinVariantExtractor that filters LocaleContexts based on reading direction
 */
final class DirectionFilteringVariantExtractor implements SkinVariantExtractor<LocaleContext>
{
  /**
   * Creates a filtering variant extractor.
   * @param wrapped the LocaleContext variant extractor to filter
   * @param rtlOnly if true, getVariants() only returns rtl LocaleContexts.
   *        Otherwise, only ltr LocaleContexts are returned.
   */
  DirectionFilteringVariantExtractor(
    SkinVariantExtractor<LocaleContext> wrapped,
    boolean rtlOnly
    )
  {
    _wrapped = wrapped;
    _rtlOnly = rtlOnly;
  }

  @Override
  public List<LocaleContext> getVariants()
  {
    List<LocaleContext> locales = _wrapped.getVariants();
    List<LocaleContext> filteredLocales = new ArrayList<LocaleContext>(locales.size());
      
    for (LocaleContext locale : locales)
    {
      if (locale.isRightToLeft() == _rtlOnly)
      {
        filteredLocales.add(locale);
      }
    }
      
    return filteredLocales;
  }

  @Override
  public void visit(StyleSheetNode styleSheet)
  {
    _wrapped.visit(styleSheet);
  }
    
  private final SkinVariantExtractor<LocaleContext> _wrapped;
  private final boolean _rtlOnly;
}
