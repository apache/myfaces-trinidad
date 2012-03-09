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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.TreeSet;

import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidadinternal.share.nls.LocaleContextImpl;
import org.apache.myfaces.trinidadinternal.share.nls.NullLocaleContext;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

/**
 * An @-rule processor for extracting @locale rule metadata.
 */
final class LocaleVariantExtractor implements SkinVariantExtractor<LocaleContext>
{
  public LocaleVariantExtractor()
  {
    _locales = new TreeSet<Locale>(_LOCALE_COMPARATOR);
  }
  
  @Override
  public void visit(StyleSheetNode node)
  {
    Collection<Locale> nodeLocales = node.getLocales();
    
    for (Locale locale : nodeLocales)
    {
      _locales.add(locale);
    }    
  }

  /**
   * Returns un unmodifiable list containing LocaleContexts corresponding
   * to all processed @locale rules.
   */
  public List<LocaleContext> getVariants()
  {
    List<LocaleContext> localeContexts = _toLocaleContexts(_locales);
    return Collections.unmodifiableList(localeContexts);
  }

  // Converts from Locale to LocaleContext, from Collection -> List, and
  // adds in entries for null locales.
  private static List<LocaleContext> _toLocaleContexts(Collection<Locale> locales)
  {
    int size = locales.size() + 2;
    List<LocaleContext> localeContexts = new ArrayList<LocaleContext>(size);
    _addNullLocaleContexts(localeContexts);
    _addLocaleContexts(localeContexts, locales);
    
    return localeContexts;
  }

  private static void _addNullLocaleContexts(Collection<LocaleContext> localeContexts)
  {
    localeContexts.add(NullLocaleContext.getLeftToRightContext());
    localeContexts.add(NullLocaleContext.getRightToLeftContext());
  }
  
  private static void _addLocaleContexts(
    Collection<LocaleContext> localeContexts,
    Collection<Locale>        locales
    )
  {
    for (Locale locale : locales)
    {
      localeContexts.add(new LocaleContextImpl(locale));
    }
  }

  private static final class LocaleComparator implements Comparator<Locale>
  {
    @Override
    public int compare(Locale locale1, Locale locale2)
    {
      return locale1.toString().compareTo(locale2.toString());
    }
  }

  private final Collection<Locale> _locales;

  private static final Comparator<Locale> _LOCALE_COMPARATOR = new LocaleComparator();
}
