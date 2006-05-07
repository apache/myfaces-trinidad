/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfinternal.binding;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.adfinternal.util.nls.StringUtils;


/**
 * ValueBinding that wraps a second value binding to strip
 * the mnemonic.
 *
 * @author The Oracle ADF Faces Team
 */
public class StripAccessKeyBinding extends ValueBindingAdapter
{
  /**
   * Constructor purely for serialization.
   */
  public StripAccessKeyBinding()
  {
    super(null);
  }

  public StripAccessKeyBinding(ValueBinding base)
  {
    super(base);
  }

  public Object getValue(FacesContext context)
  {
    Object o = super.getValue(context);
    if (o == null)
      return null;

    String text = o.toString();
    int accessKeyIndex = StringUtils.getMnemonicIndex(text);
    if (accessKeyIndex == StringUtils.MNEMONIC_INDEX_NONE)
      return text;

    return StringUtils.stripMnemonic(text);
  }

  public Class getType(FacesContext context)
  {
    return String.class;
  }
}
