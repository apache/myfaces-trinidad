/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfdemo.composite;

import org.apache.myfaces.adfinternal.taglib.UIXEditableValueTag;


/**
 * NOTE: a clients may not extend UIXEditableValueTag (or
 * any other tag classes), as these are not part of the public
 * API (note the package);  I'm doing it for expedience here.
 */
public class DateFieldTag extends UIXEditableValueTag
{
  public String getComponentType()
  {
    return "org.apache.myfaces.adfdemo.DateField";
  }

  public String getRendererType()
  {
    return null;
  }
}
