/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
function _getDateFieldFormat(dateField)
{
  var name = dateField.name;
  if (name && _dfs)
  {
    var format = _dfs[name];
    if (format)
      return new SimpleDateFormat(format);
  }

  return new SimpleDateFormat();
}

function _fixDFF(dateField)
{
  var format = _getDateFieldFormat(dateField);

  if (dateField.value != "")
  {
    try
    {
      var value = format.getAsObject(dateField.value);
      if (value != null)
        dateField.value = format.getAsString(value);
    }
    catch (e)
    {
      // no-op
    }
  }
}

