/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

function ssnGetAsString(value, label)
{
  return value.substring(0,3) + '-' + value.substring(3,5) + '-' + value.substring(5);
}

function ssnGetAsObject(value, label)
{ 
  if (!value)return null;
  var len=value.length;
  var messageKey = SSNConverter.NOT;
  if (len < 9 )
    messageKey = SSNConverter.SHORT;
  else if (len > 11)
    messageKey = SSNConverter.LONG;
  else if (len == 9)
  { if (!isNaN(value))
      return value;
  }
  else if (len == 11 && value.charAt(3) == '-' && 
            value.charAt(6) == '-')
  {
    var result = value.substring(0,3) + value.substring(4,6) + 
                value.substring(7);
    if (!isNaN(result))
      return result;
  }
  if (messageKey!=null && this._messages!=null)
  { 
    // format the detail error string
    var detail = this._messages[messageKey];
    if (detail != null)
    {
      detail = TrFastMessageFormatUtils.format(detail, label, value);
    }
  
    var facesMessage = new TrFacesMessage(
                        this._messages[SSNConverter.SUMMARY],
                        detail,
                        TrFacesMessage.SEVERITY_ERROR)
   throw new TrConverterException(facesMessage);
 }
 return null;
}
function SSNConverter(messages)
  {this._messages = messages;}
SSNConverter.prototype = new TrConverter();
SSNConverter.prototype.getAsString = ssnGetAsString;
SSNConverter.prototype.getAsObject = ssnGetAsObject;
SSNConverter.SUMMARY = 'SUM';
SSNConverter.SHORT = 'S';
SSNConverter.LONG  = 'L';
SSNConverter.NOT   = 'N';
