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
package org.apache.myfaces.adf.util;

// CLASS  for way of sending back the translated string for 
// given key. There are two messages for the key. One for the given 
// key and other of key_detail.
class ErrorMessages
{
  ErrorMessages(String message, String detailMessage)
  {
    _message = message;
    _detailMessage = detailMessage;    
  }
  
  public String getMessage()
  {
    return _message;
  }
  
  public String getDetailMessage()
  {
    return _detailMessage;
  }
  
  private String _message;
  private String _detailMessage;
}
