/*
 * Copyright  2001-2006 The Apache Software Foundation.
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

/*
**Test
*/

function EncodingFormat(
  length,
  messages
  )
{
  this._length   = length;
  this._messages = messages;
  this._class    = "EncodingFormat";
}

EncodingFormat.prototype = new TrValidator();

//LFS - Length failed summary
EncodingFormat.prototype.LFS  = "LFS";
//LF - Length failed
EncodingFormat.prototype.LF  = "LF";

function _cjkParse(
  parseString,
  label
  )
{
  var i = 0;
  var length = this._length;

  while (i < parseString.length)
  { 
    var ch = parseString.charCodeAt(i);
    if ((ch < 0x80) || ((0xFF60 < ch) && (ch < 0xFFA0))) length--; 
    else length -= 2;
   
    if (length < 0)
    {
      var facesMessage = _createFacesMessage( this._messages[this.LFS],
                                              this._messages[this.LF],
                                              label,
                                              parseString);   
      throw new TrValidatorException(facesMessage);     
    }

    i++;
  }

  return parseString;
}


function CjkFormat(
  length,
  messages
  )
{
  this._base = EncodingFormat;
  this._base(length, messages);
  this._class = "CjkFormat";
  
}


CjkFormat.prototype = new EncodingFormat();
CjkFormat.prototype.validate  = _cjkParse;

function _utf8Parse(
  parseString,
  label
  )
{
  var i = 0;
  var length = this._length;

  while (i < parseString.length)
  { 
    var ch = parseString.charCodeAt(i);
    if (ch < 0x80) length--;
    else if (ch < 0x800) length -= 2;
    else
    {
      // Surrogates;  see bug 3849516
      if ((ch & 0xF800) == 0xD800)
        length -= 2;
      else
        length -= 3;
    }

    if (length < 0)
    {
      var facesMessage = _createFacesMessage( this._messages[this.LFS],
                                              this._messages[this.LF],
                                              label,
                                              parseString);   
      throw new TrValidatorException(facesMessage);              
    }

    i++;
  }

  return parseString;
}


function Utf8Format(
  length,
  messages
  )
{
  this._base = EncodingFormat;
  this._base(length, messages);
  this._class = "Utf8Format";
}


Utf8Format.prototype = new EncodingFormat();
Utf8Format.prototype.validate  = _utf8Parse;



function _sbParse(
  parseString,
  label
  )
{
  if (this._length < parseString.length)
  {
    var facesMessage = _createFacesMessage( this._messages[this.LFS],
                                            this._messages[this.LF],
                                            label,
                                            parseString);   
    throw new TrValidatorException(facesMessage);      
  }

  return parseString;
}


function SBFormat(
  length,
  messages
  )
{
  this._base = EncodingFormat;
  this._base(length, messages);
  this._class = "SBFormat";
  
}


SBFormat.prototype = new EncodingFormat();
SBFormat.prototype.validate  = _sbParse;


