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

function TrByteLengthValidator(
  length,
  summary,
  detail
  )
{
  this._length   = length;
  this._summary = summary;
  this._detail = detail;
  this._class    = "TrByteLengthValidator";
}

TrByteLengthValidator.prototype = new TrValidator();

function CjkFormat(
  length,
  summary,
  detail
  )
{
  this._base = TrByteLengthValidator;
  this._base(length, summary,detail);
  this._class = "CjkFormat";
  
}

CjkFormat.prototype = new TrByteLengthValidator();
CjkFormat.prototype.validate  = function(
  parseString,
  label,
  converter
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
      var facesMessage;
      if(this._summary == undefined)
      {
        facesMessage = _createFacesMessage( "org.apache.myfaces.trinidad.validator.ByteLengthValidator.MAXIMUM",
                                                label,
                                                parseString);
      }
      else
      {
        facesMessage = _createCustomFacesMessage(this._summary,
                                            this._detail,
                                            label,
                                            parseString);
      }
      throw new TrValidatorException(facesMessage);     
    }

    i++;
  }

  return parseString;
}




function Utf8Format(
  length,
  summary,
  detail
  )
{
  this._base = TrByteLengthValidator;
  this._base(length, summary,detail);
  this._class = "Utf8Format";
}


Utf8Format.prototype = new TrByteLengthValidator();
Utf8Format.prototype.validate  = function(
  parseString,
  label,
  converter
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
      var facesMessage;
      if(this._summary == undefined)
      {
        facesMessage = _createFacesMessage( "org.apache.myfaces.trinidad.validator.ByteLengthValidator.MAXIMUM",
                                                label,
                                                parseString);
      }
      else
      {
        facesMessage = _createCustomFacesMessage(this._summary,
                                            this._detail,
                                            label,
                                            parseString);
      }
      throw new TrValidatorException(facesMessage);              
    }

    i++;
  }

  return parseString;
}

function SBFormat(
  length,
  summary,
  detail
  )
{
  this._base = TrByteLengthValidator;
  this._base(length, summary,detail);
  this._class = "SBFormat";
  
}


SBFormat.prototype = new TrByteLengthValidator();
SBFormat.prototype.validate  = function(
  parseString,
  label,
  converter
  )
{
  if (this._length < parseString.length)
  {
      var facesMessage;
      if(this._summary == undefined)
      {
        facesMessage = _createFacesMessage( "org.apache.myfaces.trinidad.validator.ByteLengthValidator.MAXIMUM",
                                                label,
                                                parseString);
      }
      else
      {
        facesMessage = _createCustomFacesMessage(this._summary,
                                            this._detail,
                                            label,
                                            parseString);
      }
    throw new TrValidatorException(facesMessage);      
  }

  return parseString;
}