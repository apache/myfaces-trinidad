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

function TrNumberConverter(
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._messages = messages;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrNumberConverter";
}

TrNumberConverter.prototype = new TrConverter();


TrNumberConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrNumberConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._messages,
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}



// Less fraction digits
TrNumberConverter.LFD = 'LFD';
// Less integer digits
TrNumberConverter.LID = 'LID';
// Less value
TrNumberConverter.LV  = 'LV';
// More value
TrNumberConverter.MV  = 'MV';
// default
TrNumberConverter.D   = 'D';


function TrRangeValidator(
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._messages = messages;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrRangeValidator";
}

TrRangeValidator.prototype = new TrValidator();
TrRangeValidator.prototype.validate  = function(
  value,
  label
)
{

  // This should probably do more than call decimalParse!
  // the following line is needed because what's being passed
  // into the validator is a number, and _decimalParse expects a string.
  numberString = "" + value;
  try
  {
    return _decimalParse(numberString, 
                       this._messages,
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
  }
  catch (e)
  {
    throw new TrValidatorException(e.getFacesMessage());
  }
}

function TrLengthValidator(
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
 
  this._messages = messages;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrLengthValidator";
}

TrLengthValidator.prototype = new TrValidator();
TrLengthValidator.prototype.validate  = function(
  value,
  label
)
{

  // This should probably do more than call decimalParse!
  // the following line is needed because what's being passed
  // into the validator is a number, and _decimalParse expects a string.
  string = "" + value;
  length = string.length;
  
  if(length >= this._minValue && length <= this._maxValue)
  {
    return string;
  }
  else
  {
    facesMessage = _createFacesMessage(this._messages[(TrNumberConverter.D+ '_S')],
                                       this._messages[TrNumberConverter.D],
                                        label,
                                        string);
    throw new TrConverterException(facesMessage);
  }
}

function TrDateTimeRangeValidator(
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
 
  this._messages = messages;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrDateTimeRangeValidator";
}

TrDateTimeRangeValidator.prototype = new TrValidator();
TrDateTimeRangeValidator.prototype.validate  = function(
  value,
  label
)
{
  dateTime = value.getTime();
  minDate = parseInt(this._minValue);
  maxDate = parseInt(this._maxValue);
  
  if(dateTime >= minDate && dateTime <= maxDate)
  {
    return value;
  }
  else
  {
    facesMessage = _createFacesMessage(this._messages[(TrNumberConverter.D+ '_S')],
                                       this._messages[TrNumberConverter.D],
                                        label,
                                        ""+value);
    throw new TrConverterException(facesMessage);
  }
  
}

function _decimalParse(
  numberString,
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue,
  label
  )
{

  // The following are from the javadoc for TrNumberConverter
  // If the specified String is null, return a null. Otherwise, trim leading and trailing whitespace before proceeding.
  // If the specified String - after trimming - has a zero length, return null.
  if (numberString == null)
    return null;
    
  numberString = TrUIUtils.trim(numberString);
  if (numberString.length == 0)
    return null
    
  var facesMessage = null;        

  // Get LocaleSymbols (from Locale.js)
  var symbols = getLocaleSymbols();
  if (symbols)
  {
    // We don't want leading or trailing grouping separators
    var grouping = symbols.getGroupingSeparator();
    if ((numberString.indexOf(grouping) == 0) ||
        (numberString.lastIndexOf(grouping) ==  (numberString.length - 1)))
    {
      facesMessage =  _createFacesMessage( messages[(TrNumberConverter.D+ '_S')],
                                        messages[TrNumberConverter.D],
                                        label,
                                        numberString);
      throw new TrConverterException(facesMessage);
    }

    // Remove the thousands separator - which Javascript doesn't want to see
    var thousands = new RegExp("\\" + grouping, "g");
    numberString = numberString.replace(thousands, "");
    // Then change the decimal separator into a period, the only
    // decimal separator allowed by JS
    var decimal = new RegExp("\\" + symbols.getDecimalSeparator(),  "g");
    numberString = numberString.replace(decimal, ".");
  }


  // OK; it's non-empty.  Now, disallow exponential
  // notation, and then use some JS magic to exclude
  // non-numbers
  if ((numberString.indexOf('e') < 0) &&
      (numberString.indexOf('E') < 0) &&
      (((numberString * numberString) == 0) ||
       ((numberString / numberString) == 1)))
  {
    var result = parseFloat(numberString);
    if (!isNaN(result))
    {
      var integerDigits = numberString.length;
      var fractionDigits = 0;

      var sepIndex = numberString.lastIndexOf('.');
      if (sepIndex != -1)
      {
        integerDigits = sepIndex;
        fractionDigits = numberString.length - sepIndex -1;
      }
      
      var messageKey;
      
      if ((maxValue != (void 0)) &&
          (result  > maxValue))
      {
        messageKey = TrNumberConverter.LV;
      }
      else if ((minValue != (void 0)) &&
               (result  < minValue))
      {
        messageKey = TrNumberConverter.MV;
      }
      else if ((maxPrecision != (void 0)) &&
               (integerDigits  > maxPrecision))
      {
        messageKey = TrNumberConverter.LID;
      }
      else if ((maxScale != (void 0)) &&
               (fractionDigits  > maxScale))
      {
        messageKey = TrNumberConverter.LFD;
      }

      if (messageKey != (void 0))
      {
        var messages = messages;
        
        if ((messages == (void 0)) ||
            (messages[messageKey] == (void 0)))
          throw  new TrConverterException(null, null, "Conversion failed, but no appropriate message found");  // default error format
        else
        {
          facesMessage =  _createFacesMessage( messages[(messageKey + '_S')],
                                      messages[messageKey],
                                      label,
                                      numberString);
          throw new TrConverterException(facesMessage);
        }
       }
      return result;
    }
  }

  facesMessage = _createFacesMessage( messages[(TrNumberConverter.D+ '_S')],
                                        messages[TrNumberConverter.D],
                                        label,
                                        numberString);
  throw new TrConverterException(facesMessage);
}

function TrRegExpValidator(
  pattern,
  messages
  )
{  
  this._pattern  = pattern;
  this._messages = messages;
  this._class = "TrRegExpValidator";
}

// no match pattern
TrRegExpValidator.NM = 'NM';
// no match pattern summary
TrRegExpValidator.NMS = 'NMS';

TrRegExpValidator.prototype = new TrValidator();
TrRegExpValidator.prototype.validate  = function(
  parseString,
  label
  )
{
  //For some reason when using digits as input values 
  // parseString becomes a integer type, so get away with it.  
  parseString = parseString + '';
  
  var matchArr = parseString.match(this._pattern); 
        
  if ((matchArr != (void 0)) && (matchArr[0] == parseString))
  {
    return parseString;
  }
  else
  {    
    var facesMessage = _createFacesMessage( this._messages[TrRegExpValidator.NMS],
                                            this._messages[TrRegExpValidator.NM],
                                            label,
                                            parseString,
                                            this._pattern);                                          
    throw new TrValidatorException(facesMessage); 
  }
}