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

function NumberConverter(
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
  this._class = "NumberConverter";
}

NumberConverter.prototype = new TrConverter();


NumberConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

NumberConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return this._decimalParse(numberString, 
                       this._messages,
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}

NumberConverter.prototype._decimalParse = function(
  numberString,
  messages,
  maxPrecision,
  maxScale,
  maxValue,
  minValue,
  label
  )
{
  var facesMessage = null; 
  if (!numberString)
  { 
    facesMessage = _createFacesMessage( messages[(NumberConverter.D+ '_S')],
                                        messages[NumberConverter.D],
                                        label,
                                        numberString);
    throw new TrConverterException(facesMessage);
  }
       

  // Get LocaleSymbols (from Locale.js)
  var symbols = getLocaleSymbols();
  if (symbols)
  {
    // We don't want leading or trailing grouping separators
    var grouping = symbols.getGroupingSeparator();
    if ((numberString.indexOf(grouping) == 0) ||
        (numberString.lastIndexOf(grouping) ==  (numberString.length - 1)))
    {
      facesMessage =  _createFacesMessage( messages[(NumberConverter.D+ '_S')],
                                        messages[NumberConverter.D],
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

  // First, reject anything that's all spaces
  var i = numberString.length - 1;
  while (i >= 0)
  {
    if (numberString.charAt(i) != ' ')
      break;
    i--;
  }

  if (i >= 0)
  {
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
          messageKey = NumberConverter.LV;
        }
        else if ((minValue != (void 0)) &&
                 (result  < minValue))
        {
          messageKey = NumberConverter.MV;
        }
        else if ((maxPrecision != (void 0)) &&
                 (integerDigits  > maxPrecision))
        {
          messageKey = NumberConverter.LID;
        }
        else if ((maxScale != (void 0)) &&
                 (fractionDigits  > maxScale))
        {
          messageKey = NumberConverter.LFD;
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
  }

  facesMessage = _createFacesMessage( messages[(NumberConverter.D+ '_S')],
                                        messages[NumberConverter.D],
                                        label,
                                        numberString);
  throw new TrConverterException(facesMessage);
}


// Less fraction digits
NumberConverter.LFD = 'LFD';
// Less integer digits
NumberConverter.LID = 'LID';
// Less value
NumberConverter.LV  = 'LV';
// More value
NumberConverter.MV  = 'MV';
// default
NumberConverter.D   = 'D';


function _decimalValidate(
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

function DecimalValidator(
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
  this._class = "DecimalValidator";
}

DecimalValidator.prototype = new TrValidator();
DecimalValidator.prototype.validate  = _decimalValidate;


function _regExpParse(
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
    var facesMessage = _createFacesMessage( this._messages[RegExpFormat.NMS],
                                            this._messages[RegExpFormat.NM],
                                            label,
                                            parseString,
                                            this._pattern);                                          
    throw new TrValidatorException(facesMessage); 
  }
}


function RegExpFormat(
  pattern,
  messages
  )
{  
  this._pattern  = pattern;
  this._messages = messages;
  this._class = "RegExpFormat";
}

// no match pattern
RegExpFormat.NM = 'NM';
// no match pattern summary
RegExpFormat.NMS = 'NMS';

RegExpFormat.prototype = new TrValidator();
RegExpFormat.prototype.validate  = _regExpParse;
