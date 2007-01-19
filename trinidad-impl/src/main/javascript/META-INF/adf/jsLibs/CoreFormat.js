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
function TrIntegerConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrIntegerConverter";
}

TrIntegerConverter.prototype = new TrConverter();

TrIntegerConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.IntegerConverter.FORMAT_HINT",
	  null);
}

TrIntegerConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrIntegerConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.IntegerConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}
function TrLongConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrLongConverter";
}

TrLongConverter.prototype = new TrConverter();

TrLongConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.LongConverter.FORMAT_HINT",
	  null);
}

TrLongConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrLongConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.LongConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}
function TrShortConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrShortConverter";
}

TrShortConverter.prototype = new TrConverter();

TrShortConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.ShortConverter.FORMAT_HINT",
	  null);
}

TrShortConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrShortConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.ShortConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}
function TrByteConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrByteConverter";
}

TrByteConverter.prototype = new TrConverter();

TrByteConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.ByteConverter.FORMAT_HINT",
	  null);
}

TrByteConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrByteConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.ByteConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}

function TrDoubleConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrDoubleConverter";
}

TrDoubleConverter.prototype = new TrConverter();

TrDoubleConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.DoubleConverter.FORMAT_HINT",
	  null);
}

TrDoubleConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrDoubleConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.DoubleConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}
function TrFloatConverter(
  message,
  maxPrecision,
  maxScale,
  maxValue,
  minValue)
{
  this._message = message;
  this._maxPrecision = maxPrecision;
  this._maxScale = maxScale;
  this._maxValue = maxValue;
  this._minValue = minValue;

  // for debugging
  this._class = "TrFloatConverter";
}

TrFloatConverter.prototype = new TrConverter();

TrFloatConverter.prototype.getFormatHint = function()
{
	return TrMessageFactory.createMessage(
    "org.apache.myfaces.trinidad.convert.FloatConverter.FORMAT_HINT",
	  null);
}

TrFloatConverter.prototype.getAsString = function(
  number,
  label
  )
{
  return "" + number;
}

TrFloatConverter.prototype.getAsObject = function(
  numberString,
  label
  )
{
  return _decimalParse(numberString, 
                       this._message,
                       "org.apache.myfaces.trinidad.convert.FloatConverter",
                       this._maxPrecision,
                       this._maxScale,
                       this._maxValue,
                       this._minValue,
                       label);
}


function TrRangeValidator(
  maxValue,
  minValue,
  messages)
{
  this._maxValue = maxValue;
  this._minValue = minValue;
  this._messages = messages;

  // for debugging
  this._class = "TrRangeValidator";
}

TrRangeValidator.prototype = new TrValidator();
TrRangeValidator.prototype.getHints = function(
  converter
  )
{
  return _returnRangeHints(
    this._messages,
    this._maxValue,
    this._minValue,
    "org.apache.myfaces.trinidad.validator.RangeValidator.MAXIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.RangeValidator.MINIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.RangeValidator.RANGE_HINT",
    "hintMax",
    "hintMin",
    "hintRange"
  );
}
TrRangeValidator.prototype.validate  = function(
  value,
  label,
  converter
)
{
  string = "" + value;
  numberValue = parseFloat(string);
  var facesMessage;
  if(this._minValue && this._maxValue)
  {
  	//range
    if(numberValue >= this._minValue && numberValue <= this._maxValue)
    {
      return string;
    }
    else
    {
    	var key = "org.apache.myfaces.trinidad.validator.LongRangeValidator.NOT_IN_RANGE";
    	if(this._messages && this._messages["range"])
    	{
        facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["range"],
                                        label,
                                        string,
                                        ""+this._minValue,
                                        ""+this._maxValue);
    	}
    	else
    	{
        facesMessage = _createFacesMessage(key,
                                        label,
                                        string,
                                        ""+this._minValue,
                                        ""+this._maxValue);
    	}
    }
  }
  else
  {
  	//only min
  	if(this._minValue)
  	{
  		if(numberValue >= this._minValue)
  		{
  			return string;
  		}
  		else
  		{
        var key = "org.apache.myfaces.trinidad.validator.LongRangeValidator.MINIMUM";
        if(this._messages && this._messages["min"])
        {
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["min"],
                                        label,
                                        string,
                                        ""+this._minValue);
        }
        else
        {
          facesMessage = _createFacesMessage(key,
                                        label,
                                        string,
                                        ""+this._minValue);
        }
  		}
  	}
  	//max only
  	else
  	{
  		if(numberValue <= this._maxValue)
  		{
  			return string;
  		}
  		else
  		{
        var key = "org.apache.myfaces.trinidad.validator.LongRangeValidator.MAXIMUM";
        if(this._messages && this._messages["max"])
        {
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["max"],
                                        label,
                                        string,
                                        ""+this._maxValue);
        }
        else
        {
          facesMessage = _createFacesMessage(key,
                                        label,
                                        string,
                                        ""+this._maxValue);
        }
  		}
  	}
  }
  throw new TrConverterException(facesMessage);
}

function TrLengthValidator(
  maxValue,
  minValue,
  messages)
{
 
  this._maxValue = maxValue;
  this._minValue = minValue;
  this._messages = messages;

  // for debugging
  this._class = "TrLengthValidator";
}

TrLengthValidator.prototype = new TrValidator();
TrLengthValidator.prototype.getHints = function(
  converter
  )
{
  return _returnRangeHints(
    this._messages,
    this._maxValue,
    this._minValue,
    "org.apache.myfaces.trinidad.validator.LengthValidator.MAXIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.LengthValidator.MINIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.LengthValidator.RANGE_HINT",
    "hintMax",
    "hintMin",
    "hintRange"
  );
}
TrLengthValidator.prototype.validate  = function(
  value,
  label,
  converter
)
{

  string = "" + value;
  length = string.length;
  
  if(length >= this._minValue && length <= this._maxValue)
  {
    return string;
  }
  else
  {
    if(length < this._minValue) //to short
    {
    	var key = "org.apache.myfaces.trinidad.validator.LengthValidator.MINIMUM";
    	var facesMessage;
    	if(this._messages && this._messages["min"])
    	{
        facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["min"],
                                        label,
                                        string,
                                        ""+this._minValue);
    	}
    	else
    	{
        facesMessage = _createFacesMessage(key,
                                        label,
                                        string,
                                        ""+this._minValue);
    	}
      throw new TrConverterException(facesMessage);
    }
    if(length > this._maxValue) //to long
    {
      var key = "org.apache.myfaces.trinidad.validator.LengthValidator.MAXIMUM";
      var facesMessage;
      if(this._messages && this._messages["max"])
      {
        facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["max"],
                                        label,
                                        string,
                                        ""+this._maxValue);
      }
      else
      {
        facesMessage = _createFacesMessage(key,
                                        label,
                                        string,
                                        ""+this._maxValue);
      }
      throw new TrConverterException(facesMessage);
    }
  }
}

function TrDateTimeRangeValidator(
  maxValue,
  minValue,
  messages)
{
  this._maxValue = maxValue;
  this._minValue = minValue;
  this._messages = messages;
  // for debugging
  this._class = "TrDateTimeRangeValidator";
}

TrDateTimeRangeValidator.prototype = new TrValidator();
TrDateTimeRangeValidator.prototype.getHints = function(
  converter
  )
{
  return _returnRangeHints(
    this._messages,
    converter.getAsString(new Date(this._maxValue)),
    converter.getAsString(new Date(this._minValue)),
    "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.MAXIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.MINIMUM_HINT",
    "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.RANGE_HINT",
    "hintMax",
    "hintMin",
    "hintRange"
  );
}
TrDateTimeRangeValidator.prototype.validate  = function(
  value,
  label,
  converter
)
{
  dateTime = value.getTime();
  var facesMessage;
  //range
  if(this._minValue && this._maxValue)
  {
    minDate = parseInt(this._minValue);
    maxDate = parseInt(this._maxValue);
    if(dateTime >= minDate && dateTime <= maxDate)
    {
      return value;
    }
    else
    {
    	var key = "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.NOT_IN_RANGE";
    	if(this._messages && this._messages["range"])
        {
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["range"],
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._minValue)),
                                        ""+converter.getAsString(new Date(this._maxValue)));
        }
    	else
    	{
          facesMessage = _createFacesMessage(key,
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._minValue)),
                                        ""+converter.getAsString(new Date(this._maxValue)));
    	}
    }
  }
  else
  {
    //only min
    if(this._minValue)
    {
      minDate = parseInt(this._minValue);
      if(dateTime >= minDate)
      {
        return value;
      }
      else
      {
        var key = "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.MINIMUM";
    	if(this._messages && this._messages["min"])
        {
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["min"],
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._minValue)));
        }
    	else
    	{
          facesMessage = _createFacesMessage(key,
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._minValue)));
    	}
      }
    }
    //max only
    else
    {
      maxDate = parseInt(this._maxValue);
      if(dateTime <= maxDate)
      {
        return value;
      }
      else
      {
        var key = "org.apache.myfaces.trinidad.validator.DateTimeRangeValidator.MAXIMUM";
    	if(this._messages && this._messages["max"])
        {
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["max"],
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._maxValue)));
        }
    	else
    	{
          facesMessage = _createFacesMessage(key,
                                        label,
                                        ""+converter.getAsString(value),
                                        ""+converter.getAsString(new Date(this._maxValue)));
    	}
      }
    }
  }
  throw new TrConverterException(facesMessage);
}

function TrDateRestrictionValidator(
  weekdaysValue,
  monthValue,
  messages)
  
{
  this._weekdaysValue = weekdaysValue;
  this._monthValue = monthValue;
  this._messages = messages;
  this._weekdaysMap = {'2':'tue','4':'thu','6':'sat','1':'mon','3':'wed','5':'fri','0':'sun'};
  this._translatedWeekdaysMap = {'sun':'0','mon':'1','tue':'2','wed':'3','thu':'4','fri':'5','sat':'6'};
  this._monthMap = {'2':'mar','4':'may','9':'oct','8':'sep','11':'dec','6':'jul','1':'feb','3':'apr','10':'nov','7':'aug','5':'jun','0':'jan'};
  this._translatedMonthMap = {'jan':'0','feb':'1','mar':'2','apr':'3','may':'4','jun':'5','jul':'6','aug':'7','sep':'8','oct':'9','nov':'10','dec':'11'};

  // for debugging
  this._class = "TrDateRestrictionValidator";
}

TrDateRestrictionValidator.prototype = new TrValidator();
TrDateRestrictionValidator.prototype.getHints = function(
  converter
  )
{
  return _returnHints(
    this._messages,
    this._translate(this._weekdaysValue, this._translatedWeekdaysMap, converter.getLocaleSymbols().getWeekdays()),
    this._translate(this._monthValue, this._translatedMonthMap, converter.getLocaleSymbols().getMonths()),
    "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.WEEKDAY_HINT",
    "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.MONTH_HINT",
    "hintWeek",
    "hintMonth"
  );
}
TrDateRestrictionValidator.prototype._translate = function(
  values,
  map,
  valueArray
  )
{
	if(values)
	{
    var translatedValues = new Array();
    var valuesAsArray = eval(values);
    for(i = 0; i<valuesAsArray.length; i++)
    {
      translatedValues.push(valueArray[map[valuesAsArray[i].toLowerCase()]]);
    }
    return eval(translatedValues);
	}
	else
	{
    return values;
	}
}
TrDateRestrictionValidator.prototype.validate  = function(
  value,
  label,
  converter
)
{
  submittedDay = value.getDay();
  weekDaysArray = eval(this._weekdaysValue);
  if(weekDaysArray)
  {
  	var dayString = this._weekdaysMap[submittedDay];
  	for(var i = 0; i < weekDaysArray.length; ++i)
  	{
  		if(weekDaysArray[i].toLowerCase() == dayString)
  		{
  			var facesMessage;
  			var key = "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.WEEKDAY";
  			if(this._messages && this._messages["days"])
  			{
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["days"],
                                        label,
                                        ""+converter.getAsString(value),
                                        dayString);
  			}
  			else
  			{
          facesMessage = _createFacesMessage(key,
                                        label,
                                        ""+converter.getAsString(value),
                                        dayString);
  			}
        throw new TrConverterException(facesMessage);
  		}
  	}
  }
  
  submittedMonth = value.getMonth();
  monthArray = eval(this._monthValue);
  if(monthArray)
  {
  	var monthString = this._monthMap[submittedMonth];
  	for(var i = 0; i < monthArray.length; ++i)
  	{
  		if(monthArray[i].toLowerCase() == monthString)
  		{
  			var facesMessage;
  			var key = "org.apache.myfaces.trinidad.validator.DateRestrictionValidator.MONTH";
  			if(this._messages && this._messages["month"])
  			{
          facesMessage = _createCustomFacesMessage(TrMessageFactory.getSummaryString(key),
                                        this._messages["month"],
                                        label,
                                        ""+converter.getAsString(value),
                                        monthString);
  			}
  			else
  			{
          facesMessage = _createFacesMessage(key,
                                        label,
                                        ""+converter.getAsString(value),
                                        monthString);
  			}
        throw new TrConverterException(facesMessage);
  		}
  	}
  }
	return value;
}

function _decimalParse(
  numberString,
  message,
  standardKey,
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
      if(message == null)
      {
        facesMessage =  _createFacesMessage( standardKey+".CONVERT",
                                          label,
                                          numberString);
      }
      else
      { 
        facesMessage =  _createFacesMessage( standardKey+".CONVERT",
                                          label,
                                          numberString);
        //var summary = "";
        //var detail = "";
        //facesMessage =  _createCustomMessage( summary,
                                          //detail,
                                          //label,
                                          //numberString);
      }
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
        fractionDigits = parseInt(numberString.length - parseInt(sepIndex -1));
      }
      
      var messageKey;
      //not true for float/double converter
      if ((maxValue != null) &&
          (result  > maxValue))
      {
        messageKey = standardKey+".MAXIMUM";
      }
      else if ((minValue != null) &&
               (result  < minValue))
      {
        messageKey = standardKey+".MINIMUM";
      }

      if (messageKey != null)
      {
        var messages = messages;
        if ((messages == null) ||
            (messages[messageKey] == null))
          throw  new TrConverterException(null, null, "Conversion failed, but no appropriate message found");  // default error format
        else
        {
          facesMessage =  _createFacesMessage( messageKey,
                                      label,
                                      numberString);
          throw new TrConverterException(facesMessage);
        }
       }
      return result;
    }
  }

  facesMessage = _createFacesMessage( standardKey+".CONVERT",
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

TrRegExpValidator.prototype = new TrValidator();
TrRegExpValidator.prototype.getHints = function(
  converter
  )
{
  var hints = new Array();
  if(this._messages["hint"])
  {
    hints.push(TrMessageFactory.createCustomMessage(
      this._messages["hint"],
	    ""+this._pattern)
	  );
  }
  else
  {
    hints.push(TrMessageFactory.createMessage(
      "org.apache.myfaces.trinidad.validator.RegExpValidator.NO_MATCH_HINT",
	    ""+this._pattern)
	  );
  }
	return hints;
}
TrRegExpValidator.prototype.validate  = function(
  parseString,
  label,
  converter
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
  	var key = "org.apache.myfaces.trinidad.validator.RegExpValidator.NO_MATCH";
    var facesMessage;
    if(this._messages && this._messages["detail"])
    {
      facesMessage = _createCustomFacesMessage(
                                         TrMessageFactory.getSummaryString(key),
                                         this._messages["detail"],
                                         label,
                                         parseString,
                                         this._pattern);
    }
    else
    {
      facesMessage = _createFacesMessage(key,
                                         label,
                                         parseString,
                                         this._pattern);                                          
    }
    throw new TrValidatorException(facesMessage); 
  }
}

function _returnRangeHints(
  messages,
  max,
  min,
  maxKey,
  minKey,
  rangeKey,
  maxHint,
  minHint,
  rangeHint
)
{
  
  //we have both, max and min, so we only use the range Hint
  if(max && min)
  {
  	var hints = new Array();
    if(messages && messages[rangeHint])
    {
      hints.push(
        TrMessageFactory.createCustomMessage(
        messages[rangeHint],
        ""+min,
        ""+max)
  	  );
    }
    else
    {
      hints.push(
        TrMessageFactory.createMessage(
        rangeKey,
        ""+min,
	      ""+max)
	    );
    }
    return hints;
  }
  
  return _returnHints(
    messages,
    max,
    min,
    maxKey,
    minKey,
    maxHint,
    minHint
  );
  
}
function _returnHints(
  messages,
  max,
  min,
  maxKey,
  minKey,
  maxHint,
  minHint
)
{
  var hints;
  if(max)
  {
    hints = new Array();
    if(messages && messages[maxHint])
    {
      hints.push(
        TrMessageFactory.createCustomMessage(
          messages[maxHint],
	        ""+max)
	    );
    }
    else
    {
      hints.push(
        TrMessageFactory.createMessage(
          maxKey,
	        ""+max)
	    );
    }
    
  }
  if(min)
  {
    if(!hints)
    {
      hints = new Array();
    }
    if(messages && messages[minHint])
    {
      hints.push(
        TrMessageFactory.createCustomMessage(
          messages[minHint],
	        ""+min)
       );
    }
    else
    {
      hints.push(
        TrMessageFactory.createMessage(
          minKey,
	        ""+min)
       );
    }
  }
  return hints;
}