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

/**
 * Simple function for opening a popup
 * @param contentId(String) id of the element to pop
 * @param triggerId(String) optional id of the element that launched the popup
 * @param event(Event) the javascript event object (used to position relative popups)
 * @param triggerType(String) 'click'(default) | 'hover'
 * @param position(String) 'relative'(default) | 'centered'
 * @param modal(boolean)
 * @param width(int) 
 * @param height(int)
 * @param xOffset(int)
 * @param yOffset(int)
 **/
TrPanelPopup.showPopup = function(
  contentId, 
  triggerId, 
  event, 
  triggerType,
  position, 
  modal, 
  width, 
  height, 
  xOffset, 
  yOffset)
{
  if (contentId == null)
    return;
  
  // Get/Initialize a map of visible popups
  var visiblePopups = TrPanelPopup._VISIBLE_POPUPS;
  if (!visiblePopups)
    visiblePopups = TrPanelPopup._VISIBLE_POPUPS = new Object();
  
  // Check if the popup is already visible
  if (visiblePopups[contentId])
    // Popup is already visible
    return;
    
  // Create new popup object and add it to the map of visible popups
  if (triggerType == "hover")
    visiblePopups[contentId] = new TrHoverPopup();
  else
    visiblePopups[contentId] = new TrClickPopup();

  var popup = visiblePopups[contentId];

  var content = document.getElementById(contentId);
  if (!content)
     return;

  popup.setContent(content);
  popup.setTrigger(document.getElementById(triggerId));
  popup.setModal(modal);
  popup.setCentered(position == 'centered');
  popup.setSize(width, height);
  popup.setRelativeOffsetX(xOffset);
  popup.setRelativeOffsetY(yOffset);
  
  popup.showPopup(event);
}

/**
 * Public function for hiding the current popup.
 */  
TrPanelPopup.hidePopup = function(event)
{
  event = window.event || event;
  var visiblePopups = TrPanelPopup._VISIBLE_POPUPS;
  if (!visiblePopups)
    return;

  //loop through element stack and find out which popup the event occured in.
  var currElement = event.target || event.srcElement;
  while (currElement)
  {
    var id = currElement.id;
    if (id)
    {
      var currPopup = visiblePopups[id];
      if (currPopup)
      {
        // We found the popup, so hide it.
        currPopup.hide(event);
        break;
      }
    }
    currElement = currElement.parentNode;
  }
}

/**
 * Class to handle a popup element on a page.
 */
function TrPanelPopup()
{
  //define object properties
  this._content = false;
  this._trigger = false;
  this._centered = false;
  this._modal = false;
  this._visible = false;
}

TrPanelPopup.prototype.getContent = function()
{
  return this._content;
}

TrPanelPopup.prototype.setContent = function(content)
{ 
  this._content = content;
  
  //Initialize the styles for the content
  if (this._content)
  {
    this._content.style.cssText  = "position: absolute; z-index: 5001; top: 0px; left: 0px; visibility:hidden; padding: 0px;";  
  }
}

/**
 * Get the element being used as the trigger
 **/
TrPanelPopup.prototype.getTrigger = function()
{
  return this._trigger;
}

/**
 * Sets the element to be used as the trigger to show the popup.  We
 * use this to ensure further events on the trigger don't cause a re-popup.
 * @param trigger(Element) The element that will trigger the popup.
 **/
TrPanelPopup.prototype.setTrigger = function(trigger)
{
  this._trigger = trigger;
}

/**
 * Sets the popup to be centered on screen when visible
 * @param centered(boolean) true if popup should be centered
 **/
TrPanelPopup.prototype.setCentered = function(centered)
{
  this._centered = centered;
}

/**
 * Returns true if the popup is set to modal.
 **/
TrPanelPopup.prototype.isModal = function()
{
  return this._modal;
}

/**
 * Sets the popup to be modal when visible
 */
TrPanelPopup.prototype.setModal = function(modal)
{
  this._modal = modal;
}

/**
 * Sets X offset to apply if popup is positioned relative to mouse x.
 * @param x(int) The x offset value.
 **/
TrPanelPopup.prototype.setRelativeOffsetX = function(x)
{
  this._relativeOffsetX = parseInt(x);
}

/**
 * Gets X offset to apply if popup is positioned relative to mouse x.
 * @return (int) The x offset value, or zero if unset.
 **/
TrPanelPopup.prototype.getRelativeOffsetX = function()
{
  return (this._relativeOffsetX) ? this._relativeOffsetX: 0;
}

/**
 * Sets Y offset to apply if popup is positioned relative to mouse y.
 * @param y(int) The y offset value.
 **/
TrPanelPopup.prototype.setRelativeOffsetY = function(y)
{
  this._relativeOffsetY = parseInt(y);
}

/**
 * Gets Y offset to apply if popup is positioned relative to mouse y.
 * @return (int) The y offset value, or zero if unset.
 **/
TrPanelPopup.prototype.getRelativeOffsetY = function()
{
  return (this._relativeOffsetY) ? this._relativeOffsetY: 0;
}


/**
 * Returns true if the popup is currently visible.
 **/
TrPanelPopup.prototype.isVisible = function()
{
  return this._visible;
}

/**
 * Holds the return value of the dialog.  Check this property after the 
 * popup has closed.
 **/
TrPanelPopup.prototype.returnValue = undefined;

/**
 * Attach a callback function that will be invoked when the popup
 * has been closed.  The callbackProps and returnValue properties will be
 * passed as parameters (e.g. function myCallback(props, value);).
 **/
TrPanelPopup.prototype.callback = undefined;

/**
 * Attach properties to the popup that will be passed to the callback function
 * (e.g. a component target to populate with the returnValue).
 **/
TrPanelPopup.prototype.callbackProps = undefined;

/**
 * Make the popup visible
 **/
TrPanelPopup.prototype.show = function(event)
{
  //we can't show content that isn't there
  if (!this.getContent())
    return;
 
  //don't pop during ppr - safety check
  if (_pprBlocking)
    return;

  //already visible
  if (this.isVisible())
    return;

  this._calcPosition(event);
  
  if (this.isModal())
    TrPanelPopup._showMask();
  
  TrPanelPopup._showIeIframe();

  this.getContent().style.visibility = "visible"; 
  
  this._visible = true;
}

/**
 * Hide the popup if visible.  Hiding the popup causes the callback
 * handler to be invoked (if configured).
 **/
TrPanelPopup.prototype.hide = function(event)
{
  //we can't hide content that isn't there
  if (!this.getContent())
    return;

  if (this.isModal())
    TrPanelPopup._hideMask();
  
  TrPanelPopup._hideIeIframe();
  
  this.getContent().style.visibility = "hidden";
  //move popup back to top left so it won't affect scroll size if window resized
  this.getContent().style.left = "0px";
  this.getContent().style.top = "0px";
  
  //call the callback function if attached
  if (this.callback)
  {
    try
    {
      this.callback(this.callbackProps, this.returnValue);
    }
    catch(ex)
    {
      alert("Error calling TrPanelPopup callback function:\n" + ex);
    }
  }
  
  this._visible = false;
  
  // Remove the popup from the list of visible popups
  var popups = TrPanelPopup._VISIBLE_POPUPS;
  if (popups)
    delete popups[this.getContent().id];
}

/**
 * Size the popup to a specific width and height
 */
TrPanelPopup.prototype.setSize = function(width, height)
{
  if (width)
  {
    var i = parseInt(width);
    if (i > 0)
      this.getContent().style.width = i + "px";
  }
  if (height)
  {
    var i = parseInt(height);
    if (i > 0)
      this.getContent().style.height = i + "px";
  }
}

// The modal mask - shared by all instances
TrPanelPopup._mask = undefined;

/**
 * Show the popup mask that blocks clicks in modal mode.  Initialize it
 * if not already.
 **/
TrPanelPopup._showMask = function()
{
  //initialise mask only once
  if (!TrPanelPopup._mask)
  {
    //create mask for modal popups
    TrPanelPopup._mask = document.createElement('div');
    TrPanelPopup._mask.name = "TrPanelPopup._BlockingModalDiv";
    var cssText = "display:none;position: absolute; z-index: 5000;top: 0px;left: 0px;cursor: not-allowed;";
    if (_agent.isIE && _agent.version == 7)
      //workaround for bug in IE7 : see http://blog.thinkature.com/index.php/2006/12/29/odd-mouse-handling-with-transparent-objects-under-internet-explorer-7/
      cssText = cssText + "background-color: white; filter: alpha(opacity=0);";
    else
      cssText = cssText + "background-color: transparent";
    TrPanelPopup._mask.style.cssText = cssText;
    TrPanelPopup._mask.innerHTML = "&nbsp;";

    //consume all events
    _addEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);

    //handle window resize events
    _addEvent(window, "resize", TrPanelPopup._setMaskSize);

    //set initial mask size
    TrPanelPopup._setMaskSize();

    //add mask to body
    document.body.appendChild(TrPanelPopup._mask);
    
  }

  TrPanelPopup._mask.style.display = "block";
  
}

/**
 * Hide the popup mask that blocks clicks in modal mode.
 **/
TrPanelPopup._hideMask = function()
{
  _removeEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);
  _removeEvent(window, "resize", TrPanelPopup._setMaskSize);
  TrPanelPopup._mask.style.display = "none";
}

/**
 * Check to see if a point lies inside the bounds of an element
 */
TrPanelPopup.prototype._hitTest = function(element, point)
{
  var pos = this._getElementPosition(element);
  if (pos.x > point.x || pos.y > point.y)
  {
    return false;
  }
  return pos.x + element.offsetWidth >= point.x &&
    pos.y + element.offsetHeight >= point.y;
}

/**
 * Reposition an element to ensure that it fits on the screen
 */
TrPanelPopup.prototype._fitOnScreen = function(element)
{
  var vis = this._getStyle(element, 'visibility');
  element.style.visibility = 'hidden';
  if (element.origX)
  {
    element.style.left = element.origX + 'px';
  }
  if (element.origY)
  {
    element.style.top = element.origY + 'px';
  }
  var w = element.offsetWidth;
  var h = element.offsetHeight;
  var pos = this._getElementPosition(element);
  var x = pos.x;
  var y = pos.y;
  var ww = document.body.offsetWidth;
  var wh = document.body.offsetHeight;

  var parentPos = this._getElementPosition(element.parentNode);

  if (x + w > ww)
  {
    var diff = (x + w) - ww;
    var ox = element.offsetLeft;
    if (!element.origX)
    {
      element.origX = ox;
    }
    element.style.left = (ox - diff - parentPos.x) + 'px';
  }
  if (y + h > wh)
  {
    var diff = (y + h) - wh;
    var oy = element.offsetTop;
    if (!element.origY)
    {
      element.origY = oy;
    }
    element.style.top = (oy - diff - parentPos.y) + 'px';
  }
  element.style.visibility = vis;
}

/**
 * Get the Page X and Y of an event
 */
TrPanelPopup.prototype._getEventPosition = function(event)
{
  return {
    x: (event.pageX || (event.clientX +
        (document.documentElement.scrollLeft || document.body.scrollLeft))),
    y: (event.pageY || (event.clientY +
        (document.documentElement.scrollTop || document.body.scrollTop)))
  };
}

/**
 * Check if the browser is Internet Explorer
 */
TrPanelPopup.prototype._isIE = function()
{
  return navigator.appVersion.indexOf("MSIE") != -1;
}

/**
 * Get the position of an element relative to the page, not
 * its parent
 */
TrPanelPopup.prototype._getElementPosition = function(element)
{
  var curleft = 0;
  var curtop = 0;
  var obj = element;
  while (obj)
  {
    curleft += obj.offsetLeft;
    curtop += obj.offsetTop;
    obj = obj.offsetParent;
  }
  return { x: curleft, y: curtop };
}

/**
 * Get a css property as its JavaScript variable name
 */
TrPanelPopup.prototype._cssToJs = function(prop)
{
  var jsProp = '';
  var upperNext = false;
  for (var c = 0; c < prop.length; c++)
  {
    if (prop.charAt(c) == '-')
    {
      upperNext = true;
      continue;
    }
    
    if (upperNext)
    {
      jsProp += prop.charAt(c).toUpperCase();
    }
    else
    {
      jsProp += prop.charAt(c);
    }
      
    upperNext = false;
  }
  
  return jsProp;
}

/**
 * Get a calculated CSS style value
 */
TrPanelPopup.prototype._getStyle = function(element, prop)
{
  if (element.currentStyle)
  {
    // remove dashes and uppercase next letter
    var jsProp = this._cssToJs(prop);
    return element.currentStyle[jsProp];
  }
  else if (window.getComputedStyle)
  {
    return document.defaultView.getComputedStyle(element, '')
      .getPropertyValue(prop);
  }
  return '';
}

/**
 * Function to center an element on the screen
 */
TrPanelPopup.prototype._centerOnScreen = function(element)
{
  element.style.position = 'absolute';
  var vis = this._getStyle(element, 'visibility');
  element.style.visibility = 'hidden'; // stop flickering
  var parentPos = this._getElementPosition(element.parentNode);

  var loc;
  if (this._isIE())
  {
    loc = document.body.scrollLeft +
      ((document.body.clientWidth - element.clientWidth) / 2) -
      parentPos.x;
    element.style.left = ((loc>0)?loc:0) + "px";
    loc = document.body.scrollTop +
      ((document.body.clientHeight - element.clientHeight) / 2) -
      parentPos.y;
    element.style.top = ((loc>0)?loc:0) + "px";
  }
  else
  {
    loc = window.pageXOffset + ((window.innerWidth - element.clientWidth) / 2) -
      parentPos.x;
    element.style.left = ((loc>0)?loc:0) + "px"
    loc = window.pageYOffset + ((window.innerHeight - element.clientHeight)/2) -
      parentPos.y;
    element.style.top= ((loc>0)?loc:0) + "px"
  }
  element.style.visibility = vis;
}

/**
 * Get the element to add to the dialog to, to ensure dialog
 * positioning
 */
TrPanelPopup.prototype._getOffsetParent = function()
{
  for (var elem = this.getContent(); elem != null;
    elem = elem.parentNode)
  {
    if (elem.tagName && 'form' == elem.tagName.toLowerCase())
    {
      return elem;
    }
  }
  return document.body;
}

/**
 * Position the popup ensuring it doesn't go off-page, and if centered, then 
 * center in the middle of the current window.
 **/
TrPanelPopup.prototype._calcPosition = function(event)
{
  var popup = this.getContent();
  event = window.event || event;
  
  var parent = this._getOffsetParent();
  if (!this._centered)
  {
    var pos = this._getEventPosition(event);
    var parentPos = this._getElementPosition(parent);
    popup.style.left = (pos.x - parentPos.x + 
      this.getRelativeOffsetX()) + 'px';
    popup.style.top = (pos.y - parentPos.y +
      this.getRelativeOffsetY()) + 'px';
  }
    
  if (!popup.origParent)
  {
    popup.origParent = popup.parentNode;
  }
  
  parent.appendChild(popup);
  
  if (this._centered)
  {
    this._centerOnScreen(popup);
  }
  else
  {
    this._fitOnScreen(popup);
  }
  
  if (!this.isModal())
  {
    var pos = this._getEventPosition(event);
    TrPanelPopup._resizeIeIframe(pos.x, pos.y, 
      popup.offsetWidth, popup.offsetHeight);
  }
}

/**
 * Simple event handler that consumes any clicks when modal popup is shown
 */
TrPanelPopup._consumeMaskEvent = function(event)
{
  return false;
}

/*
 * Sizes/resizes the modal mask if the window size changes
 */
TrPanelPopup._setMaskSize = function()
{
  //only bother if mask is inited
  if (!TrPanelPopup._mask)
    return;

  if (window.innerHeight!=window.undefined)
    fullHeight = window.innerHeight;
  else if (document.compatMode=='CSS1Compat')
    fullHeight = document.documentElement.clientHeight;
  else if (document.body)
    fullHeight = document.body.clientHeight;

  if (window.innerWidth!=window.undefined) 
    fullWidth = window.innerWidth;
  else if (document.compatMode=='CSS1Compat')
    fullWidth = document.documentElement.clientWidth;
  else if (document.body)
    fullWidth = document.body.clientWidth;
    
  // Determine what's bigger, scrollHeight or fullHeight / width
  if (fullHeight > document.body.scrollHeight)
  {
    popHeight = fullHeight;
  }
  else
  {
    popHeight = document.body.scrollHeight
  }
  
  TrPanelPopup._mask.style.height = popHeight + "px";
  TrPanelPopup._mask.style.width = document.body.scrollWidth + "px";
  
  TrPanelPopup._resizeIeIframe(0, 0, document.body.scrollWidth, popHeight);
}

/**
 * FUNCTIONS BELOW IMPLEMENT CSS/IFRAME WORKAROUND FOR THE INFAMOUS IE 6.x SELECT ZINDEX BUG
 * More info here: http://dotnetjunkies.com/WebLog/jking/archive/2003/07/21/488.aspx
 **/
TrPanelPopup._showIeIframe = function()
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.display = "block";      
  }
}

TrPanelPopup._hideIeIframe = function()
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.display = "none";      
  }
}

TrPanelPopup._resizeIeIframe = function(left, top, width, height)
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.left = left;
    TrPanelPopup._maskIframe.style.top = top;
    TrPanelPopup._maskIframe.style.width = width;
    TrPanelPopup._maskIframe.style.height = height;
  }
}

TrPanelPopup._initIeIframe = function()
{
  if (!TrPanelPopup._maskIframe)
  {
    //create single reusable iframe if not already inited
    TrPanelPopup._maskIframe = document.createElement('iframe');
    TrPanelPopup._maskIframe.name = "TrPanelPopup._ieOnlyZIndexIframe";
    TrPanelPopup._maskIframe.style.cssText = "display: none; left: 0px; position: absolute; top: 0px; z-index: 4999;";
    TrPanelPopup._maskIframe.style.filter = "progid:DXImageTransform.Microsoft.Alpha(style=0,opacity=0)";
    // FIXME: should this be set to avoid SSL warnings?
    //TrPanelPopup._maskIframe.src = "javascript:false;"
    document.body.appendChild(TrPanelPopup._maskIframe);
  }
}








function TrHoverPopup()
{
  TrPanelPopup.call(this);

  // Setup callback function for hiding the popup
  this._hoverCallbackFunction = TrUIUtils.createCallback(this, this.hidePopup);
}

// TrHoverPopup inherits from TrPanelPopup
TrHoverPopup.prototype = new TrPanelPopup();

TrHoverPopup.prototype.showPopup = function(event)
{
  // Setup event listener for mouse leaving trigger or content elements
  _addEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
  _addEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);
  
  this.show(event);
}

TrHoverPopup.prototype.hidePopup = function(event)
{
  event = window.event || event;
  
  var popup = this.getContent();
  var trigger = this.getTrigger();
  
  var eventPos = this._getEventPosition(event);
  
  // see if event is in the popup or the trigger bounds
  if ((this._hitTest(popup, eventPos) ||
    this._hitTest(trigger, eventPos)))
  {
    return;
  }
  
  // Cancel event listeners
  _removeEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
  _removeEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);

  this.hide(event);
  
  if (popup.origParent)
  {
    popup.origParent.appendChild(popup);
  }
}

TrHoverPopup.prototype.isModal = function()
{
  // Prevent modal for hover popups
  return false;
}




function TrClickPopup()
{
  TrPanelPopup.call(this);

  // Setup callback function for hiding the popup
  this._clickCallbackFunction = TrUIUtils.createCallback(this, this.hidePopup);
}

// TrHoverPopup inherits from TrPanelPopup
TrClickPopup.prototype = new TrPanelPopup();

TrClickPopup.prototype.showPopup = function(event)
{
  if (!this.isModal())
    // Setup event listener for clicking off the popup
    _addEvent(document, "click", this._clickCallbackFunction);
    
  this.show(event);
}

TrClickPopup.prototype.hidePopup = function(event)
{
  //loop through element stack where event occurred
  event = window.event || event;
  var currElement = event.target || event.srcElement;
  while (currElement)
  {
    //if clicked on trigger or popup  
    if (currElement == this.getContent() || 
        currElement == this.getTrigger())
    {
      break;
    }
    currElement = currElement.parentNode;
  }
  
  if (!currElement)
  {
    // Cancel event listeners
    _removeEvent(document, "click", this._clickCallbackFunction);

    //if click was on something other than the popupContainer
    this.hide(event);
    
    if (this.getContent().origParent)
    {
      this.getContent().origParent.appendChild(this.getContent());
    }
  }
}

