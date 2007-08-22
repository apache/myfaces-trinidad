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
    TrPanelPopup._mask.style.cssText = "display:none;position: absolute; z-index: 5000;top: 0px;left: 0px;cursor: not-allowed; background-color: transparent;";
    TrPanelPopup._mask.innerHTML = "&nbsp;";

    //consume all events
    TrPanelPopup._addEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);

    //handle window resize events
    TrPanelPopup._addEvent(window, "resize", TrPanelPopup._setMaskSize);

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
  TrPanelPopup._removeEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);
  TrPanelPopup._removeEvent(window, "resize", TrPanelPopup._setMaskSize);
  TrPanelPopup._mask.style.display = "none";
}

/**
 * Position the popup ensuring it doesn't go off-page, and if centered, then 
 * center in the middle of the current window.
 **/
TrPanelPopup.prototype._calcPosition = function(event)
{
  //position the popup
  var left = 0;
  var top = 0;
  
  var isIE = _agent.isIE;
  
  //bring some sanity to the cross browser measurements
  var xOffset = isIE ? document.documentElement.scrollLeft : window.pageXOffset;
  var yOffset = isIE ? document.documentElement.scrollTop : window.pageYOffset;
  var scrollWidth = document.body.scrollWidth;
  var scrollHeight = document.body.scrollHeight;
  var bodyWidth = isIE ? document.body.clientWidth : window.innerWidth;
  var bodyHeight = isIE ? document.body.clientHeight : window.innerHeight;
  var containerWidth = this.getContent().clientWidth;
  var containerHeight = this.getContent().clientHeight;

  if (this._centered)
  {
    left = xOffset + ((bodyWidth - containerWidth) / 2);
    top = yOffset + ((bodyHeight - containerHeight) / 2);
  }
  else
  {
    var eventX = isIE ? window.event.clientX : event.clientX;
    var eventY = isIE ? window.event.clientY : event.clientY;
    
    // Apply the offsets
    eventX += this.getRelativeOffsetX();
    eventY += this.getRelativeOffsetY();

    //ensure we keep popup within current page width
    if (xOffset + eventX + containerWidth > scrollWidth)
      left = scrollWidth - containerWidth;
    else
      left = xOffset + eventX;

    //ensure we keep popup within current page height
    if (yOffset + eventY + containerHeight > scrollHeight)
      top = scrollHeight - containerHeight;
    else
      top = yOffset + eventY;
  }  

  this.getContent().style.left = left + "px";
  this.getContent().style.top = top + "px";

  if (!this.isModal())
    TrPanelPopup._resizeIeIframe(left, top, containerWidth, containerHeight);
}

/**
 * Simple event handler that consumes any clicks when modal popup is shown
 */
TrPanelPopup._consumeMaskEvent = function(event)
{
  return false;
}

//useful event registration function
TrPanelPopup._addEvent = function(obj, evType, fn)
{
  // TODO: abstract onto Agent object
  if (obj.addEventListener)
  {
    obj.addEventListener(evType, fn, false);
    return true;
  }
  else if (obj.attachEvent)
  {
    var r = obj.attachEvent("on"+evType, fn);
    return r;
  }
  else
  {
    return false;
  }
}

//useful event deregistration function
TrPanelPopup._removeEvent = function(obj, evType, fn)
{
  // TODO: abstract onto Agent object
  if (obj.removeEventListener)
  {
    obj.removeEventListener(evType, fn, false);
    return true;
  }
  else if (obj.detachEvent)
  {
    var r = obj.detachEvent("on"+evType, fn);
    return r;
  }
  else
  {
    return false;
  }
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
    TrPanelPopup._maskIframe.style.cssText = "display: none; left: 0px; position: absolute; top: 0px; z-index: 199;";
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
  TrPanelPopup._addEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
  TrPanelPopup._addEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);
  
  this.show(event);
}

TrHoverPopup.prototype.hidePopup = function(event)
{
  // Only hide the popup if the event has a relatedTarget other than the
  // trigger, the content, or their child elements.  This allows mouse
  // to move between trigger and content without re-showing the popup

  //loop through element stack where event occurred
  var currElement = event.relatedTarget || event.toElement;
  while (currElement)
  {
    //if over trigger or popup  
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
    TrPanelPopup._removeEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
    TrPanelPopup._removeEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);

    this.hide(event);
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
    TrPanelPopup._addEvent(document, "click", this._clickCallbackFunction);
    
  this.show(event);
}

TrClickPopup.prototype.hidePopup = function(event)
{
  //loop through element stack where event occurred
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
    TrPanelPopup._removeEvent(document, "click", this._clickCallbackFunction);

    //if click was on something other than the popupContainer
    this.hide(event);
  }

}
