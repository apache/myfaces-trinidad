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

// Number of submits done on this page. This gives an accurate
// value of the depth of the history list (or distance to the page
// that preceeded this one).
var _pprSubmitCount = 0;

// IE doesn't add the first partial refresh into the history list.
// Doesn't matter if it's a partialSubmit or a partialChange. However,
// subsequent submits are counted. This variable keeps track of whether we've
// done any action on this page.
var _pprSomeAction = false;

// Number of outstanding partial page request.  Partial
// page rendering keeps this ref count to distinguish
// between real partial page activity and back/refresh.
var _pprRequestCount = 0;

// Flag used by partial page rendering to mark whether the
// parent window has been unloaded.
var _pprUnloaded = false;

// Flag used by partial page rendering and the back issue
// to indicate whether or not we need to restore the saved inline scripts.
var _pprBackRestoreInlineScripts = false;

// _pprBlocking is true if we're blocked waiting on a PPR event
var _pprBlocking = false;

// ER 4014884: block on every submit if requested.
var _blockOnEverySubmit = false;

// The name attached to the iFrame from which we do PPR updates
var _pprIframeName = "_pprIFrame";

// Controls whether or not Trinidad will allow the first click to go through in
// certain instances. When a PPR event occurs, we block all subsequent user
// input until it completes. However, there may be instances where the client
// wants to receive the very first click. For example, If the user has entered
// text in a textInput field with a firePartialAction attached, then
// immediately clicked a submit button two events will be triggered - an
// onChange followed by an onClick. The onChange will trigger the client action
// which will immediately start the PPR blocking, so the onClick will get
// consumed by the blocking code and no submit will occur. Setting this value
// to true will allow the click to go through. This value can be controlled by
// the firstClickPassed attribute on the body element.
var _pprFirstClickPass = false;

// We block using a special DIV element. This is it's name
var _pprdivElementName = '_pprBlockingDiv';

// stores the variables needed to load the libraries for IE
var _pprLibStore;


// The time at which we started the latest PPR block
var _pprBlockStartTime = 0;


// A holder for the pending timeout (Gecko only).
var _pprBlockingTimeout = null;

// Keeps track of the last element to initiate a PPR request
var _pprEventElement = null;

// We block input while a PPR request is in transit.
// _pprSavedCursor holds the cursor that the client had before the PPR event
// and _pprSavedCursorFlag keeps track of if we set _pprSavedCursor.
// var _pprSavedCursor = null;
var _pprSavedCursorFlag = false;

// Keeps track of whether the user has actually made a choice from the popup
var _pprChoiceChanged = false;


// Object containing information about the user agent
var _agent = new Object();

// Object for the last time we submitted
var _lastDateSubmitted;

// Object for the last time we reset a form
var _lastDateReset = 0;

// Variables tracking the last time we validated a field, and the last time the
// validation actually failed.
var _lastDateValidated  = 0;
var _lastValidationFailure  = 0;


// Keeps track of arguments that will be needed for a delayed event
_delayedEventParams = new Object();

// Object containing the initial state of a chosen form
var _initialFormState;
var _initialFormExclude = new Object();
var _initialFormStateName;
var _navDirty;

// this is the id of the component which gets the initial focus when the
// page loads.
var _initialFocusID = null;

// Certain Trinidad facilities can request that focus be set to a particular node,
// or the node AFTER a particular node following a PPR update. These three
// variables store the values needed to track down that node.
var _AdfFocusRequestDoc = null;
var _AdfFocusRequestID = null;
var _AdfFocusRequestNext = false;

// _checkUnload is set on our body tag and is called when the window
// unloads. In our dialog windows, we call _checkUnload via an intermediary
// function _unloadADFDialog to work around Google's pop-up blocker
// feature which blocks onUnload event handlers from window's opened
// with window.open.
// We don't want to call _checkUnload twice in a row,
// once from dialog code and once from body's onUnload event handler,
// so we block the second call in _unloadADFDialog.
var _blockCheckUnloadFromDialog = false;


// variables needed if _submitForm was called before the form had
// completely rendered.
var _saveForm = null;
var _saveDoValidate = null;
var _saveParameters = null;
var _submitRejected = false;
var _inPartialSubmit = false;

// Flag used for timing issues with radioButtons
var _pendingRadioButton = false;

// List of mouse event names to capture
var _IE_MOUSE_CAPTURE_EVENTS = [
  "onclick",
  "ondblclick",
  "onmousedown",
  "onmousemove",
  "onmouseout",
  "onmouseover",
  "onmouseup"
  ];

// List of mouse event names to capture
var _GECKO_MOUSE_CAPTURE_EVENTS = [
  "click",
  "mousedown",
  "mouseup",
  "mouseover",
  "mousemove",
  "mouseout",
  "contextmenu"
  ];

/**
 * Return true if the agent is at least the specified agent type and version.
 */
function _atLeast(
  kind,
  version
  )
{
  return (!kind    || (kind    == _agent.kind))    &&
         (!version || (version <= _agent.version));
}


/**
 * Return true if the agent is at most the specified agent type and version.
 */
function _atMost(
  kind,
  version
  )
{
  return (kind == _agent.kind) && (version >= _agent.version);
}

function _supportsDOM()
{
  var retVal = false;

  if (_agent.isIE)
  {
    retVal = _agent.version >= 5.5;
  }
  else if (_agent.isNav)
  {
    retVal = false;
  }
  else if (_agent.isGecko || _agent.isSafari)
  {
    retVal = true;
  }
  else if(_agent.isBlackBerry)
  {
    retVal = false;
  }

  return retVal;
}

/**
 * initialize information about the agent
 */
function _agentInit()
{
  // convert all characters to lowercase to simplify testing
  var agentString = navigator.userAgent.toLowerCase();

  // note that this only returns m.n (e.g. if the
  // version number is 2.3.4, this returns the float 2.3)
  var version = parseFloat(navigator.appVersion);

  // note that isBlackBerry refers to the BlackBerry browser
  // we do not currently specify the BlackBerry platform
  // because it is not necessary (if we decide to support
  // other browsers on the BlackBerry platform it may become necessary)
  var isOpera      = false;
  var isIE         = false;
  var isNav        = false;
  var isGecko      = false;
  var isSafari     = false;
  var isPIE        = false;
  var isBlackBerry = false;
  var kind         = "unknown";
  var isWindows    = false;
  var isSolaris    = false;
  var isMac        = false;

  if (agentString.indexOf("msie") != -1)
  {
    // extract ie's version from the ie string
    var matches = agentString.match(/msie (.*);/);
    version = parseFloat(matches[1]);
    if (agentString.indexOf("ppc") != -1 &&
        agentString.indexOf("windows ce") != -1 &&
        version >= 4.0)
    {
      isPIE = true;
      kind = "pie";
    }
    else {
      isIE = true;
      kind = "ie";
    }
  }
  else if (agentString.indexOf("opera") != -1)
  {
    isOpera = true
    kind = "opera";
  }
  else if ((agentString.indexOf("applewebkit") != -1) ||
           (agentString.indexOf("safari") != -1))
  {
    isSafari = true
    kind = "safari";
  }
  else if (agentString.indexOf("gecko/") != -1)
  {
    isGecko = true;
    kind = "gecko";
    version = 1.0;
  }
  else if(agentString.indexOf("blackberry") != -1)
    {
      // if we support non-BlackBerry Browser agents on blackberry
      // devices in the future, we may need to revisit this because
      // those agents may include "blackberry" in the User-Agent
      // string; we can't just check if the User-Agent "starts with"
      // blackberry because navigator.userAgent on BlackBery Browser 4.0
      // starts with Mozilla/4.0 (even though the User-Agent sent to the
      // server starts with BlackBerry<model>/<version>)
    
      // BlackBerry Browser 4.0+ supports navigator.appVersion,
      // and earlier versions don't support script, so we can
      // leave the version as defined above
      isBlackBerry = true;
      kind = "blackberry";
  }
  else if ((agentString.indexOf('mozilla')    != -1) &&
           (agentString.indexOf('spoofer')    == -1) &&
           (agentString.indexOf('compatible') == -1))
  {
    if (version >= 5.0)
    {
      isGecko = true;
      kind = "gecko";
      version = 1.0;
    }
    else
    {
      isNav = true;
      kind = "nn";
    }
  }
  if (agentString.indexOf('win') != -1)
  {
    isWindows = true;
  }
  else if (agentString.indexOf('mac') != -1)
  {
    isMac = true;
  }
  else if (agentString.indexOf('sunos') != -1)
  {
    isSolaris = true;
  }

  _agent.isIE = isIE;
  _agent.isNav = isNav;
  _agent.isOpera = isOpera;
  _agent.isPIE = isPIE;
  _agent.isGecko = isGecko;
  _agent.isSafari = isSafari;
  _agent.isBlackBerry = isBlackBerry;
  _agent.version = version
  _agent.kind = kind;
  _agent.isWindows = isWindows;
  _agent.isSolaris = isSolaris;
  _agent.isMac = isMac;

  _agent.atLeast = _atLeast;
  _agent.atMost  = _atMost;
}


_agentInit();

// available features in ie
var _ieFeatures =
{
  channelmode:1, // ie 5.0
  copyhistory:1,
  directories:1,
  fullscreen:1,  // ie 5.0
  height:1,
  location:1,
  menubar:1,
  resizable:1,
  scrollbars:1,
  status:1,
  titlebar:1,    // ie 5.0 when trusted
  toolbar:1,
  width:1
};

// available features in Netscape
var _nnFeatures =
{
  alwayslowered:1,
  alwaysraised:1,
  copyhistory:1,
  dependent:1,
  directories:1,
  height:1,
  hotkeys:1,
  innerheight:1,
  innerwidth:1,
  location:1,
  menubar:1,
  outerwidth:1,
  outerheight:1,
  resizable:1,
  scrollbars:1,
  status:1,
  titlebar:1,
  toolbar:1,
  width:1,
  "z-lock":1
}

// override values for modeless windows. Values in this
// list can't be overridden by the caller for modeless windows
var _modelessFeatureOverrides =
{
};

// override values for modal windows. Values in this
// list can't be overridden by the caller for modal windows
var _modalFeatureOverrides =
{
};


var _featureDefaults =
{
  // default values for features of document windows
  document:
  {
    channelmode:false,
    copyhistory:true,
    dependent:false,
    directories:true,
    fullscreen:false,
    hotkeys:false,
    location:true,
    menubar:true,
    resizable:true,
    scrollbars:true,
    status:true,
    toolbar:true
  },
  // default values for features of dialog windows
  dialog:
  {
    channelmode:false,
    copyhistory:false,
    dependent:true,
    directories:false,
    fullscreen:false,
    hotkeys:true,
    location:false,
    menubar:false,
    resizable:true,
    scrollbars:true,
    status:true
  }
}


// featues that require signbing in order to be set
var _signedFeatures =
{
  alwayslowered:1,
  alwaysraised:1,
  titlebar:1,
  "z-lock":1
};

// features that are boolean values
var _booleanFeatures =
{
  alwayslowered:1,
  alwaysraised:1,
  channelmode:1,
  copyhistory:1,
  dependent:1,
  directories:1,
  fullscreen:1,
  hotkeys:1,
  location:1,
  menubar:1,
  resizable:1,
  scrollbars:1,
  status:1,
  titlebar:1,
  toolbar:1,
  "z-lock":1
};



/**
 * Gets the preferred width of the content
 */
function _getBodyWidth(
  element,
  offsetWidth,
  offsetLeft
  )
{
  var maxWidth = _getContentWidth(element, offsetWidth, 0);

  // bogusly double the offset to guess the right margin...
  // However, in right to left languages, the left margin can get very large,
  // and cause the window to become huge (Bug 2846393). Limit it to an
  // empirically reasonable value.
  var marginWidth = 10;

  if (_isLTR() || (offsetLeft <= 5))
  {
      marginWidth = 2 * offsetLeft;
  }

  return maxWidth + marginWidth;
}

/**
 * Gets the preferred width of the content
 */
function _getContentWidth(
  element,
  offsetWidth,
  offsetLeft
  )
{
  var children = element.childNodes;

  // XXXSafari: what to do here?
  var isGecko = _agent.isGecko;

  var hasContentProp = (isGecko)
                         ? "tagName"
                         : "canHaveHTML"
  var maxWidth = 0;

  for (var i = 0; i < children.length; i++)
  {
    var currChild = children[i];

    if (currChild[hasContentProp] && (currChild.offsetWidth > 0))
    {
      var currWidth = 0;
      var currOffsetWidth = currChild["offsetWidth"];

      if (isGecko)
      {
        if ((currOffsetWidth == offsetWidth) ||
            (currOffsetWidth <= 1))
        {
          var currOffsetLeft = currChild.offsetLeft;
          if (currChild.parentNode != currChild.offsetParent)
          {
            currOffsetLeft = currOffsetLeft -
                             (currChild.parentNode.offsetLeft);
          }

          currWidth = _getContentWidth(currChild,
                                       currOffsetWidth,
                                       currOffsetLeft);
        }
        else
        {
          currWidth = currOffsetWidth;
        }
      }
      else
      {
        currWidth = currChild["clientWidth"];

        if (currWidth == 0)
        {
          var currOffsetLeft = currChild.offsetLeft;
          if (currChild.parentElement != currChild.offsetParent)
          {
            currOffsetLeft = currOffsetLeft -
                             (currChild.parentElement.offsetLeft);
          }

          currWidth = _getContentWidth(currChild,
                                       currOffsetWidth,
                                       currOffsetLeft);
        }
      }

      if (currWidth > maxWidth)
      {
        maxWidth = currWidth;
      }
    }
  }

  // handle error cases
  if (maxWidth == 0)
    maxWidth = offsetWidth;

  return maxWidth + offsetLeft;
}

/**
 * Safely returns the parent window of a window, or undefined if security doesn't allow us to
 * retrieve the parent
 */
function _getParentWindow(currWindow)
{
  var parentWindow = currWindow.parent;

  try
  {
    // dummy read to test security error
    parentWindow.name;
    
    return parentWindow;
  }
  catch (e)
  {
    return undefined;
  }
}

/**
 * Returns the window for the document
 */
function _getWindowForDocument(document)
{
  if (_agent.isIE)
  {
    return document.parentWindow;
  }
  else
  {
    return document.defaultView;
  }
}

/**
 * Safely retrieve the top accessible window
 */
function _getTop(element)
{
	
  var initialDocument = (element && element.ownerDocument)
                          ? element.ownerDocument
                          : document;
  
  // since top might be in another domain, crawl up as high as possible manually
  var currWindow = _getWindowForDocument(initialDocument);
  var currParentWindow = _getParentWindow(currWindow);
  
  while (currParentWindow && (currParentWindow != currWindow))
  {
    currWindow = currParentWindow;
    currParentWindow = _getParentWindow(currWindow);
  }

  return currWindow;
}


/**
 * renders transparent image for spacing
 */
function t(width,height)
{

  // if the transparent url is not null render img tag
  if ( _tURL != void 0 )
  {
    document.write('<img src="' + _tURL + '"');

    if (width!=void 0)
      document.write(' width="' + width + '"');
    if (height!=void 0)
      document.write(' height="' + height + '"');

    // if accessibility mode is not null, render alt attribute
    if (_axm != void 0)
      document.write(' alt=""');

    document.write('>');
  }
}



/**
 * Returns the object containing the dependent windows.
 */
function _getDependents(
  parentWindow,
  createIfNecessary
  )
{
  var depends;

  if (parentWindow)
  {
    depends = parentWindow["_dependents"];

    if (depends == (void 0))
    {
      if (createIfNecessary)
      {
        depends = new Object();
        parentWindow["_dependents"] = depends;
      }
    }
  }

  return depends;
}

/**
 * Get the named dependent
 */
function _getDependent(
  parentWindow,
  dependentName
  )
{
  var depends = _getDependents(parentWindow);
  var dependent;

  if (depends)
  {
    dependent = depends[dependentName];
  }

  return dependent;
}


/**
 * Sets the value of the named dependent
 */
function _setDependent(
  parentWindow,
  dependentName,
  dependentValue
  )
{
  var depends = _getDependents(parentWindow, true);

  if (depends)
  {
    depends[dependentName] = dependentValue;
  }
}


/**
 * Returns the dependent which is modal.
 */
function _getModalDependent(
  parentWindow
  )
{
  return _getDependent(parentWindow, "modalWindow");
}


/**
 * Returns true if the passed in dependent is the modal dependent
 * of the parent window,
 */
function _isModalDependent(
  parentWindow,
  dependent
  )
{
  return (dependent == _getModalDependent(parentWindow));
}


/**
 * Called by our modal windows when changes are applied and
 * the window is closed to make sure that the parent window
 * is updated appropriately. Due to bug 3184718 (Google
 * pop-up blocker does not call onUnload event handlers from
 * dialog windows) we cannot rely on _checkUnload being
 * called by the unload event. We call this function
 * instead which in turn calls _checkUnload.
 */
function _unloadADFDialog(
  event
  )
{
  // _checkUnload is called from body's
  // unload event when
  // We use this flag to keep it from
  // running through _checkUnload function twice.
  // If Google's pop-up blocker is
  // not enabled, then _checkUnload is called from body's
  // unload event as well as from here.

  _blockCheckUnloadFromDialog = false;
  _checkUnload(event);
  _blockCheckUnloadFromDialog = true;
}

/**
 * Called by the unload handler of modal windows to call the event
 * handler and make sure that the parent window is updated appropriately
 * to show that no modal window is up anymore.
 */
function _checkUnload(
  event
  )
{
  //PH:set the right event object;
  event = _getEventObj();
  
  // Make sure we don't run through this function twice
  // when we close a dialog. The
  // _unloadADFDialog function blocks a second run
  // using the _blockCheckUnloadFromDialog flag.

  if (_blockCheckUnloadFromDialog)
  {
    _blockCheckUnloadFromDialog = false;
    return;
  }

  // Check to see if we are a modal window that has been
  // abandoned (who's parent has changed out from under us).
  // In this case, we skip the unload handler, since the
  // modal window no longer has permission to access its
  // parent, and JavaScript errors may occur
  if (_isModalAbandoned())
    return;

  // Check to see if we have an open modal child window
  var modalWindow = _getModalDependent(window);
  if (modalWindow != null)
  {
    // If we are being unloaded before our modal child has been
    // closed, that means that the user must have navigated
    // to a new page just before the modal window was displayed.
    // In this case, let's just close our modal child.  We need
    // to be extra careful to make sure that the modal child does
    // not try to access any properties on the parent window
    // when closing, because the modal child may no longer have
    // permission to access us at this point.  Set a property
    // on the modal window to let it know that it has been
    // abandoned.
    _setModalAbandoned(modalWindow);

    // Now we can safely close the modal window
    modalWindow.close();
  }

  _pprUnloaded = true;

  var topWindow = _getTop();

  if (!topWindow)
    return;

  var parentWindow = topWindow["opener"];

  if (!parentWindow)
    return;

  var unloader = _getDependent(parentWindow, self.name);

  if (_isModalDependent(parentWindow, self))
  {
    // remove the modal window
    _setDependent(parentWindow, "modalWindow", (void 0));

    parentWindow.onfocus = null;

    var parentBody = parentWindow.document.body;

    // release the ie mouse grab
    if (_agent.atLeast("ie", 4))
    {
      if (_agent.atLeast("ie", 5) && _agent.isWindows)
      {
        parentBody.onlosecapture = null;

        _removeModalCaptureIE(parentBody);
      }
      parentBody.style.filter = null;
    }

    if (_agent.isGecko)
    {
      if (parentBody != (void 0))
      {
        _removeModalCaptureGecko(parentWindow, parentBody);
      }
    }
  }

  if (unloader != (void 0))
  {
    // remove our dependent info
    _setDependent(parentWindow, self.name, (void 0));

    // try the passed in event (netscape way first), then
    // try to get the event the IE way
    if (event == (void 0))
      event = self.event;

    // call the unloader with the unloading window and the event
    unloader(topWindow, event);
  }
}

// Adds a (IE-specific) capture to the specified element
// for blocking mouse events during modal dialog display
function _addModalCaptureIE(element)
{
  // Captured events still bubble on IE.  Register
  // mouse event handlers to cancel event bubbling
  // and save away old listeners so we can restore
  // them when the capture is removed.
  var savedListeners = new Object();
  var events = _IE_MOUSE_CAPTURE_EVENTS;
  var eventCount = events.length;

  for (var i = 0; i < eventCount; i++)
  {
    var eventName = events[i];
    savedListeners[eventName] = element[eventName];
    element[eventName] = _captureEventIE;
  }

  // Stash away the saved listener somewhere where
  // we can get at them later
  window._modalSavedListeners = savedListeners;

  // Set the capture
  element.setCapture();
}

// Removes a (IE-specific) capture added via _addModalCaptureIE()
function _removeModalCaptureIE(element)
{
  // Release the capture
  element.releaseCapture();

  // Restore event handlers that were saved away
  // during _addModalCaptureIE().
  var savedListeners = window._modalSavedListeners;

  if (savedListeners)
  {
    var events = _IE_MOUSE_CAPTURE_EVENTS;
    var eventCount = events.length;

    for (var i = 0; i < eventCount; i++)
    {
      var eventName = events[i];

      element[eventName] = savedListeners[eventName];
    }

    window._modalSavedListeners = null;
  }
}


// Captures (and consumes) events during modal grabs
// on IE browsers
function _captureEventIE()
{
  window.event.cancelBubble = true;
}

// Adds a (Gecko-specific) capture to the specified element
// for blocking mouse events during modal dialog display
function _addModalCaptureGecko(element)
{
  var events = _GECKO_MOUSE_CAPTURE_EVENTS;
  var eventCount = events.length;

  for (var i = 0; i < eventCount; i++)
  {
    var eventName = events[i];
    element.addEventListener(eventName, _captureEventGecko, true);
  }
}

// Removes a (Gecko-specific) capture added via _addModalCapture()
function _removeModalCaptureGecko(parentWindow, element)
{
  var events = _GECKO_MOUSE_CAPTURE_EVENTS;
  var eventCount = events.length;

  for (var i = 0; i < eventCount; i++)
  {
    var eventName = events[i];
    element.removeEventListener(eventName,
                                parentWindow._captureEventGecko,
                                true);
  }
}

// Captures (and consumes) events during modal grabs
// on Gecko browsers
function _captureEventGecko(
  event
  )
{
  // Stop propagation and suppress default action
  event.stopPropagation();
  window.preventDefault = true;
}

// Tests whether the current window is an "abandoned"
// modal window.  This is a modal window who's parent
// window has navigated to a new page, in which case
// the modal window is out of context.
function _isModalAbandoned()
{
  // We look for the _abandoned property on the modal window.
  // Note that in the LOV case, we actually have two onunload
  // event handlers that need to be suppressed: The onunload
  // handler for the LOV window and the onunload handler for
  // the actual contents of the LOV window which are nested
  // within a frame.  So, we check for the _abandoned property
  // on the "top" window.
  var topWindow = _getTop();
  
  return topWindow._abandoned;
}

// Marks the specified modal window as abandoned
function _setModalAbandoned(
  modalWindow
  )
{
  // Just set the _abandoned property on the modal window.
  modalWindow._abandoned = true;
}


function _focusChanging()
{
  if (_agent.isIE)
  {
    return (window.event.srcElement != window.document.activeElement);
  }
  else
  {
    // Netscape gives us no good way of determining this
    return true;
  }
}


/**
 * Function that returns a single key/value pair String
 */
function _getKeyValueString(
  target,
  keyName,
  index
  )
{
  var value = target[keyName];

  if (typeof(value) == "function")
  {
    value = "[function]";
  }

  // XXXSafari: what to do here?
  var separator = (_agent.isGecko)
                    ? ((index + 1) % 3 == 0)
                      ? '\n'
                      : '    '
                    : '\t';

  return keyName + ':' + value + separator;
}

function _dumpSuppress(
  target
  )
{
  _dump(target, {innerText:1, outerText:1, outerHTML:1, innerHTML:1});
}

/**
 * Utility for dumping the contents of a JavaScript object.
 */
function _dump(
  target,
  suppressProps,
  name
  )
{
  var props = "";

  if (target)
  {
    // default the name if none provided
    if (!name)
    {
      name = target["name"];
    }

    //
    // Because we need to catch exceptions that IE throws if
    // for some object reads, we need to have separate ie and netscape
    // code for the exception catching.  Unfortunately, "try" and
    // catch are reserved words, so we have to dynamically create
    // our adding function so that Netscape doesn't throw it's own
    // exception when it parses our file
    //
    // var adderContent = "return i + ':' + target[i] + '\\n';";
    var adderContent = "return _getKeyValueString(target, key, index);";

    // wrap adder content with a try and eat the bogus ie exception
    if (_agent.atLeast("ie", 5) || _agent.isGecko || _agent.isSafari)
      adderContent = "try{" + adderContent + "}catch(e){return '';}";

    var adder = new Function("target", "key", "index", adderContent);
    var propCount = 0;
    var propArray = new Array();

    for (var key in target)
    {
      // don't add properties that should be suppressed
      if ((!suppressProps || !suppressProps[key]) && !key.match(/DOM/))
      {
        propArray[propCount] = key;
        propCount++;
      }
    }

    // sort the array so that we can find stuff
    propArray.sort();

    for (var i = 0; i < propArray.length; i++)
    {
      props += adder(target, propArray[i], i);
    }
  }
  else
  {
    // the object to dump was undefined
    name = "(Undefined)";
  }

  // tell the user that the object has no properties
  if (props == "")
  {
    props = "No properties";
  }

  alert(name + ":\n" + props);
}

function _getJavascriptId(name)
{
  return name.split(':').join('_');
}

/**
 * Calls the correct validations function for the form and returns true
 * if the validation succeeded.
 */
function _validateForm(
  form,
  source
  )
{
  var funcName = '_' + _getJavascriptId(form.name) + 'Validator';
  var formWind = window[funcName];
  if (formWind)
    return formWind(form, source);

  return false;
}


/**
 * Returns the next sibling that is not a comment
 */
function _getNextNonCommentSibling(
  parent,
  index
  )
{
  var children = parent.children;

  for (var i = index + 1; i < children.length; i++)
  {
    var child = children[i];

    if (child && (child.tagName != "!"))
    {
      return child;
    }
  }

  return null;
}


/**
 * Validate the specified field.
 */
function _valField(
  formName,
  nameInForm
  )
{
  if (nameInForm)
  {
    // get the target whose validation we want to run
    var target = document.forms[formName][nameInForm];

    // get its onblur function
    var blurFunc = target.onblur;

    if (blurFunc)
    {
      var valFunc = blurFunc.toString();

      // whack off the beginning and end of the function, leaving the content
      var valContents = valFunc.substring(valFunc.indexOf("{") + 1,
                                          valFunc.lastIndexOf("}"));

      var targetString = "document.forms['" +
                         formName +
                         "']['" +
                         nameInForm +
                         "']";

      // replace 'this' with the actual target
      valContents = valContents.replace(/this/, targetString);

      // trim off last argument
      var lastArg = valContents.lastIndexOf(",");

      valContents = valContents.substring(0, lastArg) + ")";

      // perform the validation
      eval(valContents);
    }
  }
}

function _validationAlert(errorString)
{
  // Show the error and note the time we finished this validation.
  // Record the validation both before and after the alert so that we
  // halt any validations caused by events triggered along with this
  // one, or by the closing of this alert.
  _recordValidation(true, 0);
  alert(errorString);
  _recordValidation(true, 0);
}

// Records the time of this validation event.
// If fail is set, this is a validation failure, that is noted also.
function _recordValidation(fail, now)
{
  if (!now)
    now = new Date();

  _lastDateValidated = now;
  if (fail)
    _lastValidationFailure = now;
}


// returns true if a validation has occurred "recently"
// failures: True means only report on recent failures, false means report on
//           any validation.
function _recentValidation(failures)
{
  var retVal = false;
  var timeWindow = 250;

  // Assuming that a reasonable user won't close the dialog and change the
  // text within a quarter of a second, we ignore any validations within
  // 250ms. of the last failed validation. The timings I've seen here range
  // in the 60 - 90 ms. range, but that is on fast development machines. We
  // could probably lower this to 150 if we're seeing dropped validations
  // for really fast, ambitious users.
  // With Macintosh IE, we manage to crash the browser!
  if (_agent.isMac)
  {
    // The iBook can have diffs of up to about 480 ms.
    // Call it 600 to be safe.
    timeWindow = 600;
  }

  var newDate = new Date();
  var diff;

  // If failures are requested, the caller is only interested in failures.
  // If simple validation requested, caller interested in any validation fail
  // or not.
  diff = newDate - _lastValidationFailure;
  if ((diff >= 0) && (diff < timeWindow))
  {
    retVal = true;
  }
  else if (!failures)
  {
    diff = newDate - _lastDateValidated;
    if ((diff >= 0) && (diff < timeWindow))
    {
      retVal = true;
    }
  }
  return retVal;
}

/**
 * Field validation function
 * @param err   The error string
 * @param input The field we are validating
 */
function _validateField(
  input,
  validationIndex,
  errorFormatIndex,
  emptyValidation,
  nextSibOK
  )
{
  var isNN = _agent.isNav;

  // don't validate under Netscape if tabbing to the next sibling is OK
  if (isNN && nextSibOK)
  {
    return;
  }

  // Bug #2205664 (also 2210697, and 2465351): Handling these validations is
  // fraught with peril on Netscape. The problem is that onBlur events stack
  // up. The user leaves the text field, an onBlur fires, we raise an alert
  // causing another onBlur to queue up, we finish handling the first,
  // return, and immediately get the second.
  // Bug 2465351 also mentions Mozilla, so it's been added for completeness.
  if (isNN || _agent.isMac || _agent.isGecko)
  {
    if (_recentValidation(false))
      return;
  }

  // determine whether we need to validate the field
  var doValidate = emptyValidation || (_getValue(input) != "");

  // We only validate if we aren't in the middle of validation in order
  // to avoid infinite lopps caused by the fact that the focus has already
  // moved to a new field when the validation fires, and if the validation
  // on the new field failed also, we would ping-pong between the fields
  // forever
  if (doValidate && !window._validating && _focusChanging())
  {
    if (nextSibOK)
    {
      var activeElement = window.document.activeElement;

      if (activeElement)
      {
        var parent = input.parentElement;

        if (parent == activeElement.parentElement)
        {
          var children = parent.children;

          for (var i = 0; i < children.length; i++)
          {
            if (input == children[i])
            {
              doValidate = (activeElement != _getNextNonCommentSibling(parent, i));
            }
          }
        }
      }
    }

    if (doValidate)
    {
      var validationError = _getValidationError(input, validationIndex);

      if (validationError)
      {
        var isShowing = _isShowing(input);

        // mark that we are in the middle of validation
        window._validating = input;

        if (isShowing)
          input.select();

        // move the focus back to the failed field before showing the alert
        // to grab the user's attention about what failed.  We don't
        // do this for netscape because doing so in Netscape will cause
        // an infinite loop because Netscape appears to keep the onFocus
        // caused by moving the focus to the field and the onBlur
        // caused by the display of the alert queued up.  This is a problem
        // because it causes the window._validating flag to be cleared
        // and another round of validation to occur
        // ColorField has required validation on hidden field,
        // but cannot receive focus
        if (!isNN && isShowing)
        {
          input.focus();

          // See if there's a specific position at which validation
          // failed;  if there is, we'll try to select everything after it.
          if (window["_failedPos"] != (void 0))
          {
            // IE style - createTextRange()
            if (input.createTextRange)
            {
              var rng = input.createTextRange();
              rng.moveStart("character", window["_failedPos"]);
              rng.select();
            }
            // Mozilla style.  Sadly, this won't work for TEXTAREA,
            // because of a Mozilla bug.
            else if (input.selectionStart != (void 0))
            {
              input.selectionStart = window["_failedPos"];
            }

            window["_failedPos"] = (void 0);
          }
        }

        // get the error String, if any
        var errorString = _getErrorString(input, errorFormatIndex,
                                          validationError);

        if (errorString)
        {
          // show the error and note the time we finished this validation.
          _validationAlert(errorString);
        }

        // move the focus back to the field after showing the alert for
        // Netscape
        // ColorField has required validation on hidden field,
        // but cannot receive focus.
        if (isNN && isShowing)
        {
          input.focus();
        }
      }
    }
  }
}


/**
 * Field unvalidation function.
 *
 * If the input object is the currently validating object, reset the
 * validating object so that field validations can continue.  This method
 * is called from the onFocus handler of validating objects.
 */
function _unvalidateField(
  input
  )
{
  if (window._validating == input)
  {
    window._validating = void 0;
  }
}

/**
 * Used to submit a selected item in a choice as if it's a commandLink
 * or commandButton
 */
function _commandChoice(
  form,
  choice
)
{
  var src = document.forms[form].elements[choice].value;

  // need this strange [0] for when choice repeated,
  // for example a processChoiceBar in actions facet of panelPage.
  if (src == void(0))
    src = (document.forms[form].elements[choice])[0].value;

  // if it starts with a '#', it's an url.
  var gtIndex = src.indexOf("#");
  if ( gtIndex == 0)
    window.document.location.href = src.substring(1,src.length);
  else
  {
    var openBracketIndex = src.indexOf("[");
    var srcID = src.substring(0, openBracketIndex);
    var validateString = src.substring(openBracketIndex+1, openBracketIndex+2)
    var validate = parseInt(validateString);
    submitForm(form,validate,{source:srcID});
  }
}



/**
 * Attempts to submits the form, potentially firing validation and notifying
 * any Cabo onSubmit handlers registered on the form, returning
 * <code>true</code> if the submission actually occurred.
 * <p>
 * If the <code>doValidate</code> parameter is false, no validation will
 * be performed, and the form is guaranteed to be submitted.  Otherwise,
 * the form will be submitted if both the validation succeeds and any
 * registered Cabo onSubmit handlers do not return <code>false</code>.
 * <p>
 * @param form The form to submit.  This can either be the name of the form
 *             in the current <code>document</code>, the index of the form
 *             in the current <code>document</code> or the form itself.
 * @param doValidate boolean value specifying whether validation should
 *   occur before the form is submitted.  (As per a common Javascript
 *   idiom, it is acceptable to pass true/false as well as 0/1).  If
 *   this parameter is ommitted, it defaults to true.
 * @param parameters a single Javascript object that specifies
 *   all the additional key-value pairs to submit.  There must be
 *   pre-existing &lt;input type="hidden"&gt; elements as targets
 *   for each of these parameters.
 */
function submitForm(
  form,
  doValidate,
  parameters
  )
{
  // If we've delayed any sort of event submission, we won't want to do it at
  // all now that the form is getting submitted. Blow away the saved data. Any
  // timeout handler will cancel the event submission if the data no longer
  // exists.
  var pending = true;
  if (_agent.isIE)
  {
    pending = false;
    // keep track of whether there was a pending event
    for (var key in _delayedEventParams)
    {
      pending = true;
      break;
    }
  }

  if (pending)
  {
    _delayedEventParams = new Object();
    _delayedEventParams["reset"] = true;
  }

  // if the form was passed as a form name, get the form object
  if ((typeof form) == "string")
  {
    form = document[form];
  }
  // if the form was passed as a form index, get the form object
  else if ((typeof form) == "number")
  {
    form = document.forms[form];
  }

  // we had better have a form now
  if (!form)
    return false;

  // Check to see if submitForm is called before the form
  // has been rendered completely. If so, save the parameters
  // and return. At the end of the form, we always call _submitFormCheck
  // which will re-call submitForm if it had been rejected.
  // This is for bug 2752257.

  // Bug #3058770: In LovInput, we have to hack up a form for NLS. It's not
  // validated, so there is no real validator, we've just hacked one. The
  // submit always sets doValidate to false. Just make sure that you never use
  // this validator if doValidate is false (it might just be the value '1').
  var formComplete = window["_"+ _getJavascriptId(form.name) + "Validator"];

  if (formComplete == (void 0))
  {
    _saveFormForLaterSubmit(form, doValidate, parameters);

    // Do not submit the form,
    // since the form hasn't been rendered completely yet.
    return false;
  }

  // Bug 1789483: ignore a second form submission that happens
  // less than 0.5 seconds after the first one
  var newDate = new Date();
  if (_recentSubmit(newDate))
  {
    // However if we're allowing the first click through... we queue up this
    // submit.
    if (_pprFirstClickPass && _pprBlocking)
    {
      _saveFormForLaterSubmit(form, doValidate, parameters);
    }
    return;
  }

  // just in case, clear it the delayed submit flags
  _submitRejected = false;
  _inPartialSubmit = false;

  _lastDateSubmitted = newDate;

  // default value for doValidate is true
  if (doValidate == (void 0))
    doValidate = true;

  // assume that we should submit the form
  var doSubmit = true;

  // validate the form if necessary, and don't submit the
  // form if validation fails
  var paramSource;
  if (parameters != null)
    paramSource = parameters.source;
  else
    paramSource = "";

  if (doValidate && !_validateForm(form, paramSource))
    doSubmit = false;

  //
  // If we have an onSubmit handler, call it
  //
  var onSubmit = window["_" + _getJavascriptId(form.name) + "_Submit"];

  if (onSubmit != (void 0))
  {
    // create function so that "return" is handled correctly,
    var func = new Function("doValidate", onSubmit);

    // install the function on the object so that "this" is
    // handled correctly
    form._tempFunc = func;

    // call the submit handler with the doValidate flag,
    var handlerResult = form._tempFunc(doValidate);

    // uninstall the temporary function
    form._tempFunc = (void 0);

    // if we're validating and the handler returns false,
    // don't submit the form
    if (doValidate && (handlerResult == false))
    {
      doSubmit = false;
    }
  }

  if (doSubmit)
  {
    // reset any hidden form values before submitting
    _resetHiddenValues(form);

    //
    // assign any dynamic values before submitting
    //
    var isDOM = _supportsDOM();
    var tempParams = new Object();

    if (parameters)
    {
      for (var paramName in parameters)
      {
        var paramValue = parameters[paramName];
        if (paramValue != (void 0))
        {
          // do not try to get properties from the form element directly.
          // Some code somewhere was setting an htmlInputElement as
          // a property on the formElement, but not as a child.
          // This was causing bug 4536656.
          // I can't yet figure out who is setting the htmlInputElement as
          // a property (instead of a child).
          // As a workaround get them from the elements array instead.
          // In any case it is always safe to get the element from the
          // elements array.
          //var hiddenField = form[paramName];
          var hiddenField = form.elements[paramName];
          if (_agent.isPIE)
          {
            var element = form.elements[paramName];
            element.value = paramValue;
          }
          else
          {
            var hiddenFieldCreated = false;
            // See if the hidden field exists.  And, because
            // of some rather strange IE behavior w/regards to
            // form.elements['id'], make sure we haven't accidentally
            // grabbed a string
            if (hiddenField && (typeof(hiddenField) != "string"))
            {
              // This condition was added to support enter key
              // on forms for hcommandButton
              if (hiddenField.type == 'submit')
              {
                var tmpField = document.createElement("input");
                tmpField.type = "hidden";
                tmpField.name = paramName;
                tmpField.value = parameters[paramName];
                form.appendChild(tmpField);
                tempParams[paramName] = tmpField;
                hiddenFieldCreated = true;
              }
              else
                hiddenField.value = paramValue;
            }
            //VAC- added so that PDA's do not enter this flow. Since no PDA currently
            //supports createElement function on the document.  Furthermore, if the
            //hidden field exists there should be no reason to create a new hidden field
            //with the same name and attach it to the form.
            else
            {
            if (isDOM)
            {
              if (! hiddenFieldCreated)
              {
                // as a convenience to the client, build a hidden field to hold
                // this parameter.
                var tmpField = document.createElement("input");
                tmpField.type = "hidden";
                tmpField.name = paramName;
                tmpField.value = parameters[paramName];
                form.appendChild(tmpField);
                tempParams[paramName] = tmpField;
              }
            }
          }
        }
      }
    }
    }

    // finally submit the form
    if (_agent.isPIE)
    {
      var isPartialForPIE = false;
      var partialTargets = parameters["partialTargets"];
      var partial = parameters["partial"];
      if ((partialTargets != (void 0)) || (partial != (void 0)))
      {
        var data = "";
        data = createNameValueString(form);
        var useragent = navigator.userAgent;
        try
        {
          XmlHttp = new ActiveXObject("Microsoft.XMLHTTP");
        }
        catch(e)
        {
          XmlHttp = null;
          XmlRequest = null;
          var errnum = e.number &amp; 0xFFFFFF;
          if (errnum == 655789)
          {
            alert("\"Run ActiveX controls and plug-ins\" and \"Script ActiveX controls marked safe for scripting\" must be enabled!");
          }
        }
        if( XmlHttp == null)
        {
          form.target="_self";
          form.submit();
          return doSubmit;
        }
        else
        {
          var urlenc = "application/x-www-form-urlencoded; charset=utf-8";
          XmlHttp.open (form.method.toUpperCase(), form.action, false);
          XmlHttp.setRequestHeader ("Content-Type", urlenc);
          XmlHttp.setRequestHeader ("User-Agent", useragent);
          try
          {
            XmlHttp.send(data);
          }
          catch (e)
          {
            XmlHttp.open (form.method.toUpperCase(), form.action, false);
            XmlHttp.setRequestHeader ("Content-Type", urlenc);
            XmlHttp.setRequestHeader ("User-Agent", useragent);
            XmlHttp.send(data);
          }
          var responseString = new String(XmlHttp.responseText);
          try
          {
            xmlDOC = new ActiveXObject("Microsoft.XMLDOM");
            xmlDOC.loadXML(responseString.toString());
          }
          catch (e)
          {
            form.target="_self";
            form.submit();
            return doSubmit;
          }
          try
          {
            var root = xmlDOC.documentElement;
            var pprscriptnodes = root.selectNodes("//pprscripts");
            var pprscript =
              pprscriptnodes.item(0).childNodes.item(0).nodeTypedValue;
            var pprtargetnodes = root.selectNodes("//pprtargets/pprtarget");
            for (var i = 0; i < pprtargetnodes.length; i++)
            {
              var origpprtargetid = pprtargetnodes.item(i).attributes(0).text;
              var tructargetid;
              var ind = origpprtargetid.indexOf("__xc");
              if (ind > -1 )
              {
                tructargetid = origpprtargetid.substring(0,ind);
              }
              else
              {
                tructargetid = origpprtargetid;
              }
              var pprnode = root.selectNodes("//ppr[@target_id='"
                                             + tructargetid
                                             + "']");
              if ((pprnode != null) && (pprnode != (void 0)))
              {
                try
                {
                  var pprtext =
                    pprnode.item(0).childNodes.item(0).nodeTypedValue;
                  var newpprtext = pprtext.toString();
                  if ((pprtext.indexOf("span") > -1)
                      || (pprtext.indexOf("div") > -1))
                  {
                    var re = new RegExp ('\\s*id\\s*=\\s*"'
                                         + origpprtargetid
                                         + '"\\s*', "gi");
                    //var re =new RegExp('span\\s*id\\s*=\\s*"(.*?)"\\s*',"gi");
                    newpprtext = pprtext.replace(re,' id="'
                                                 + origpprtargetid
                                                 + '_ppr_" ');
                    // newpprtext = pprtext.replace(re,'span id="$1_ppr_" ');
                  }
                  // alert(origpprtargetid + " " + tructargetid + " "
                  //       + i + " " +newpprtext);
                  eval("window['"
                       + origpprtargetid
                       + "'].innerHTML=newpprtext+'<script>'+pprscript+'</script>'");
                }
                catch(e)
                {
                  // alert("exception here" + e.message);
                }
              }
            }
          }
          catch (e)
          {

            var elem = form.elements["partial"];
            elem.value="";
            form.target="_self";
            form.submit();
            return doSubmit;
          }
          finally
          {
            xmlHttp = null;
            xmlDoc = null;
            responseString = null;
            root = null;
          }
        }
      }
      else
      {
        form.target="_self";
        form.submit();
        return doSubmit;
      }
    }
    else // ! (_agent.isPIE)
    {
      form.submit();
      if (_blockOnEverySubmit)
        _pprStartBlocking(window);
    }

    // Remove any dynamically added form parameters. We do this for two
    // reasons:
    // 1. IE6 does not return dynamically-added form elements in the form map,
    // so we end up re-adding the same form elements again.
    // 2. If we don't remove them, then subsequent form submits behave like
    // they are PPR requests (because the dynamically added "partial" and
    // "partialTargets" parameters will be on the request).
    // (Bug #3623890. This seems to break on a few Apps pages with bad form
    // setups)
    if (isDOM)
    {
      for (var paramName in tempParams)
        form.removeChild(tempParams[paramName]);
    }
  }

  return doSubmit;
}

/**
 * This function is called when enter key is hit on any form input element.
 * @src if non-null, the ID of the object to fire
 */
function _submitOnEnter(e, frm,src)
{
  if (window.event != (void 0))
    e = window.event;

  var eventSource;
  if (e.srcElement == undefined)
    // Gecko browsers
    eventSource = e.target;
  else
    eventSource = e.srcElement;

  if (!eventSource) return true;
  // Only process for "INPUT": but not for submit and reset
  // buttons
  if(eventSource.tagName == 'A') return true;

  if ((eventSource.tagName == 'INPUT') &&
      (eventSource.type != 'submit') &&
      (eventSource.type != 'reset'))
  {
    if (_getKC(e)==13)
    {
      if (src != (void 0))
      {
        var params = new Object();
        params[src] = src;
        params['source'] = src;

        submitForm(frm,0,params);
      }

      return false;
    }
  }

  return true;
}

/**
 * In some cases we need to hold off on a submit for a while (waiting for the
 * page to complete rendering, waiting for another submit to complete, etc.).
 * This function will save off the state of the submit request for later
 * processing in _submitFormCheck().
 */
function _saveFormForLaterSubmit(form, val, params)
{
    _saveForm = form;
    _saveDoValidate = val;
    _saveParameters = params;
    if (form.target == _pprIframeName)
    {
      _inPartialSubmit = true;
    }
    _submitRejected = true;
}

/**
 * Checks if _submitForm had been called before the form had completely
 * rendered, and if so, recall it. This function is rendered at the end of the
 * form, so it is guaranteed that the form is complete when this is called.
 */
function _submitFormCheck()
{
  if (_submitRejected)
  {
    if (_inPartialSubmit)
    {
      _submitPartialChange(_saveForm, _saveDoValidate, _saveParameters);
      _inPartialSubmit = false;
    }
    else
    {
      submitForm(_saveForm, _saveDoValidate, _saveParameters);
    }
    _saveForm = null;
    _saveDoValidate = null;
    _saveParameters = null;
  }
}

/**
 * Attempts to reset the form, calling
 * any reset function calls registered on the form.
 * The form will be reloaded if any
 * reset function call returns <code>true</code>.
 * This function returns <code>true</code> if the page
 * had to be reloaded, and false otherwise.
 * <p>
 * @param form The form to submit.  This can either be the name of the form
 *             in the current <code>document</code>, the index of the form
 *             in the current <code>document</code> or the form itself.
 */
function resetForm(
  form
  )
{
  var doReload = false;

  // if the form was passed as a form name, get the form object
  if ((typeof form) == "string")
  {
    form = document[form];
  }
  // if the form was passed as a form index, get the form object
  else if ((typeof form) == "number")
  {
    form = document.forms[form];
  }

  // we had better have a form now
  if (!form)
    return false;

  var resetCallbacks= window[ "_" + _getJavascriptId(form.name) + "_Reset"];

  if (resetCallbacks && !doReload)
  {
    for (var i = 0; i < resetCallbacks.length; i++)
    {
      var trueResetCallback = unescape(resetCallbacks[i]);

      doReload = (eval(trueResetCallback));

    }
  }


  if ( doReload )
  {
    window.document.location.reload();
  }
  else
  {
    form.reset();
  }
  _lastDateReset = new Date();
  return doReload;
}


// Create  query string with the data from a given form
function createNameValueString(form) {
  var datatosend = "";
  try
  {
    var arr = form.elements;
    for (var i = 0; i < arr.length; i++)
    {
      try
      {
        var element = arr[i];
        if(element.name)
        {
          if (element.type == "text"
              || element.type == "password"
              || element.type == "textarea"
              || element.type == "hidden")
          {
            datatosend += (element.name + "=" + escape(element.value) + "&");
          }
          else if (element.type.indexOf("select") != -1)
          {
            //PH:selectdata must be initialized to "". Otherwise, results for 
            //selectdata+="stringtoconcatenate" is "undefinedstringtoconcatenate"
            var selectdata ="" ;
            for (var j = 0; j < element.options.length; j++)
            {
              if (element.options[j].selected == true)
                selectdata += element.name + "="
                              + escape(element.options[j].value) + "&";
            }
            if( !selectdata)
            {
              var val = _getValue(element);
              if (val)
              {
                selectdata += element.name + "=" + escape(val) + "&";
              }
            }
            if (selectdata)
            {
              datatosend += selectdata;
            }
          }
          else if (element.type == "checkbox" && element.checked)
            datatosend += ( element.name + "=" + escape(element.value) + "&");
          else if (element.type == "radio" && element.checked == true)
            datatosend +=  (element.name + "=" + escape(element.value) + "&");
        }
      }
      catch (e)
      {
      }
      element = null;
    }
  }
  catch(e)
  {
  }
  return ( datatosend.substring(0, datatosend.length - 1));
}




/**
 * Resets any server-generated hidden values on the form passed in.
 * All hidden form values generated by the server on this form are set to
 * the empty string.
 */
function _resetHiddenValues(
  form
  )
{
  var resetFields = window["_reset" + _getJavascriptId(form.name) + "Names"];
  if (resetFields)
  {
    for (var i = 0; i < resetFields.length; i++)
    {
      var currField;
      if (_agent.isPIE)
      {
        currField = form.elements[resetFields[i]];
      }
      else
      {
        currField = form[resetFields[i]];
      }
      if (currField)
      {
        currField.value = '';
      }
    }
  }
}



/**
 * Returns the value of a form element.
 */
function _getValue(formElement)
{
  var shadowElem = formElement;
  var elementType = formElement.type;

  // When we're dealing with an array of elements, find the
  // real element type by looking inside the array.
  if (!elementType && formElement.length)
  {
    // See bug 3651045;  IE can put "fieldsets" in with
    // form elements!
    for (var i = 0; i < formElement.length; i++)
    {
      elementType = formElement[i].type;
      if (elementType != (void 0))
      {
        shadowElem = formElement[i];
        break;
      }
    }
  }

  if (elementType == "checkbox")
  {
    if (formElement.length)
    {
      for (var i = 0; i < formElement.length; i++)
      {
        // See above for why we check each element's type
        if (formElement[i].type == "checkbox" &&
            formElement[i].checked)
        {
          return formElement[i].value;
        }
      }
    }
    else
    {
      return formElement.checked;
    }
  }
  else if (elementType.substring(0,6) == "select")
  {
    formElement = shadowElem;
    var selectedIndex = formElement.selectedIndex;

    // selectedIndex exists and non-negative
    if (selectedIndex != (void 0) &&
        selectedIndex != null &&
        selectedIndex >= 0)
    {
      var opt = formElement.options[selectedIndex];
      var value = opt.value;
      if (!value)
      {
        // If there's no value, it could be for two reasons:
        //  (1) The user has only specified "text".
        //  (2) The user explicitly wanted "value" to be empty.
        // We can't really tell the difference between the two,
        // unless we assume that users will be consistent with
        // all options of a choice.  So, if _any_ option
        // has a value, assume this one was possibility (2)
        for (var i = 0; i < formElement.options.length; i++)
        {
          if (formElement.options[i].value)
            return value;
        }

        // OK, none had a value set - this is option (1) - default
        // the "value" to the "text"
        return opt.text;
      }

      return value;
    }

    // no selected value
    return "";
  }
  else if (elementType == "radio")
  {
    if (formElement.length)
    {
      for (var i = 0; i < formElement.length; i++)
      {
        // See above for why we check each element's type
        if (formElement[i].type == "radio" &&
            formElement[i].checked)
        {
          return formElement[i].value;
        }
      }
    }
    else
    {
      if (formElement.checked)
      {
        return formElement.value;
      }
    }

    // no selected value
    return "";
  }
  else
  {
    return formElement.value;
  }
}

/**
 * Sets the selected index
 */
function _setSelectIndexById(id, index)
{
  var element = _getElementById(document, id);
  if (element != null)
    element.selectedIndex = index;
}


/**
 * Synchronizes the index of a repeated choice.
 */
function _syncChoiceIndex(ch)
{
  var form = ch.form;
  var name = ch.name;
  var comps = form.elements[name];
  for (i=0; i<comps.length; i++)
  {
    comps[i].selectedIndex = ch.selectedIndex;
  }
}


/**
 * Clears a password field if it contains the magic postback string.
 */
function _clearPassword(field, e)
{
  if (window.event != (void 0))
    e = window.event;

  if (field.value != "******")
    return true;

  // Backspace
  if ((e.keyCode == 8) ||
     // Delete (46) through F1 (112)
      ((e.keyCode >= 46) && (e.keyCode < 112)))
    field.value="";
  return true;
}


/**
 * If appropriate sets the focus on the input passed in
 */
function _setFocus(currInput)
{
  // check if currInput is showing before setting focus, for example
  // ColorField has required validation on hidden field,
  // but cannot receive focus.
  if (_isShowing(currInput))
  {
    if (currInput.focus)
      currInput.focus();

    //PH:element["value"] is not supported for PIE,IEM and BB. Therefore 
    //use element.value which is supported by all
    if ((currInput.type == "text")
        && (currInput.value != (void 0))
        && (currInput.value != null)
        && (currInput.value.length > 0))
    {
      // IE fails on this select if a timeout occurs to handle a
      // pending event. Don't do it if we've reset the delayed
      // events object.
      if (true != _delayedEventParams["reset"])
        currInput.select();
    }
  }
}

/**
 * Calls an array of validation functions and returns a single error
 * String.
 */
function _multiValidate(
  form,
  source,
  validators,
  globalMessageIndex
  )
{
  var failures = "";

  var subforms = window[form.name + "_SF"];
  var ignorePrefixes = new Array();
  var foundUsedSubform = false;
  var key;
  if (source != (void 0))
  {
    // Find if there's any prefix that matches
    for (key in subforms)
    {
      if (source.indexOf(key + ":") == 0)
      {
        foundUsedSubform = true;
        break;
      }
    }

    // Build up all prefixes that don't match
    for (key in subforms)
    {
      if (source.indexOf(key + ":") != 0)
      {
        if ((foundUsedSubform) || (subforms[key] == 1))
          ignorePrefixes.push(key + ":");
      }
    }
  }

  // We check for any relevent validation failures here, not just validations.
  // If a validation has been run on one field in the form (e.g. an onBlur), we
  // still need to run every other validation. However, if that one validation
  // failed, the user has seen one alert, don't bug them with a second til they
  // have fixed the first error.
  if (validators && !_recentValidation(true))
  {
    // get the list of different validations
    var validations = _getValidations(form);

    var firstFailure = true;

    // loop through the validations, building up the error string
    for (var i = 0; i < validators.length; i += 5)
    {
      var isIgnored = false;
      // If this field is one that's specifically being ignored,
      // then don't validate here.
      for (var j = 0; j < ignorePrefixes.length; j++)
      {
        if (validators[i].indexOf(ignorePrefixes[j]) == 0)
        {
          isIgnored = true;
          break;
        }
      }

      if (isIgnored)
        continue;

      // get the current form element to validate
      var currInput = null;
      if (_agent.isPIE)
      {
          currInput = form.elements[validators[i]];
      }
      else
      {
        currInput = form[validators[i]];
        // To support required validation on shuttle component
        if(currInput == undefined)
        {
          currInput = form.elements[validators[i]+":trailing:items"];
        }
      }

      // Make sure we have a non-null input control.  It is possible
      // that in rich client environments the DOM for the input
      // control may have been temporarily removed from the document.
      // If we don't find DOM for the current input, move on to the
      // next input.

      // todo: Should also check for visibility of currInput, since
      //       rich client may have "hidden" the input, in which case
      //       validation shouldn't fire.
      if (!currInput)
        continue;

      var label = _getLabel(form, currInput);

      // if currInput is an array then multiple elements have the same name.
      // Only the first will be validated as subsequent values should be in sync
      var elementType = currInput.type;

      if (!elementType && currInput.length)
      {
        var firstType = currInput[0].type;
        if (firstType != "radio" && firstType != "checkbox")
        {
          currInput = currInput[0];
        }
      }

      var value = _getValue(currInput);
      var required = validators[i+1];
      if ( required && ((value == "" ) || (value == null)))
      {

        // move the focus back to the first failed field
        if (firstFailure)
        {
          _setFocus(currInput);

          firstFailure = false;
        }

        // get the formatted error string for the current input and
        // formatIndex
        requiredFormatIndex = validators[i+2];
        var requiredErrorString = _getErrorString(currInput,
                                                  requiredFormatIndex);

        if (requiredErrorString)
        {
          requiredErrorString = _getGlobalErrorString(currInput, 
                                              globalMessageIndex, 
                                              requiredErrorString,
                                              label);   
          failures += '\n' + requiredErrorString;
        }
      }
      else if (validations)
      {

        var converterInfo = validators[i+3];

        // set the converterError var to false for each input, otherwise nothing
        // after the first conversion error is validated
        var converterError = false;

        if ( converterInfo != null)
        {

          // do the conversion if this element has a value
          if ((value != null) &&
              !((typeof value == "string") && (value == "")))
          {
            // evaluate the converter
            var converterConstructor = validations[converterInfo];

            if (converterConstructor)
            {
              var converter = eval(converterConstructor);
              try{
                value = converter.getAsObject(value, label);
              }
              catch (e)
              {
                converterError = true; 
                // move the focus back to the first failed field
                if (firstFailure)
                {
  
                  _setFocus(currInput);
  
                  firstFailure = false;
                }
  
                // get the formatted error string for the current input
                var errorString1 = e.getFacesMessage().getDetail();
  
                if (errorString1)
                {                         
                  errorString1 = _getGlobalErrorString(currInput, 
                                                       globalMessageIndex, 
                                                       errorString1,
                                                       label);                                         
                  failures += '\n' + errorString1;
                }
              }
            }
          }
        }
        
        if ( converterError == false)
        {
          var validatorInfo = validators[i+4];
          for ( var j = 0; j < validatorInfo.length; j = j + 1)
          {
            // do the validation if this element has a value
            // Don't just compare against "", since the value has
            // already been converted to a non-string type
            if ((value !== null) &&
                 !((typeof value == "string") && value == ""))
            {
              // evaluate the validator
              var validatorConstructor = validations[validatorInfo[j]];
              if (validatorConstructor && value !== undefined)
              {
                var validator = eval(validatorConstructor);

                try {
                  validator.validate(value, label, converter);
                }
                catch (e)
                {
                  // move the focus back to the first failed field
                  if (firstFailure)
                  {
  
                    _setFocus(currInput);
  
                    firstFailure = false;
                  }
  
                  // get the formatted error string for the current input and
                  // formatIndex
                  var errorString = e.getFacesMessage().getDetail();
  
                  if (errorString)
                  {     
                    errorString = _getGlobalErrorString(currInput, 
                                                        globalMessageIndex, 
                                                        errorString,
                                                        label);       
                    failures += '\n' + errorString;
                  }
                }
              }
            }
          }
        }
      }
    }

    _recordValidation((failures.length > 0), 0);
  }

  return failures;
}

/**
 * Used for the converters and validators we provide which all have the form
 *
 * {0} - label
 * {1} - string value
 * {2} - extra param
 * {3} - extra param
 */
function _createFacesMessage(
  key,
  label,
  value,
  param2,  
  param3
)
{  
  var summary = TrMessageFactory.getSummaryString(key);
  var detail = TrMessageFactory.getDetailString(key);
  // format the detail error string
  if (detail != null)
  {
    detail = TrFastMessageFormatUtils.format(detail, label, value, param2, param3);
  }
  return new TrFacesMessage(summary, 
                          detail, 
                          TrFacesMessage.SEVERITY_ERROR);
}

/**
 * Used for the converters and validators we provide which all have the form
 *
 * {0} - label
 * {1} - string value
 * {2} - extra param
 * {3} - extra param
 */
function _createCustomFacesMessage(
  summary,
  detail,
  label,
  value,
  param2,  
  param3
)
{  

  // format the detail error string
  if (detail != null)
  {
    detail = TrFastMessageFormatUtils.format(detail, label, value, param2, param3);
  }
  
  return new TrFacesMessage(summary, 
                          detail, 
                          TrFacesMessage.SEVERITY_ERROR);
}


function _getGlobalErrorString(
  input,
  formatIndex,
  errorString,
  label
  )
{
  var form = _getForm(input);  
  // get the list of different error formats
  var errorFormats = window["_" + _getJavascriptId(form.name) + "_Formats"];

  if (errorFormats)
  {
    // get the appropriate error format
    var errorFormat = errorFormats[formatIndex];

    if (errorFormat && label != null)
    {
        return _formatErrorString(errorFormat,
                                 {
                                   "0":label,
                                   "1":errorString
                                 });
    }
  }   
  
  return errorString;  
}                


/**
 * Returns true if the element is visible such that it could
 * receive focus or have its value selected, otherwise false.
 */
 function _isShowing(
   input)
 { 
   //PH: removed !input.focus because firstly, focus() function is supported by 
   //all browsers (PIE,IEM,BB,FF,IE) and secondly, _isShowing should be treated 
   //as a function to test visibility only. If there is a case where one really 
   //wants to test whether focus function exists or not, do it in an if 
   //statement and call _isShowing within it.
   if (input.type == 'hidden')
       return false;
   
   // determine visibility from style information
   if (_agent.isIE)
   {
     var node = input;

     // IE does not give a "computed" style, so we
     // need to walk up the DOM to get the styles
     while (node != (void 0))
     {
       computedStyle = node.currentStyle;

       if ((computedStyle != (void 0)) &&
           ( (computedStyle["visibility"] == "hidden") ||
             (computedStyle["display"] == "none")))
       {
         // node or one of its parents parents are NOT showing
         return false;
       }

       // consider parent style
       node = node.parentNode;
     }

     // node and all parents are showing
     return true;
   }

   if (_agent.isGecko || _agent.isSafari)
   {
     // Radio buttons:  it'll be an array
     if (!input.ownerDocument && input.length)
       input = input[0];

     var computedStyle = input.ownerDocument.defaultView.getComputedStyle(input,
                                                                          null);
     
     // either of these styles will prevent focus from succeeding
     return ((computedStyle["visibility"] != "hidden") &&
             (computedStyle["display"] != "none"));
   }
   
   return true;
 }

/**
 * Returns the id of an input element on either IE or Netscape, dealing
 * with the fact that Netscape doesn't support IDs locally.
 */
 function _getID(
   input
   )
 {
   if (!_agent.isNav)
   {
     //VAC- bug 4205372 for PIE devices return the name of the input element
     if (_agent.isPIE){
       return input.name;
     }
     // for non-Netscape return the ID directly
     var id = input.id;

     var inputType = input.type;

     if (!inputType && input.length)
       inputType = input[0].type;

     // for radio buttons, return ID of enclosing <span>
     if (inputType == "radio")
     {
       var inputParent;

       if (input.length)
       {
         inputParent = input[0].parentNode;
         if (inputParent.tagName == 'FIELDSET')
           inputParent = inputParent.parentNode;
       }
       else
       {
         inputParent = input.parentNode;
       }

       id = inputParent.id;
     }

     return id;
   }
   else
   {
     var form = _getForm(input);
     // for Netscape, use table lookup
     var nameToID = window["_" + _getJavascriptId(form.name) + "_NameToID"];

     if (nameToID)
     {
       var name = _getName(input);
       return nameToID[name];
     }
   }
 }


/**
 * Returns the form of an input element on either IE or Netscape, dealing
 * with the fact that radio inputs do not directly support the form attribute.
 */
 function _getForm(
   input
   )
 {
   var form = input.form;

   if (form == (void 0))
   {
     // Try the first item of the array
     if (input.length)
     {
       form = input[0].form;
     }
   }

   return form;
 }

/**
 * Returns the name of an input element on either IE or Netscape, dealing
 * with the fact that radio inputs do not directly support the name attribute.
 */
 function _getName(
   input
   )
 {
   var name = input.name;

   if (name == (void 0))
   {
     var inputType = input.type;

     if (!inputType && input.length)
       inputType = input[0].type;

     // for radio buttons, return ID of enclosing <span>
     if (inputType == "radio" && input.length)
     {
       name = input[0].name;
     }
   }

   return name;
 }

/**
 * Return true if the object or any of its prototypes'
 * are an instance of the specified object type.
 */
function _instanceof(
  obj,  // the object instance
  type  // the constructor function
)
{
  if (type == (void 0))
    return false;
    
  if (obj == (void 0))
    return false;

  while (typeof(obj) == "object")
  {
    if (obj.constructor == type)
      return true;

    // walk up the prototype hierarchy
    obj = obj.prototype;
  }

  return false;
}


function _getLabel(
  form,
  input
)
{

  // get the mapping of id's to labels
  var labelMap = window["_" + _getJavascriptId(form.name) + "_Labels"];
  
  var label;
  
  // get the label for this input element, if one has been
  // associated using the ID of the input element
  if (labelMap)
  {
    label = labelMap[_getID(input)];
  }
  
  return label;
}

/**
 * Return the formatted error string for an input field
 * and an errorFormatIndex
 */
function _getErrorString(
  input,
  errorFormatIndex,
  validationError
  )
{
  var errorFormat;

  var form = _getForm(input);
  var value = _getValue(input);

  // use the message embedded in the validationError, if any
  if (_instanceof(validationError, window["TrConverterException"]))
  {
    errorFormat = validationError.getFacesMessage().getDetail();
  }
  // use the message embedded in the validationError, if any
  else if (_instanceof(validationError, window["TrValidatorException"]))
  {
    errorFormat = validationError.getFacesMessage().getDetail();
  }
  else if (errorFormatIndex != (void 0))
  {
    // get the list of different error formats
    var errorFormats = window["_" + _getJavascriptId(form.name) + "_Formats"];

    if (errorFormats)
    {
      // get the appropriate error format
      errorFormat = errorFormats[errorFormatIndex];
    }
  }

  if (errorFormat)
  {
    var label = _getLabel(form, input);
    
    // format the error string, replacing the following tokens
    //   {0}    the value of the label
    //   {1}    the value of the input element
    var errorString = _formatErrorString(errorFormat,
                                         {
                                           "0":label,
                                           "1":value
                                         });
    // return the error
    return errorString;
  }
}




/**
 * Returns the array of validation information used to validate the form at
 * submission time.
 */
function _getValidations(
  form
  )
{
  return window["_" + _getJavascriptId(form.name) + "_Validations"];
}


/**
 * Perform the error validation and return true if there is an error.
 */
function _getValidationError(
  input,           // form element to be validated
  validationIndex, // index of validation code
  validations      // the validations array, if available
  )
{
  if (!validations)
  {
    // get the list of different validations
    validations = _getValidations(input.form);
  }

  if (validations)
  {
    var validator = validations[validationIndex];

    if (validator)
    {
      // get the true validator by replacing any value token with
      // the value of the input
      var trueValidator = validator.replace(/%value%/g, "_getValue(input)");

      // return true/Exception if a validation error has occurred
      return (eval(trueValidator));
    }
  }

  // no error
  return (void 0);
}



/**
 * Performs token replacement on the the error format, replacing each
 * token found in the token Object with the value for that token.
 */
function _formatErrorString(
  errorFormat, // error format string with embedded tokens to be replaced
  tokens       // tokens Object containin token names and values to be replaced
  )
{
  var currString = errorFormat;

  // loop through all of the tokens, replacing them if they are present
  for (var currToken in tokens)
  {
    var currValue = tokens[currToken];

    // if the token has no value, replace it with the empty string
    if (!currValue)
    {
      currValue = "";
    }

    // the tokens are delimited by '{' before and '}' after the token
    var currRegExp = "{" + currToken + "}";

    // support tokens of the form %token% as well as {token}
    currString = currString.replace(new RegExp('%' + currToken + '%', 'g'),
                                    currRegExp);

    // Replace the token.  Don't use String.replace, as the value may
    // include dollar signs, which leads Netscape astray (bug 2242675)
    var indexOf = currString.indexOf(currRegExp);

    if (currValue.indexOf && currValue.indexOf(currRegExp) >= 0)
    {
     var b1 = '';
     for (i=0; i<currValue.length; i++)
     {
       b1 = b1 + 'placeHolderString';
     }  
   
     while (indexOf >= 0)
    {
      currString=(currString.substring(0,indexOf)
           + b1
           + currString.substring(indexOf+currRegExp.length));
      indexOf = currString.indexOf(currRegExp);   
    }    
   
    indexOf = currString.indexOf(b1);
   
    while (indexOf >= 0)
    {  
      currString =(currString.substring(0,indexOf)
           + currValue
           + currString.substring(indexOf+b1.length));      
      indexOf = currString.indexOf(b1);   
    }
  }
  else
    while (indexOf >= 0)
    {
      currString = (currString.substring(0, indexOf)
                      + currValue
                      + currString.substring(indexOf + currRegExp.length));
      indexOf = currString.indexOf(currRegExp);
    }
 }

  // And now take any doubled-up single quotes down to one,
  // to handle escaping
  var twoSingleQuotes = /''/g;
  return currString.replace(twoSingleQuotes, "'");
}


/**
 * Chain two functions together returning whether the default
 * event handling should occur
 */
function _chain(
  evh1,        // event handler 1 string
  evh2,        // event handler 2 string
  target,      // target of event
  event,       // the fired event
  shortCircuit // shortcircuit if handler 1 false
  )
{
  var result1 = _callChained(evh1, target, event);

  if ( shortCircuit && (result1 == false))
    return false;

  var result2 = _callChained(evh2, target, event);

  // since undefined results should be evaluated as true,
  // return false only if either result1 or result2 return false
  return !((result1 == false) || (result2 == false));
}

function _callChained(
  handler,
  target,
  event
  )
{
  if (handler && (handler.length > 0))
  {
    // handle ie case, where we have no event parameter
    if (event == (void 0))
    {
      event = target.window.event;
    }

    // create function so that "return" is handled correctly,
    // use event parameter so that both ie and netscape
    // functions work
    var func = new Function("event", handler);

    // install the function on the object so that "this" is
    // handled correctly
    target._tempFunc = func;

    // evaluate the result
    var result = target._tempFunc(event);

    // clear the temporary function
    target._tempFunc = (void 0);

    // undefined results should be evaluated as true,
    return !(result == false);
  }
  else
  {
    return true;
  }
}

// Enforce the maximum length of a form element
// Returns true if event processing should continue, false otherwise.
function _checkLength(formElement, length, event)
{
  elementLength = formElement.value.length;
  if (elementLength > length)
  {
    // Input is longer than max, truncate and return false.
    // This takes care of the case where the user has pasted in text
    // that's too long. Return true here because the onChange event can
    // continue (now that we've truncated the value). This allows chained
    // handlers to work.
    formElement.value = formElement.value.substr(0, length);
    return true;
  }

  // If less than max length (i.e. within acceptable range), return true
  if (elementLength < length)
    return true;

  // If we've made it to here, we know that elementLength == length

  if (_agent.isIE)
  {
    // in many forms there is a hidden field named "event"
    // Sometimes IE gets confused and sends us that instead of
    // the true event, so...
    if (event["type"] == "hidden")
      event = window.event;
  }

  // If this is a change event, the field has already been updated to a string
  // of the maximum allowable length. This is fine. Continue processing.
  if (event.type == 'change')
    return true;

  // If we've made it to here, we know that this is a keyPress event

  // If the input is something less than a space (e.g. tab, CR, etc.)
  // return true.
  // If key was CTRL-v, which will be used to paste some new text,
  // pass it along.
  if (event)
  {
    if ((event.which < 32)
        || ((event.which == 118) && (event["ctrlKey"])))
      return true;
  }

  // Default return FALSE. If we're here, this is an onKeyPress event, it's a
  // printable character, and elementLength already equals the maximum allowed.
  // We need to return false here to cancel the event otherwise this last
  // character will end up in the input field in position MAX+1.
  return false;
}

/**
 * Cover for document.getElementById that works on IE 4.x
 */
function _getElementById(
  doc,
  id
  )
{
  //PH: Since BB supports getDocumentById use this to obtain the element.
  if(typeof(doc.getElementById) != 'undefined')
  {
    //
    // If we arent' on Internet Explorers before 5,
    // use the DOM way of doing this
    //
    //PH:exclude BlackBerry
    if (((_agent.kind != "ie") || (_agent.version >= 5)) && (!_agent.isBlackBerry))
    {    
      var element = doc.getElementById(id);     
    
      // IE's implementation of getElementById() is buggy.  If
      // the page contains an anchor which has the same name
      // as the requested id, IE will return the anchor, even
      // if the anchor's id attribute is not set.  So, make
      // sure that we actually get back an element with the
      // correct id.  
      if ((element == null) || (element.id == id))
        return element;
      // If we get here, that means that IE has probably returned 
      // an anchor instead of the desired element.  Let's scan
      // the entire DOM tree to find the element we want.
      return _findElementById(doc, id);
    }
    
    return doc.getElementById(id);    
  }   
  //PH:if agent is PIE get elements this way since getElementById is 
  //not supported
  if(_agent.isPIE)
  {
    //if element is not within a form
    if(doc.forms.length == 0)
      return window[id];
    else
      //check to see if element is within the form, if so return the element else do nothing
      for(var i = 0; i<doc.forms.length; i++)
      {
        var f = doc.forms[i];
        if(f[id])
          return f[id];        
      }
      
    //element is not within the form but form(s) is(are) present. 
    return window[id];   
  }  
  
  return doc.all[id];  
  
}

// A recursive method which searches the entire DOM tree
// to find the element with the specified ID
function _findElementById(
  node,
  id
  )
{
  // Check to see if the current node is the node
  // that we are looking for
  if (node.id == id)
    return node;

  // Check all children of the current node
  if (node.childNodes)
  {
    var childNodes = node.childNodes;
    for (var i = 0; i < childNodes.length; i++)
    {
      var foundNode = _findElementById(childNodes.item(i), id);
      if (foundNode != null)
        return foundNode;
    }
  }

  return null;
}

// Returns '?' or '&' depending on whether the
// baseURL already contains a query string
function _getQuerySeparator(baseURL)
{
  var lastChar = baseURL.charAt(baseURL.length - 1);
  if ((lastChar == '&') || (lastChar == '?'))
    return "";

  return (baseURL.indexOf('?') >= 0) ? '&' : '?';
}

/**
 * Adds a parameter to an existing URL, replacing the parameter if
 * it already exists
 */
function _addParameter(
  baseURL,
  paramName,
  paramValue
  )
{
  // check if we have parameters
  var queryIndex = baseURL.indexOf('?');

  if (queryIndex == -1)
  {
    // no parameters, so append to parameters
    return baseURL + '?' + paramName + '=' + paramValue;
  }
  else
  {
    // check if the parameter already exists
    var paramIndex = baseURL.indexOf('?' + paramName + '=', queryIndex);

    if (paramIndex == -1)
      paramIndex = baseURL.indexOf('&' + paramName + '=', queryIndex + 1);

    if (paramIndex == -1)
    {
      // parameter isn't in the URL
      return baseURL + '&' + paramName + '=' + paramValue;
    }
    else
    {
      //
      // replace the value of the parameter
      //
      // the +2 skips over the '&' or '?' and the '='
      var valueIndex = paramIndex + paramName.length + 2;

      // get the URL + the parameter
      var newString = baseURL.substring(0, valueIndex);

      // append the new value
      newString += paramValue;

      var lastIndex = baseURL.indexOf('&', valueIndex);

      if (lastIndex != -1)
      {
        // append the rest of the string after the replaced value
        newString += baseURL.substring(lastIndex);
      }

      return newString;
    }
  }
}

/**
 * Adds a parameter to the parameters object for form submission
 */
function _addFormParameter(
  parameters,
  paramName,
  paramValue
  )
{
  // Always create a new object, since we don't want to mess with
  // the caller's parameters
  var newParameters = new Object();

  // Copy over existing parameters
  if (parameters)
  {
    for (var name in parameters)
      newParameters[name] = parameters[name];
  }

  // Now set the new parameter value
  newParameters[paramName] = paramValue;

  return newParameters;
}

//
// _pprInstallBlockingHandlers: Helps implement blocking
//                              This function just installs or de-installs the
//                              event consuming handlers.
//
function _pprInstallBlockingHandlers(win, install)
{
  var doc = win.document;

  if (doc == (void 0))
    return;

  if (_agent.isIE)
  {
    var el = win._pprConsumeFirstClick;
    if (install)
    {
      // See comment in _pprConsumeFirstClick().
      // If the event that started this PPR chain was an onChange or onBlur,
      // AND the event location is the element on which the change happened
      // (i.e. the user didn't click somewhere outside the element)
      // then we want to make sure that the blocking starts immediately.
      var ev = win.event;
      if (ev != (void 0))
      {
        var destElt = document.elementFromPoint(ev.x, ev.y);
        if (!win._pprFirstClickPass // never attach unless passing first click
            || (((ev.type == 'change') || (ev.type == 'blur'))
                && (ev.srcElement == destElt))
            || (!_isSubmittingElement(destElt)))
        {
          _pprControlCapture(win, true);
          return;
        }
      }

      // If we're here, we didn't set up a capture.
      // For an onClick, we have to pass on the first click,
      // then we'll capture every subsequent event.
      doc.attachEvent('onclick', el);
    }
    else
    {
      doc.detachEvent('onclick', el);
      _pprControlCapture(win, false);
    }
  }
  else // Gecko or other standards based browser
  {
    var el = win._pprConsumeBlockedEvent;

    // Set up the same handler on all these events. The handler will just eat
    // the event unless it's the first click and we're passing that.
    var handlers = { 'click':1, 'keyup':1, 'keydown':1, 'keypress':1};
    for (var h in handlers)
    {
      if (install)
        doc.addEventListener(h, el, true);
      else
        doc.removeEventListener(h, el, true);
    }
  }
}

/**
 * function used to store the global variables
 * needed to load the libraries for IE
 */
function _pprLibraryStore(numberOfLibraries)
{
  // keep track of which libraries are loaded
  this.loadedStatus = new Array(numberOfLibraries);

  for (var i=0; i < numberOfLibraries; i++)
    this.loadedStatus[i] = false;

  // keep track of the total number of libraries to load
  this.total = numberOfLibraries;

  // an array to store each library script
  this.allLibraries = new Array(numberOfLibraries);
}


//
// _pprStartBlocking: Starts consuming every click (to implement blocking)
//
function _pprStartBlocking(win)
{
  // In order to force the user to allow a PPR update to complete, we
  // block all mouse clicks between the start of a PPR update, and the end.
  // We do this by building a dummy DIV element and having it grab all clicks.
  // On Mozilla, we just expand it to cover the entire body as a glass frame.
  // On IE, we leave the DIV at zero size, but route every click to it.
  if (!win._pprBlocking)
  {
    var body = win.document.body;
    win._pprBlockStartTime = new Date();

    // XXXSafari: What to do for Safari? Safari will probably work like gecko,
    //            but... need to check.
    if (_agent.isGecko)
    {
      // If the user clicks the stop button, then we'll be stuck blocking.
      // So we don't hang, this timeout will clear the block in eight
      // seconds whether we've finished or not, but first we clear any
      // previously existing timeout.
      if (win._pprBlockingTimeout != null)
      {
        win.clearTimeout(win._pprBlockingTimeout);
      }
      win._pprBlockingTimeout = win.setTimeout("_pprStopBlocking(window);",
                                               8000);
    }
    else if (_agent.isIE)
    {
      // save off the element we'll return focus to
      _pprEventElement = window.document.activeElement;
    }
    _pprInstallBlockingHandlers(win, true);
    win._pprBlocking = true;
  }
}

//
// _pprStopBlocking: Finishes up the blocking, releases the page back
//                   to normal processing
//
function _pprStopBlocking(win)
{
  var doc = win.document;

  if (win._pprBlocking)
  {
    // XXXSafari: What to do for Safari? Safari will probably work like gecko,
    //            but... need to check.
    if (_agent.isGecko)
    {
      // If we've set a timeout, clear it now.
      if (win._pprBlockingTimeout != null)
      {
        win.clearTimeout(win._pprBlockingTimeout);
        win._pprBlockingTimeout == null;
      }
    }
    // and turn off the event capture
    _pprInstallBlockingHandlers(win, false);

    win._pprEventElement = null;
    win._pprBlocking = false;
  }
  win._pprBlocking = false;
}

/*
 * After updates we often can't just set focus to a node, it has to be prepared
 * in a browser specific way (different idosyncracies cause poor focus
 * behavior).
 */
function _pprFocus(node, doc)
{
  if (_agent.isIE)
  {
    // If the node's parent has changed through a DOM update then
    // this node hasn't been fully added to the tree yet so we
    // can't set focus to it.
    if (node.parentNode == null)
      return;

    // On IE, if a node has focus and we update it, then setting focus to it
    // seems to have no effect. Setting the focus to another node, then back to
    // the target seems to work correctly. Here we set the focus to a hidden
    // field.
    var divnode = _getElementById(doc, _pprdivElementName);
    if ((divnode) && (divnode["focus"]))
      divnode.focus();
  }
  node.focus();
}

//
// _pprControlCapture: Set up the pprDivElement to capture all
//                     mouse events. It will then ignore them.
//
function _pprControlCapture(win, set)
{
  // This is an IE only function
  if (_agent.isIE)
  {
    var doc = win.document;
    var body = doc.body;
    var divElement = _getElementById(doc, _pprdivElementName);
    if (divElement)
    {
      if (set)
      {
        divElement.setCapture();
        // If we've got an element to return focus to,
        // then capture keyboard events also.
        if (win._pprEventElement)
          divElement.focus();
        // save current cursor and display a wait cursor
        win._pprSavedCursor = body.style.cursor;
        body.style.cursor = "wait";
        win._pprSavedCursorFlag = true;
      }
      else if (win._pprSavedCursorFlag)
      {
        divElement.releaseCapture();

        // return focus to the post-PPR target element
        if (win._pprEventElement)
          win._pprEventElement.focus();
        body.style.cursor = win._pprSavedCursor;
        win._pprSavedCursor = null;
        win._pprSavedCursorFlag = false;
      }
    }
  }
  return;
}

// handle the onClick or onBlur for an IE SELECT element
// Returns true if the user has finally made a selection, and is ready to go.
function _pprChoiceAction()
{
  // this function is only needed to handle IE's weird select element
  if (!_agent.isIE)
    return true;

  var rv = false;

  // This gets called as both onClick and onBlur, but both really only want
  // to submit the event if a change has been made.
  if ((!window._pprBlocking) && (_pprChoiceChanged))
  {
    // clear the choice tracker
    _pprChoiceChanged = false;
    rv = true;
  }
  return rv;
}

// handle the onChange for an IE SELECT element
function _pprChoiceChangeEvent(event)
{
  if (!_agent.isIE)
    return true;

  // Just remember the fact that a change has occurred.
  if (!window._pprBlocking)
    _pprChoiceChanged = true;

  return true;
}


// Tests whether a partial submit should be performed
function _doPartialSubmit()
{
  //VAC added because Pocket IE does not use Iframes for PPR, but it does
  //do PPR.
  if (_agent.isPIE){
    return true;
  }
  return (_getElementById(document, _pprIframeName) != null);
}


// Fires a partial page request via form submission.
// The args are the same as submitForm().  The
// partialTargets are passed in as parameters
function _submitPartialChange(
  form,
  doValidate,
  parameters)
{
  // If there's no PPR iframe, then just perform a normal,
  // full-page submission.
  if (!_doPartialSubmit())
    return submitForm(form, doValidate, parameters);

  // Get the actual form object
  if ((typeof form) == "string")
    form = document[form];

  if (!form)
    return false;

  // Tack on the "partial" event parameter parameter
  parameters = _addFormParameter(parameters, _getPartialParameter(), "true");

  // We need to set the target of the form to be the PPR iframe.
  // Save the old target and set the new one.
  var oldTarget = form.target;
  if(!_agent.isPIE)
  {
    form.target = _pprIframeName;
  }
  
  // Before we fire, update the request count
  _pprRequestCount++;

  var delta = 0;

  // IE only adds to the history list if something has already been done on
  // this page, so until there has been some action, we don't
  // increment/decrement at all.
  if (!_agent.isIE || window._pprSomeAction)
  {
    delta = 1;
  }
  _pprSubmitCount += delta;

  window._pprSomeAction = true;

  // block all mouse clicks until the submit is done
  if (!_agent.isPIE)
  {
    _pprStartBlocking(window);
  }

  // Submit the form
  var submitted = submitForm(form, doValidate, parameters);

  // If the form wasn't actually submitted, update the ref count
  if (!submitted)
  {
    if (!_agent.isPIE)
    {
      _pprStopBlocking(window);
    }
    _pprRequestCount--;
    _pprSubmitCount -= delta;
  }

  // Reset the form target
  form.target = oldTarget;
}

/**
 * Returns the value of the "partial" event
 * parameter.
 */
function _getPartialParameter()
{
  return "partial";
}


function _partialRedirect(URL)
{
  // ER #2603173
  // If an error occurred during a partial operation, the client
  // can call the method PartialPageUtils.forceRedirectURL(context, url)
  // with an URL to which we can redirect to show the error.
  if (URL && (parent._pprRequestCount > 0))
  {
    if (((typeof URL) == "string") && (URL.length > 0))
    {
      // We're going to an error page, make sure we clear all
      // tracking variables before the onUnload gets called.
      parent._pprRequestCount--;
      parent._pprSubmitCount = 0;
      parent._pprSomeAction = false;
      parent.location.href = URL;
      _pprStopBlocking(parent);
    }
  }
}


/**
 * returns an array with the libraries that need to be loaded due to the
 * PPR request and are not already cached.
 */
function _createToLoadArray()
{
  var toLoadArray = new Array();
  var toLoadIndex = 0;

  if (window["_pprLibraries"] != undefined)
  {
    // loop through each library in _pprLibraries
    // if it is not in the cached libraries list, then
    // add it to an array which indicates that the library needs to be loaded.
    for (var i=0; i < _pprLibraries.length; i++)
    {

      if ((parent._cachedLibs == null)
          || (parent._cachedLibs.indexOf(_pprLibraries[i]) == -1))
      {
        toLoadArray[toLoadIndex++] = _pprLibraries[i];
      }
    }
  }

  return toLoadArray;
}

function _addLibraryToCache(libraryName)
{
  // always load ScriptEval, so don't put it in the _cachedLibs list.
  if ((libraryName.indexOf("ScriptEval")) == -1)
  {
    if (parent._cachedLibs == null)
      //=-=jmw does not work in some cases in IE for some unknown reason
      // parent._cachedLibs = new String(libraryName);
      parent._cachedLibs = "" + libraryName;
    else
      parent._cachedLibs += "," + libraryName;
  }
}

// Given the targetDocument and the array of libraries to load,
// download the libraries using IE's startDownload function.
function _loadScriptLibrariesIE(targetDocument, toLoadArray)
{
  if (toLoadArray == null) return;

  var downloadElement   = _getElementById(targetDocument, "_adfDownload");

  if (downloadElement == null) return;

  var numLibraries = toLoadArray.length;

  // initialize the global variables that are needed to load the libraries
  // in the callback.
  _pprLibStore = new _pprLibraryStore(numLibraries);

  // For each library, start the download and set up the callback
  // which will get called when the download is complete.
  for (var i = 0; i < numLibraries; i++)
  {
    var scriptStatement = "_pprExecScript(" + i + ", s);"
    // create a callback function that takes "s" as an argument
    // (this will be the script's content in string form) and
    // takes the scriptStatement as its body. This callback
    // function will then turn around and call _pprExecScript(i,s),
    // where i is the library index and s is the library string
    var downloadCallback = new Function("s",scriptStatement );
    downloadElement.startDownload(toLoadArray[i], downloadCallback);

    _addLibraryToCache(toLoadArray[i]);

  }
}

/**
 * Download the libraries that are defined in the iframe
 * into the targetDocument. The method for Gecko is
 * to create a script element in the targetDocument for
 * each library.
 */
function _loadScriptLibrariesGecko(targetDocument, toLoadArray)
{
  // we first walk through the list of library paths that are stored in
  // the _pprLibraries variable in the iframe.
  // As we get each path, we create a script element
  // with the library path as the src and add it to the target document.

  var iFrameElement   = _getElementById(targetDocument, _pprIframeName);

  if (iFrameElement)
  {
    for (var i = 0; (i < toLoadArray.length); i++)
    {
      // first check to see if this library is already loaded.
      // if so, don't bother to load it again. However, always load
      // ScriptEval

       var newScriptElement= targetDocument.createElement("script");
       newScriptElement.setAttribute('src', toLoadArray[i]);

       iFrameElement.parentNode.insertBefore(newScriptElement, iFrameElement);

      _addLibraryToCache(toLoadArray[i]);
    }
  }
}


/**
 * Download the libraries that are defined in the iframe
 * into the targetDocument. The last library will
 * always be ScriptEval.js, which will evaluate the
 * scripts defined in the _pprScripts variable.
 */
function _loadScriptLibraries(targetDocument)
{
  if (window["_pprLibraries"] != (void 0))
  {
    var toLoadArray = _createToLoadArray();

    if (toLoadArray.length > 0)
    {
      if (_agent.isIE)
      {
        _loadScriptLibrariesIE(targetDocument, toLoadArray);
      }
      else
      {
        _loadScriptLibrariesGecko(targetDocument, toLoadArray);
      }
    }
  }
}



/* If the Trinidad facility needs to set focus to a particualr node after a PPR
 * update, calling this function saves off the data needed to find that node
 *
 * Args:
 *    doc : The document that the node lives in
 * nodeid : The id attached to the desired node
 *   next : If true, we'll try to focus on the node following the one above,
 *          otherwise, we'll try to focus on the requested node.
 */
function _setRequestedFocusNode(doc, nodeid, next, win)
{
  // degenerate case - default to something that won't cause an error
  if (win == (void 0))
    win = window;

  // we only allow one outstanding focus request
  win._AdfFocusRequestDoc = doc;
  win._AdfFocusRequestID = nodeid;
  win._AdfFocusRequestNext = (next == true);
}


/* If a request was made to focus on a particular node, this function will
 * attempt to get that node.
 */
function _getRequestedFocusNode(win)
{
  // degenerate case - default to something that won't cause an error
  if (win == (void 0))
    win = window;

  if ((win._AdfFocusRequestDoc != null)
      && (win._AdfFocusRequestID != null))
  {
    var element = _getElementById(win._AdfFocusRequestDoc,
                                  win._AdfFocusRequestID);
    if (!element)
      return null;

    if (win._AdfFocusRequestNext)
    {
      // If "next" was set, the caller doesn't want this node, but the next
      // one. Try to find something that'll accept focus.
      for (var next = element.nextSibling;
           next != null;
           next = next.nextSibling)
      {
        if (_isFocusable(next)
            // we actually DO want to "tab" to links
            || ((_agent.isIE) && (next.nodeName.toLowerCase() == 'a')))
        {
          element = next;
          break;
        }
      }
    }
    return element;
  }
  return null;
}

/**
 * Onload event handler for the partial iframe for full page response.
 * Takes two parameters: the onload and onunload handlers for the
 * full page.
 */
function _fullChange()
{
  // Update the request count
  if (parent._pprRequestCount > 0)
  {

    parent._pprRequestCount--;

    // Before we get the contents, insert a script which disables
    // document.write().  That way, when we write out the contents
    // to the parent window, we won't duplicate calls to document.write()
    // that have already been performed.
    var disableWriteItem = _getElementById(document, "_pprDisableWrite");
    disableWriteItem.text = "var _pprDocumentWrite = document.write;" +
                            "var _pprDocumentWriteln = document.writeln;" +
                            "document.write = new Function('return;');" +
                            "document.writeln = new Function('return;');";

    // Insert a script which re-enables document.write() after we've
    // finished writing out the contents to the parent window.
    var enableWriteItem = _getElementById(document, "_pprEnableWrite");
    enableWriteItem.text = "document.write = _pprDocumentWrite;" +
                           "document.writeln = _pprDocumentWriteln";

    // We also need to replace the onload/onunload event handlers
    // before we get the contents for the parent window.
    var bodyItem = document.body;

    // Save the old event handlers
    var iframeOnload = bodyItem.getAttribute("onload");
    var iframeOnunload = bodyItem.getAttribute("onunload");

    // Set up the new event handlers
    bodyItem.setAttribute("onload",
      _getCommentedScript(document, ("_pprFullOnload")));
    bodyItem.setAttribute("onunload",
      _getCommentedScript(document, ("_pprFullOnunload")));


    // Get the full content of the iframe document.  This should now have
    // the correct uncommented version of our partial page scripts, plus
    // scripts to disable and re-enable document.write().
    var content = _getDocumentContent();

    // We do not want the _fullChange script to be called again.
    // It will cause a javascript error if it is. Therefore,
    // we need to clear it out.
    var pattern =
      new RegExp("<script id=[\"]*_pprFullChange.*>_fullChange\\(\\)</script>","i");
    content = content.replace(pattern, "");

    // Reset the event handlers
    bodyItem.setAttribute("onload", iframeOnload);
    bodyItem.setAttribute("onunload", iframeOnunload);

    // Write the full content to the parent window
    var targetDocument = parent.document;

    if (_agent.isIE)
    {
       // Explicitly set the charset on the target document
       // before writing contents - see bug 3296281
       var sourceCharset = document.charset;
       targetDocument.open();
       targetDocument.charset = sourceCharset;
    }

    targetDocument.write(content);
    targetDocument.close();

    // the scripts for the page will be loaded onLoad, see _checkLoad

  }
}



// Returns the first focusable node under the specified node
function _getFirstFocusable(node)
{
  if ((node == null) || _isFocusable(node))
    return node;

  if (node.hasChildNodes)
  {
    var children = node.childNodes;
    for (var i = 0; i < children.length; i++)
    {
      var child = children[i];
      var firstFocusable = _getFirstFocusable(child);
      if (firstFocusable != null)
        return firstFocusable;
    }
  }

  return null;
}

// Restores the focus to the specified node
function _restoreFocus(node, isFirstFocusable, doc)
{
  if (node == null)
    return;

  // If we are in a scrolled DIV, restoring the focus to the
  // first focusable node may cause the DIV to scroll back to 0,0.
  // So, for now we just avoid restoring the focus in this situation.
  // In the future we should see less cases where scrolling occurs,
  // since we should do a better job locating the correct node to
  // receive the focus.
  var divNode = _getAncestorByName(node, "DIV");
  if (!divNode)
  {
    _pprFocus(node, doc);
  }
  else
  {
    var scrollTop = divNode.scrollTop;
    var scrollLeft = divNode.scrollLeft;

    // If we aren't scrolled at all, or if we are restoring the
    // focus to the correct focusable owner (and not just the
    // first focusable node), then restore the focus.  Otherwise,
    // we do nothing, in order to avoid unnecessary scrolling.
    if (((scrollTop == 0) && (scrollLeft == 0)) || !isFirstFocusable)
    {
      _pprFocus(node, doc);
    }
  }

  // Bug #2753958: IE doesn't seem to want to re-set the focus when we're
  // done with a PPR update if the input element happens to be enclosed
  // within a table. However, if we make a second request, the focus is set
  // correctly. This is limited to the one interesting case.
  if ((_agent.isIE)
      && (node.tagName == 'INPUT')
      && (_getAncestorByName(node, 'TABLE')))
  {
    _pprFocus(node, doc);
  }
}

// Returns an ancestor with the specified name
function _getAncestorByName(
  node,
  ancestorName
  )
{
  ancestorName = ancestorName.toUpperCase();

  while (node)
  {
    if (ancestorName == node.nodeName)
      return node;

    node = node.parentNode;
  }

  return null;
}

// Tests whether one node is a descendent of another
function _isDescendent(
  node,
  ancestorNode
  )
{
  if (node == null)
    return false;

  while (node.parentNode)
  {
    if (node == ancestorNode)
      return true;

    node = node.parentNode;
  }

  return false;
}

// Tests whether the specified node is focusable
function _isFocusable(node)
{
  if (node == null)
    return false;

  var name = node.nodeName.toLowerCase();

  // Links that have a destination are generally focusable
  if (('a' == name) && (node.href))
  {
    // We need to be careful on IE - it seems that
    // IE has problems setting the focus to links
    // which contain a single image.  We see this when
    // IE tries to set the focus to the link around the
    // previous icon in the table.  Actually, this does
    // not seem to be a problem if the link has its
    // id set, so we first check for that.

    // If we're not on IE, or if the link has an id,
    // the link should be focusable
    if (!_agent.isIE || (node.id))
      return true;

    // If we're on IE, we only consider the link to be
    // focusable if it has something other than a single
    // image for its contents.
    var children = node.childNodes;
    if ((children) && (children.length == 1))
    {
      var childName = children[0].nodeName;
      if ('img' == childName.toLowerCase())
        return false;
    }

    return true;
  }

  // Blow off any disabled elements
  if (node.disabled)
    return false;

  // Input elements are also usually focusable
  if ('input' == name)
  {
    // But don't set the focus to hidden fields
    return (node.type != 'hidden');
  }

  // Catch everything else here...
  return (('select' == name) ||
          ('button' == name) ||
          ('textarea' == name));
}

// The partial page response includes a single script element
// which contains all of the code that needs to be executed when
// the partial page changes are applied.  Oddly enough, the code
// is actually commented out to prevent execution within the
// iframe context.  We need to pull the code out from the comment
// braces (/* */) and execute the code in the parent window's
// context.  This function returns the uncommented script code
// that needs to be executed in the parent window.
function _getCommentedScript(doc, id)
{
  var scriptItem = _getElementById(doc, id);

  if (scriptItem != null)
  {
    var scriptContents;
    if (_agent.isSafari)
      scriptContents = scriptItem.innerHTML;
    else
      scriptContents = scriptItem.text;

    // Strip off the start/end comments
    var start = 0;
    var end = scriptContents.length - 1;

    while (start < end)
    {
      if (scriptContents.charAt(start) == '*')
        break;

      start++;
    }

    while (end > start)
    {
      if (scriptContents.charAt(end) == '*')
        break;

      end--;
    }

    // Rip out the actual script contents from within the comments
    return scriptContents.substring(start + 1, end);
  }

  return null;
}

// Evaluates the specified code in the target window
function _eval(targetWindow, code)
{
  if (code == null)
    return;

  // For IE, we use window.execScript().  For Mozilla, we use
  // window.eval().  It would be nice if we could use eval() on
  // IE too, but IE's implementation of eval() always executes
  // the script in the current context, even if some other
  // window is specified.
  if (_agent.isIE)
    targetWindow.execScript(code);
  else
    targetWindow.eval(code);
}

// Returns the full content (outer HTML) of the current document
function _getDocumentContent()
{
  // Would be nice if we could also include the doctype!

  if (_agent.isIE)
    return document.documentElement.outerHTML;

  // We use innerHTML to get the content on Mozilla, so we
  // need to build up the <html> element manually
  var content = "<html"

  // We need to explicitly add in any attrs
  var attrs = document.documentElement.attributes;
  for (var i = 0; i < attrs.length; i++)
  {
    content += " ";
    content += attrs[i].name;
    content += "=\""
    content += attrs[i].value;
    content += "\"";
  }

  content += ">";

  // Add in the inner content
  content += document.documentElement.innerHTML;

  // Close up the content
  content += "</html>";

  return content;
}

//
// Transform all links on a page to (conditionally)
// perform a form submit (using the given form).
// The targetWindow is the window in which the links
// will appear.  If this is called in response to a
// partial page update, the targetWindow is the
// parent window - and not the current (iframe's)
// window.
//
function _fixAllLinks(formName, targetWindow, exclude)
{
  // Save off the initial state of this form.
  _initialFormState = _getFormState(formName, exclude);
  _initialFormStateName = formName;
  if (exclude != (void 0))
    _initialFormExclude = exclude;

  // If we are collecting state for a different window,
  // copy the state over to the target window
  if (window != targetWindow)
  {
    if (targetWindow._initialFormState == null)
      targetWindow._initialFormState = new Object();

    var sourceWindowState = _initialFormState;
    var targetWindowState = targetWindow._initialFormState;

    for (var key in sourceWindowState)
      targetWindowState[key] = sourceWindowState[key];
  }

  var links = document.links;
  var currHrefWithAnchor = targetWindow.location.href + '#';
  var iframeWithAnchor = location.href + '#';

  // Look through all of the links
  for (var i = 0; i < links.length; i++)
  {
    var href = links[i].href;
    // If the link is actually pointing to anchor on this page, or
    // to javascript: or mailto:, don't mess with it!
    // Bug #3369822: A destination of '#' in the iFrame comes back as the full
    // submitted URL, so have to check against it too.
    if (!href
        || (href.substr(0, iframeWithAnchor.length) == iframeWithAnchor)
        || (href.substr(0, currHrefWithAnchor.length) == currHrefWithAnchor)
        || (href.substr(0, 11).toLowerCase() == "javascript:")
        || (href.substr(0, 7).toLowerCase() == "mailto:")
        || (href.indexOf("_noSv=M") >= 0))
    {
      continue;
    }

    // Similarly, any link with a target frame should be ignored
    if (links[i].target)
    {
      continue;
    }

    // Encode single-quotes correctly
    var hrefArr = href.split("'");
    href = hrefArr[0];
    for (var j = 1; j < hrefArr.length; j++)
      href = href + "\\'" + hrefArr[j];

    // Bug 2807885: setting "href" actually decodes the href -
    // at least it does on Mozilla and IE.  (It doesn't on Netscape 4.)
    if (!_agent.isNav)
      href = escape(href);

    links[i].href = "javascript:_submitNav('" + formName + "','" + href + "')";
  }
}


//
// Determines if a field is excluded;  we exclude a field
// if:
//  (1) its name is specifically excluded, or
//  (2) its name is of the form "XYZ:foo", and "XYZ" is excluded
//
function _isInExclude(exclude, elementName)
{
  if (exclude != (void 0))
  {
    if (exclude[elementName] != (void 0))
      return true;
    var lastColon = elementName.lastIndexOf(':');
    if (lastColon < 0)
      return false;

    return _isInExclude(exclude, elementName.substring(0, lastColon));
  }

  return false;
}


//
// Store the current value of all elements in a form.
// Ignores all hidden fields.
//
function _getFormState(formName, exclude)
{
  var state = new Object();
  var frm = document[formName];

  for (var i = 0; i < frm.length; i++)
  {
    var name = frm.elements[i].name;
    if (name)
    {
      var element = frm[name];
      if (element)
      {
        if ((exclude != (void 0)) && _isInExclude(exclude, name))
          continue;

        // Skip over hidden values
        if (!element.type || (element.type != 'hidden'))
          state[name] = _getValue(element);
      }
    }
  }

  return state;
}

/**
 * Determines if the "navigation form" is dirty, returning true if
 * the form is dirty.  This function can only be called if
 * the "navigationFormName" attribute was set on the Trinidad body component.
 *
 * @return true if the navigation form is dirty
 */
function isNavDirty()
{
  var dirty    = false;

  if (_navDirty)
    dirty = true;
  else
  {
    // Get the current state of the form
    var newState = _getFormState(_initialFormStateName, _initialFormExclude);
    // Compare it to the old state
    for (var key in newState)
    {
      if (newState[key] != _initialFormState[key])
      {
        dirty = true;
        break;
      }
    }
  }

  return dirty;
}


//
// Add a name to the list of excluded names
//
function _addNavExclude(exclude)
{
  if (_initialFormExclude != (void 0))
    _initialFormExclude[exclude] = 1;
  else
  {
    _initialFormExclude = new Object();
    _initialFormExclude[exclude] = 1;
  }
}

//
// Submit a form navigation.  Note that we only bother submitting if
// the form is "dirty"; this relies on fixAllLinks() having been called
// to store the state of that form.
//
//
function _submitNav(formName, uri)
{
  // The form is "dirty": send a "navigate" event, and store
  // off the URL that we would have navigated to
  if (isNavDirty())
  {
    var onNav = window["_onNavigate"];
    if ((onNav == (void 0)) || !(onNav(formName, uri) == false))
    {
      var navigate = window['_navEvent'];
      if (navigate == (void 0))
        navigate = 'navigate';
      submitForm(formName, 0, {'event':navigate,'uri':uri});
    }
  }
  // Nope, not dirty: just navigate to the page
  else
    document.location.href = uri;
}


/**
 * Called to identify the input field from an event
 * This is called not only below, but also from LovInput.js.
 */
function _getInputField(event)
{
  var input = (void 0);
  var src = (void 0);

  if (window.event)
  {
    kc = window.event.keyCode;
    src = window.event.srcElement;
  }
  else if (event)
  {
    kc = event.which;
    src = event.target;
  }

  if (src != (void 0)
      && (src.tagName == "INPUT" ||
          src.tagName == "TEXTAREA" ))
    input = src;

  return input;
}

/**
 * Called when a field receives focus.
 * Prepares for a later reset of this field by saving its current value.
 */
function _enterField(
  event
  )
{
  var input;
  var src;
  var retv = true;

  var input = _getInputField(event);

  if (input != (void 0))
  {
    input.form._mayResetByInput = false;

    if (input != window._validating)
    {
      // save the last valid value for later restoration
      input._validValue = input.value;
    }
    retv = false;
  }

  return retv;
}

/**
 * Resets the form input to its last valid value.
 * This function is called from the onKeyDown for a form input.
 * If called twice in succession for the same form, with the
 * escape keycode both times, this function will reset the form.
 */
function _resetOnEscape(event)
{
  var kc;
  var input = _getInputField(event);

  if (input != (void 0))
  {
    var form = input.form;

    if (kc == 27)  // escape keycode
    {
      // reset the form input to its last valid value
      // providing there is no selection (consistent with IE)

      var hasSelection = false;

      if ((input.selectionStart != (void 0)) &&
          (input.selectionEnd   != (void 0)))
      {
        hasSelection = (input.selectionStart != input.selectionEnd);
      }
      else if (document.selection)
      {
        hasSelection = (document.selection.createRange().text.length != 0);
      }

      if (!hasSelection)
      {
        // always reset the field
        input.value = input._validValue;

        // determine if a full form reset is required
        if (form._mayResetByInput == true)
        {
          // reset the form
          // unset the flag for form reset
          form.reset();
          form._mayResetByInput = false;
        }
        else
        {
          // set the flag for form reset
          form._mayResetByInput = true;
        }
      }

      // consume this event to prevent any browser behavior from kicking in
      return false;
    }
    else // any keycode other than escape
    {
      // unset the flag for form reset
      // since some other key was pressed
      form._mayResetByInput = false;
    }
  }
  return true;
}

/**PH:  Currently, if a browser supports PPR, the _checkLoad function
 * is set as the body onload to perform some initialization, both PPR related
 * and not (such as setting the initial focus).
 * Because this function was not called for non-PPR browsers (like BlackBerry
 * 4.0), the non-PPR initialization was not happening on those browsers.
 * Therefore, I created another function called _checkLoadNoPPR to handle
 * non-PPR related initialization, such as setting the initialFocus, and
 * set the body onload to this method for browsers that do not support PPR.
 */
function _checkLoadNoPPR()
{
  if(_initialFocusID != null)
    _setFocus(_getElementById(document,_initialFocusID)); 
}

/**
 * Called by the load handler of each document body to prepare event handlers
 * for forms and fix links to submit the navigation form.
 */
function _checkLoad(
  event,
  navFormName,
  excludeList
  )
{
  // set focus to the window if a dialog. This fixes the bug where our dialog
  // windows don't have focus, so the first keystroke is ignored. 3544304
  // if I used window.focus(), I caused this bug 3876472 -
  // PAGES COME TO THE FOREGROUND WHEN THE PAGE LOADS
  // We are using _pprdivElementName cuz we need an empty div to set focus
  // to. If we get rid of this element, we'll need to set focus to
  // another element that we know is always on the page.

  // This was causing focus to go off and NEVER COME BACK in the shopping cart
  // demo. I think we can limit this to just a dialog if we can detect that
  // we're in one. For now, we'll have to live with the extra keystroke.
  /*
  if (_agent.isIE)
  {
    var divElement = _getElementById(document, _pprdivElementName);
    if (divElement && divElement.focus)
      divElement.focus();
  }
  */

  // first restore page state
  restorePartialPageState();

  if (document.forms)
  {
    for (var i = 0; i < document.forms.length; i++)
    {
      var form = document.forms[i];

      // Note: event listener functions must already be defined above
      //       no forward references
      if (form.addEventListener) // DOM events
      {
        form.addEventListener('focus', _enterField, true);
        form.addEventListener('keydown', _resetOnEscape, true);
      }
      else if (form.attachEvent) // IE5 events
      {
        form.attachEvent('onfocusin', _enterField);
        form.attachEvent('onkeydown', _resetOnEscape);
      }
    }
  }

  if (navFormName != (void 0))
  {
    var exclude;
    if (_initialFormExclude != (void 0))
      exclude = _initialFormExclude;
    else
      exclude = new Object();

    if (excludeList != (void 0))
    {
      for (var j = 0; j < excludeList.length; j++)
        exclude[excludeList[j]] = 1;
    }

    _fixAllLinks(navFormName, window, exclude);
  }

  // If we're inside a frameset, and the top frame wants
  // reloads blocked, install a _noReload handler.
  // chain _monitor also if that is already set.
  var topWindow = _getTop();
  
  if ((self != topWindow) && topWindow["_blockReload"])
  {
    // If _monitor is already set on document.onkeydown, then
    // chain _noReload and _monitor by calling another function which
    // calls both: _monitorNoReload.
    // _monitor is set on IE only.
    if ((_agent.isIE)
        && (document.onkeydown != null)
        && (((document.onkeydown).toString().indexOf('_monitor')) > 0))
    {
      document.onkeydown = _monitorNoReload;
    }
    else
    {
      document.onkeydown = _noReload;
    }
  }

  // Set initialFocus if necessary
  if ((!_agent.isNav) && (_initialFocusID != null))
  {
    var myElement = _getElementById(document,_initialFocusID);

    //PH: Set Focus on element for all browsers.
    if(myElement)
      _setFocus(myElement);
  }  
  
  if (!_agent.isNav)
    _loadScriptLibraries(document);
}

//
// Event handle that blocks keys that lead to a page reloading.
//
function _noReload(e)
{
  if (!e) e=window.event;
  var kc=e.keyCode;
  // F5 and Ctrl-R
  if ((kc==116)||(kc==82 && e.ctrlKey))
  {
    if (e.preventDefault) e.preventDefault();
    e.keyCode=0;
    return false;
  }
}

//
// Event handle that monitors access keys and
// blocks keys that lead to a page reloading.
// used for our dialogs.
//
function _monitorNoReload(e)
{
  if (_agent.isIE)
    _monitor(e);
  return _noReload(e);
}


//
// Deliver a client event with the specified type, source and parameters
// to the handler body.
//
function _handleClientEvent(type, source, params, handlerBody)
{
  var event = new Object();
  event.type = type;
  event.source = source;
  event.params = params;
  var func = new Function("event", handlerBody);
  return func(event);
}


//
// APIs dealing with cookies.  We currently have no supported
// public functions.  _getCookie() and _setCookie() are good candidates.
//

function _getCookie(name)
{
  var dc = document.cookie;

  var value = "";
  var prefix = name + "=";
  if (dc)
  {
    // Look for the cookie name in in the middle.
    var begin = dc.indexOf("; " + prefix);

    if (begin < 0)
    {
      // Not there: look for it at the beginning
      begin = dc.indexOf(prefix);
      if (begin > 0)
        begin = -1;
    }
    else
      // Found it - now skip over the colon and space
      begin += 2;

    if (begin >= 0)
    {
      var end = dc.indexOf(";", begin);
      if (end < 0)
        end = dc.length;

      value = unescape(dc.substring(begin + name.length + 1, end));
    }
  }

  return value;
}

//
// Sets a cookie value.
// This function isn't especially general (yet) as it doesn't
// allow overriding the domain, path, or expiry.
//
function _setCookie(name, value)
{
  // Compute the domain to scope as widely as is legit
  // by scoping off.
  // =-=AEW "localhost" just doesn't work.  I don't know how to
  // deal with this...  It'll confuse developers, even though
  // it will never have any real impact.
  var domain = window.location.host;

  /* =-=AEW The current Oracle Cookie design guidelines require
     cookies to _not_ be scoped widely.  So don't!
  var periodIndex = domain.indexOf(".");
  if ((periodIndex >= 0) &&
      (domain.lastIndexOf(".") != periodIndex))
  {
    // But don't scope off anything that's entirely a number,
    // since then we're probably dealing with an IP address
    var startOfDomain = domain.substr(0, periodIndex);
    if (!(((startOfDomain * startOfDomain) == 0) ||
          ((startOfDomain / startOfDomain) == 1)))
      domain = domain.substr(periodIndex);
  }
  */

  var colonIndex = domain.indexOf(":");
  if (colonIndex >= 0)
    domain = domain.substr(0, colonIndex);

  // Expire 10 years after today
  var expires = new Date();
  expires.setFullYear(expires.getFullYear() + 10);

  // And here's the cookie:
  // (Reordering the parameters seemed to break some browsers)
  var curCookie = name + "=" + value +
      "; path=/;domain=" + domain + "; expires=" + expires.toGMTString();

  document.cookie = curCookie;
}


//
// Set a single value in the Trinidad cookie
//
function _setAdfCookie(index, value)
{
  var arry = _getAdfCookie();
  arry[index] = value;

  // Rebuild the encoded value
  var encodedValue = arry[0];
  for (var i = 1; i < arry.length; i++)
  {
    encodedValue = encodedValue + "^" + arry[i];
  }

  _setCookie("oracle.uix", encodedValue);
}


//
// Extract the decoded form of the Trinidad cookie
//
function _getAdfCookie()
{
  var encodedValue = _getCookie("oracle.uix");
  var arry;
  if (encodedValue)
    arry = encodedValue.split("^");
  else
    arry = new Array("0", "", "");

  return arry;
}


//
// Defaults the time zone
//
function _defaultTZ()
{
  var tz = _getAdfCookie()[2];

  // If the time zone has already been set, then bail:  however,
  // time zones that start with "GMT" are inherently unreliable,
  // because they won't track daylight savings.  For such time zones,
  // always replace the value.
  if (tz && (tz.indexOf("GMT") != 0))
  {
    return;
  }

  // Set the correct time zone.
  _setAdfCookie(2, _getTimeZoneID());
}


//
// Compute the time zone ID, of form GMT+-XX:YY
//
function _getTimeZoneID()
{
  // Get the time zone offset, then flip the sign, as this
  // is opposite in meaning to the time zone ID
  var tzOffset = -(new Date()).getTimezoneOffset();
  var newTZ;

  // Build up the name of the time zone
  if (tzOffset > 0)
    newTZ = "GMT+";
  else
  {
    newTZ = "GMT-";
    tzOffset = -tzOffset;
  }

  var minutes = "" + tzOffset % 60;
  if (minutes.length == 1)
    minutes = "0" + minutes;
  return (newTZ + (Math.floor(tzOffset / 60)) + ":" + minutes);
}


/**
 * _monitor: monitor the document's onkeydown presses.
 * Activate the uix button that has the access key that matches the
 * key that is pressed along with the ALT key (ALT+accessKey).
 * This is called for IE, not on Mac, only.
 *
 * Background: we want the accesskey to work on our Trinidad
 * buttons (which are links wrapped around an image)
 * like it does for a <button> html element, and that is
 * it activates the button rather than sets focus like it does for a link.
 * (This already works as expected in Mozilla)
 * This javascript code must know that the element is our uix button.
 * In ButtonRenderer, we render a bogus attribute for this purpose,
 * called "uixbtn".
 * We also register document.onkeydown=_monitor on the page.
 */
function _monitor(e)
{

  // when the user holds down only the accessKey, this triggers a mouse
  // down event over and over with a keyCode of 18. So...
  // we make sure not to call the _findAccessKey function if
  // the user is holding down just the access key.

  var windowEvent = window.event;

  if ((windowEvent.altKey == true) && (windowEvent.ctrlKey == false) &&
      (windowEvent.keyCode != null) && (windowEvent.keyCode != 18 )
      && (!windowEvent.repeat) )
  {
    var keyPressed = String.fromCharCode(window.event.keyCode);

    var accessKeyNode = _getNodeWithAccessKey(document, keyPressed);

    // if the element is a Trinidad Button, make it act like an accelerator

    if (accessKeyNode != null && (accessKeyNode.getAttribute("adfbtn") != null))
    {

      // now I need to find the node to click...

      // if label, get the "for" attribute value.
      // That is the id of the element to click
      if (accessKeyNode.htmlFor)
      {
          var id=accessKeyNode.htmlFor;
          accessKeyNode = (id != null)
                          ? window.document.getElementById(id)
                          : null;
      }

      if (accessKeyNode != null)
      {
        // IE acts like it sets the focus before it triggers the access key,
        // so we will do the same.

        accessKeyNode.focus();
        accessKeyNode.click();
      }

    } // end if (accessKeyNode != null)
  }
  return true;
}

/**
 * This function is passed a DOM Node object and a keyboard key
 * that was pressed at the same time as the ALT key.
 * return the node with the access key, otherwise null.
 */
function _getNodeWithAccessKey(node, keyPressed)
{
  var keyPressedUpper = keyPressed.toUpperCase();
  var keyPressedLower = keyPressed.toLowerCase();

  var accessKeyObject =
  {
    activeFound:false,
    firstAccessKeyNode:null,
    accessKeyNode:null
  }

  // start looking through the DOM Node tree from the beginning
  // for the access key the user pressed.
  // The node returned will be the first one
  // found starting from the current active element, otherwise we will use the
  // first one found in the DOM tree regardless of the current active element.

  accessKeyObject = _findAccessKey(document,
                                   accessKeyObject,
                                   keyPressedUpper,
                                   keyPressedLower);

  var accessKeyNode = accessKeyObject.accessKeyNode;
  var firstAccessKeyNode = accessKeyObject.firstAccessKeyNode;

  // if we didn't find accessKeyNode in the tree from the active node
  // to the end of the tree,
  // just use the first element we found with the correct access key.

  if ( (accessKeyNode == null) && (firstAccessKeyNode != null) )
  {
    accessKeyNode = firstAccessKeyNode;
  }
  return accessKeyNode;
}

/**
 * This function is passed a DOM Node object, a local accessKeyObject,
 * and the keyPressed in upper and lower cases.
 * This function checks to see if the node
 * represents an HTML tag; i.e., if the node is an Element object. It
 * recursively calls itself on each of the children of the node, testing
 * them in the same way. If you invoke this function by passing it the
 * Document object, it traverses the entire DOM tree.

 * Find the element with the access key.
 * first, store the first element with the access key encountered in
 * the accessKeyObject.firstAccessKeyNode.
 * Then, find the first element with the access key
 * that is found after the focus element,
 * since this is how it works on IE for <button></button>
 * return accessKeyObject if the access key node found, otherwise return null.
 */
function _findAccessKey(node, accessKeyObject, keyPressedUpper, keyPressedLower)
{
  // check all nodes of type ELEMENT_NODE
  if (node.nodeType == 1 /*Node.ELEMENT_NODE*/)
  {
    // check if the _keyPressed  is the access key
    if ( (node.accessKey == keyPressedUpper) ||
          (node.accessKey == keyPressedLower))
    {
      if (accessKeyObject.activeFound == true)
      {
        // Great! Found accessKey on an element after the focus, so return
        // the node. The recursion will unwind and pass out this node to
        // the original call.
        accessKeyObject.accessKeyNode = node;
        return accessKeyObject;
      }
      else if (accessKeyObject.firstAccessKeyNode == null)
      {
        // the focus hasn't been found yet, but store away this node
        // in the global variable, since it is the first one found
        accessKeyObject.firstAccessKeyNode = node;
      }
    }
    // If the focus is on an element with the access key, IE activates the NEXT
    // element, which is why I set the _activeFound flag after checking the
    // access key.
    if (node == document.activeElement)
    {
      accessKeyObject.activeFound = true;
    }
  } // end check if it is an element node

  // Now get all children of n
  // Loop through the children
  // Recurse on each one

  var children = node.childNodes;
  for(var i=0; i < children.length; i++)
  {
    var accessKeyObject =
    _findAccessKey( children[i],
                    accessKeyObject,
                    keyPressedUpper,
                    keyPressedLower);

     if (accessKeyObject.accessKeyNode != null)
     {
      return accessKeyObject; // stop looping.
     }
  }
  return accessKeyObject;
}


//
// Returns true if the value contains nothing but strings
//
function _isEmpty(value)
{
  var s = "" + value;
  var i = 0;
  while (i < s.length)
  {
    if (s.charAt(i) != ' ')
      return false;
    i++;
  }

  return true;
}

//
// Returns true if the current document reads left to right.
//
function _isLTR()
{
  return document.documentElement["dir"].toUpperCase() == "LTR";
}


//
// _isSubmittingElement : Is the given element likely to submit?
//
function _isSubmittingElement(element)
{
  var isSub = false;
  var eltype = element.nodeName.toUpperCase();
  // Assume any button click is wanted
  if (eltype == "BUTTON")
  {
    isSub = true;
  }
  else if (eltype == "IMG")
  {
    // If the click was on an image, check to see if the image
    // is inside a link element.
    var pnode = element.parentNode;
    var ptype = pnode.nodeName.toUpperCase();
    if (('A' == ptype) && (pnode.href))
    {
      // OK, it's a link, now check if the onClick goes to one of our
      // submit functions.
      var oc = "" + pnode["onclick"];
      if ((oc != (void 0)) && (oc != null))
      {
        isSub = ((oc.indexOf("submitForm") > 0)
                 || (oc.indexOf("_uixspu") > 0)
                 || (oc.indexOf("_adfspu") > 0)
                 || (oc.indexOf("_addRowSubmit") > 0));
      }
    }
  }
  return isSub;
}



// Get the keycode from an event
function _getKC(event)
{
  if (window.event)
    return window.event.keyCode;
  else if (event)
    return event.which;
  return -1;
}


// Returns true if a form has been submitted a "short" time before newDate
function _recentSubmit(newDate)
{
  if (_lastDateSubmitted)
  {
    var diff = newDate - _lastDateSubmitted;
    if ((diff >= 0) && (diff < 200))
      return true;
  }
  return false;
}

// Returns true if a form has been reset a "short" time before newDate
function _recentReset(newDate)
{
  if (_lastDateReset)
  {
    var diff = newDate - _lastDateReset;
    if ((diff >= 0) && (diff < 200))
      return true;
  }
  return false;
}


// For PPR Back issue:
// save the page's contents into a hidden, disabled field.
// This function gets called when the beforeunload event fires, which is
// an IE-only event.
function _savePageStateIE()
{
  // the PPR Back Button support only works for IE.
  if (!_agent.isIE)
    return;

  var theSpan = _getElementById(document, "_pprPageContent");
  if (theSpan == null)
    return;

  // we do not want to save off the entire page if
  // there was never a ppr update to the page.
  //
  // To implement this feature, I check if the _pprSaveLib has
  // a value in it. I know it will if I had a PPR event on the
  // page at any time. If it is null or blank, then I do not
  // need to save the page state.
  var saveLibrariesElement = _getElementById(document, "_pprSaveLib");

  if (!(saveLibrariesElement != null && saveLibrariesElement.value != ""))
  {
     // don't bother saving page since
     // no ppr page ever happened on this page.
     return;
  }

  var savePageField = _getElementById(document, "_pprSavePage");
  if (savePageField == null)
    return;

  savePageField.value = theSpan.outerHTML;

}



// For PPR Back issue:
// restore the page state when the user comes back to the page
// after navigating off of it.
// This includes copying the saved html, loading the saved libraries,
// and eval'ing the inline scripts (done from ScriptEval.js).
// This function is called from _checkLoad
function restorePartialPageState()
{
  // the PPR Back Button support only works for IE.
  if (!_agent.isIE)
    return;

  // get the saved page state from the hidden, disabled field
  // and write it to the page
  var savePageField   = _getElementById(document, "_pprSavePage");

  // if the field doesn't exist or if there was nothing saved in it, return.
  if (savePageField == null || savePageField.value == "")
    return;

  var theSpan = _getElementById(document, "_pprPageContent");

  if (theSpan == null)
    return;

  theSpan.outerHTML = savePageField.value;

  // support the form submit method if the _pprSaveFormAction field is
  // present
  var lastAction = _getElementById(document, "_pprSaveFormAction");
  // lastFormAction is only defined if we are submitting an event,
  // instead of running scripts
  if(lastAction==null)
  {
    // Note: The inline javascript is eval'd in ScriptEval.js, so that it
    // is guaranteed that the libraries are all loaded first.
    // set this variable so that the saved script will be loaded, as opposed
    // to the script that is in _pprScripts, which gets loaded with every
    // ppr response.
    _pprBackRestoreInlineScripts = true;

    // next, get the libraries out of the saved field, and parse each one,
    // and reload.
    var saveLibrariesElement = _getElementById(document, "_pprSaveLib");
    if (saveLibrariesElement != null && saveLibrariesElement.value != "")
    {
      var libraries = saveLibrariesElement.value.split(",");
      _loadScriptLibrariesIE(document, libraries);
    }
  }
  else
  {
    if(lastAction.value)
      document.forms[0].action = lastAction.value;
    submitForm(0,0,{'event':'stateSynch','source':'__unknown__'});
  }




}

// Sets the _navDirty flag for the given window.
//
// Inputs: win:  The window in which to set the _navDirty flag.
//               (defaults to the current window)
//         name: The name of the form element that is forcing this change.
//               We'll check the exclude list for this name, and only set
//               _navDirty if this element is NOT on the list. If name
//               is undefined or 0, we WILL set _navDirty.
//
// NOTE: !!!! all of Common.js has been converted to set the _navDirty
//       flag through this function. However, because this change could
//       be destabilizing, Uncommon.js still sets _navDirty directly. !!!!
function _setNavDirty(win, name)
{
  var theWin = win;
  if (theWin == (void 0) || !theWin)
  {
    theWin = window;
  }

  var excludes = theWin._initialFormExclude;

  // Only set the _navDirty if this element is NOT on the exclude list.
  // However, a null name means set no matter what (sent from BodyRenderer).
  if ((name == (void 0))
      || !name
      || !_isInExclude(excludes, name))
  {
    theWin._navDirty = true;
  }
}

function _radioSet_uixspu(f,v,e,s,pt,p,o)
{
  _radioSet_adfspu(f,v,e,s,o);
}

function _radioSet_adfspu(f,v,e,s,o)
{
  if (window._pprBlocking)
    return;

  // Once again we've got timing issues. When the user clicks on the
  // text of a radio button, we get an onClick for the enclosing span before we
  // get the onClick for the button itself. This wouldn't be so bad if the
  // selected state was already changed, but it isn't. So we have to send the
  // second click if there is one, otherwise we have to send the first (only)
  // click. There's already code in submitForm to check for multiple submits
  // within 200ms, so we just send the first click after waiting 200ms.
  //
  // Of course the obvious answer to this is to use onChange instead of
  // onClick, but IE doesn't deliver onChange events to the container

  if (_pendingRadioButton)
  {
    // This is the second onClick call. We want to run the call to adfspu.
    // Eventually submitform will be called, do a submit, and set the
    // _lastDateSubmitted.

    // clear the pending flag for next time
    _pendingRadioButton = false;
    // and call
    _adfspu(f,v,e,s,o);
  }
  else
  {
    // This is the first click.

    // Remember that we've got a pending click.
    _pendingRadioButton = true;

    // Now build up a string representation of the call,
    // and put it into a timeout.

    // clear the pending flag for next time - in case there was no second click.
    var spucall = "_pendingRadioButton=false;_adfspu(";
    // Form
    if ((f != (void 0)) && (f != null))
      spucall += "'" + f + "'";
    spucall += ",";
    // Validation
    if (v != (void 0))
      spucall += v;
    spucall += ",";
    // Event
    if ((e != (void 0)) && (e != null))
      spucall += "'" + e + "'";
    spucall += ",";
    // Source
    if ((s != (void 0)) && (s != null))
      spucall += "'" + s + "'";

    // RadioSet does not pass an object
    spucall += ");";
    window.setTimeout(spucall, 200);
  }
}

/** This function is called from _spinboxRepeat.
 * This function increments or decrements the value that is in the 
 * input field by the stepSize. If the max/min is reached, check circular.  
 * If circular is true, then circle the number around. 
 * we default circular for now, because we do not support it yet.
 * Else, stop at the max or min.
 */
function _stepSpinboxValue(id, increment, stepSize, min, max)
{
   var circular = false;
   var input = _getElementById(document, id);
   if (input)
   {
      var value = input.value;
      if (isNaN(value) || isNaN(stepSize) || isNaN(min) || isNaN(max))
      {
        alert("value, stepSize, min, and max must all be numbers. value: "+
               value+", stepSize: "+stepSize+", min: "+min+", max: "+max);
        return false; 
      }
      if (increment)
      {
        var incrementedValue = parseFloat(value) + parseFloat(stepSize);
        if (incrementedValue < max)
              input.value = incrementedValue;
        else if (circular)
              input.value = min;   
        else input.value = max;
      }
      else
      {
        var decrementedValue = parseFloat(value) - parseFloat(stepSize);
        
        if (decrementedValue > min)
          input.value = decrementedValue;
        else if (circular)
          input.value = max; 
        else input.value = min;
      }
      return true;
   }
   return false;
}

/* This function is called when the inputNumberSpinbox component's spinbox 
 * buttons are released (onmouseup).
 * This function stops the spinboxTimer. 
 * The spinboxTimer calls _stepSpinboxValue in one second increments.
 */
function _clearSpinbox()
{
  window.clearTimeout(_spinboxRepeat.timer);
  _spinboxRepeat.functionString = null;
}

/**
  * This function is called when the inputNumberSpinbox component's 
  * spinbox buttons are pressed. This is called onmousedown. 
  * It calls the _stepSpinboxValue function to increment or decrement
  * the input element's value. We call this repeatedly every second.
  * onmouseup the component calls _clearSpinbox which clears the timeout.
  */
function _spinboxRepeat(id, increment, stepSize, min, max)
{ 
  // increment/decrement
  var success = _stepSpinboxValue(id, increment, stepSize, min, max);
  // if not successful, then clear the timeout and return
  if (!success)
  {
    window.clearTimeout(_spinboxRepeat.timer);
  }
  else
  {
    if (_spinboxRepeat.functionString == null)
    {
      // setup the function to pass to the timeout.
      _spinboxRepeat.functionString = 
          "_spinboxRepeat('"+id+"',"+increment+
          ","+stepSize+","+min+","+max+");";
    }
    _spinboxRepeat.timer =
      window.setTimeout(_spinboxRepeat.functionString, 1000);
  }

}

/** This function is called from _spinboxRepeat.
 * This function increments or decrements the value that is in the 
 * input field by the stepSize. If the max/min is reached, check circular.  
 * If circular is true, then circle the number around. 
 * we default circular for now, because we do not support it yet.
 * Else, stop at the max or min.
 */
function _stepSpinboxValue(id, increment, stepSize, min, max)
{
   var circular = false;
   var input = _getElementById(document, id);
   if (input)
   {
      var value = input.value;
      if (isNaN(value) || isNaN(stepSize) || isNaN(min) || isNaN(max))
      {
        alert("value, stepSize, min, and max must all be numbers. value: "+
               value+", stepSize: "+stepSize+", min: "+min+", max: "+max);
        return false; 
      }
      if (increment)
      {
        var incrementedValue = parseFloat(value) + parseFloat(stepSize);
        if (incrementedValue < max)
              input.value = incrementedValue;
        else if (circular)
              input.value = min;   
        else input.value = max;
      }
      else
      {
        var decrementedValue = parseFloat(value) - parseFloat(stepSize);
        
        if (decrementedValue > min)
          input.value = decrementedValue;
        else if (circular)
          input.value = max; 
        else input.value = min;
      }
      return true;
   }
   return false;
}

/* This function is called when the inputNumberSpinbox component's spinbox 
 * buttons are released (onmouseup).
 * This function stops the spinboxTimer. 
 * The spinboxTimer calls _stepSpinboxValue in one second increments.
 */
function _clearSpinbox()
{
  window.clearTimeout(_spinboxRepeat.timer);
  _spinboxRepeat.functionString = null;
}

/**
  * This function is called when the inputNumberSpinbox component's 
  * spinbox buttons are pressed. This is called onmousedown. 
  * It calls the _stepSpinboxValue function to increment or decrement
  * the input element's value. We call this repeatedly every second.
  * onmouseup the component calls _clearSpinbox which clears the timeout.
  */
function _spinboxRepeat(id, increment, stepSize, min, max)
{ 
  // increment/decrement
  var success = _stepSpinboxValue(id, increment, stepSize, min, max);
  // if not successful, then clear the timeout and return
  if (!success)
  {
    window.clearTimeout(_spinboxRepeat.timer);
  }
  else
  {
    if (_spinboxRepeat.functionString == null)
    {
      // setup the function to pass to the timeout.
      _spinboxRepeat.functionString = 
          "_spinboxRepeat('"+id+"',"+increment+
          ","+stepSize+","+min+","+max+");";
    }
    _spinboxRepeat.timer =
      window.setTimeout(_spinboxRepeat.functionString, 1000);
  }

}
//PH:This method returns the 'event' object
function _getEventObj()
{
  if(typeof(event) == 'undefined')
    return window.event;
  else     
    return event;
  
  return null;
}

//***********************************************************
// "Object Oriented" js below
//***********************************************************
/**
 * User interfaces utility methods
 */
var TrUIUtils = new Object();

/**
 * Remove leading and trailing whitespace
 */
TrUIUtils.trim = function(
data)
{
  if (data != null && (typeof data) == 'string')
    return data.replace(TrUIUtils._TRIM_ALL_RE, '');

  return data;
}

// regular expression to gather whitespace at beginning and end of line
TrUIUtils._TRIM_ALL_RE = /^\s*|\s*$/g;
