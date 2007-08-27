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
function TrPopupDialog()
{
  var page = TrPage.getInstance();

  var div = document.createElement("div");
  div.style.cssText = "visibility:hidden; position: absolute;";
  div.className = page.getStyleClass("af|dialog::container");
  
  //setup the title bar
  var titlebar = document.createElement("div");
  titlebar.className = page.getStyleClass("af|dialog::title-bar");
  div.appendChild(titlebar);

  //setup the title bar
  var titleText = document.createElement("div");
  titleText.style.cssText = "float:left;";
  titleText.className = page.getStyleClass("af|dialog::title-text");
  titlebar.appendChild(titleText);

  //setup the title bar
  var closerDiv = document.createElement("div");
  closerDiv.style.cssText = "float:right;";
  closerDiv.className = page.getStyleClass("af|dialog::close-icon");
  _addEvent(closerDiv, "click", TrPopupDialog._returnFromDialog);
  titlebar.appendChild(closerDiv);

  //setup the title bar
  var sepDiv = document.createElement("div");
  sepDiv.style.cssText = "clear:both;";
  titlebar.appendChild(sepDiv);

  //setup the content iframe
  var iframe = document.createElement("iframe");
  iframe.name = "_blank";
  iframe.frameBorder = "0";
  iframe.className = page.getStyleClass("af|dialog::content");
  div.appendChild(iframe);
  
  // Hold the iframe so we can set the 'src' as needed, and also the height
  this._iframe = iframe;

  // Hold on to the outer div so we can set the width
  this._outerDiv = div;

  // Hold on to the text div so we can update the title
  this._titleTextDiv = titleText;
    
  document.body.appendChild(div);
  
  TrPanelPopup.call(this);
  this.setModal(true);
  this.setCentered(true);
  this.setContent(div);

  // flag to indicate if dialog size should be locked
  this._fixedSize = false;

}

// TrPopupDialog inherits from TrPanelPopup
TrPopupDialog.prototype = new TrPanelPopup();

/**
 * Set the title bar text
 **/
TrPopupDialog.prototype.setTitle = function(title)
{
  if (title)
  {
    this._titleTextDiv.innerHTML = title;
  }
  else
  {
    this._titleTextDiv.innerHTML = "";
  }
}

TrPopupDialog.prototype.setDestination = function(url)
{
  this._iframe.src = url;
}

TrPopupDialog.prototype.setSize = function(width, height)
{
  if (width && height)
  {
    this._resizeIFrame(width, height);
    this._fixedSize = true;
  }
}

TrPopupDialog.getInstance = function()
{
  return TrPopupDialog.DIALOG;
}

/**
 * Resize the content area of the dialog
 **/
TrPopupDialog.prototype._resizeIFrame = function(width, height)
{
  // Set the height of the iframe
  this._iframe.height = height + "px";
  // Set the width of the iframe to 100% so it is sized by it's parent outerDiv
  this._iframe.width = "100%";
  // But set the width of the outerDiv, so the title bar is also the right size
  this._outerDiv.style.width = width + "px";
  this._calcPosition(false);
}

/**
 * Called from dialog page (usually body onload) to set the dialog title to
 * that of the current page, and handle any resizing that may be required.
 **/
TrPopupDialog._initDialogPage = function()
{
  var dialog;

  try
  {
    dialog = parent.TrPopupDialog.DIALOG;

  }
  catch(err)
  {
  }

  if (!dialog)
    return;

  dialog.setTitle(document.title);
    
  // Resize the dialog to the page content
  if (!dialog._fixedSize)
  {
    if (_agent.isIE)
    {
      dialog._resizeIFrame(
        dialog._iframe.Document.body.scrollWidth+40, 
        dialog._iframe.Document.body.scrollHeight+40);
    }
    else
    {
      dialog._resizeIFrame(
        dialog._iframe.contentDocument.body.offsetWidth+40, 
        dialog._iframe.contentDocument.body.offsetHeight+40);
    }
  }
  dialog.show();
}

/*
 * This function handles the closure of the dialog.  Generally, it is
 * called via PPR.
 */
TrPopupDialog._returnFromDialog = function()
{
  var dialog = TrPopupDialog.DIALOG;
  if (dialog)
  {
    dialog.hide();
  }
  else
  {
    alert("returnFromDialog(): Error - Current popup is not a dialog");
  }

  // Clear the dialog instance
  TrPopupDialog.DIALOG = undefined;
}

/*
 * Callback function, invoked on close of dialog.  If necessary, this
 * function will make a submit to cause the return event to fire.
 * TODO - Move this function to another library - TrPage perhaps?
 */
TrPopupDialog._returnFromDialogAndSubmit = function(callbackProps, value)
{
  if (callbackProps)
  {
    var formName = callbackProps['formName'];
    var postbackId = callbackProps['postback'];

    _submitPartialChange(formName, 0, {rtrn:postbackId});
  }
}

TrPopupDialog._launchDialog = function(
  srcURL,
  dialogProps,
  callbackFunction,
  callbackProps)
{
  var dialog = TrPopupDialog.DIALOG;
  if (!dialog)
  {
    dialog = TrPopupDialog.DIALOG = new TrPopupDialog();
  }

  // Register callback on close of dialog
  dialog.callback = callbackFunction;
  dialog.callbackProps = callbackProps;

  // Dialog will auto-size to fit content unless specified
  if (dialogProps && dialogProps['width'] && dialogProps['height'])
    dialog.setSize(dialogProps['width'], dialogProps['height']);

  // Dialog will be opened by _initDialogPage() once dialog page has loaded
  // to prevent lots of up/down sizing when auto-sized.
  dialog.setDestination(srcURL);
}
