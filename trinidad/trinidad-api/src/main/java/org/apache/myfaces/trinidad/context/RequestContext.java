/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidad.context;

import java.awt.Color;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;

/**
 * Context class for all per-request and per-webapp information
 * required by Trinidad.  A <code>RequestContext</code> object can be
 * retrieved with the static {@link #getCurrentInstance} method.
 * There is one and only one <code>RequestContext</code> object
 * active in any one thread.
 * <p>
 * This class does not extend <code>FacesContext</code>;  this is intentional,
 * as extending <code>FacesContext</code> requires taking over the
 * <code>FacesContextFactory</code>.
 * <p>
 *
 * @todo Refactor this class after everything gets added to it.
 * @todo There's some values in here that seem to affect only output (e.g.,
 *  right-to-left); that's not great, since ideally that detail would be
 *  buried in something more renderer-specific.
 * @author The Oracle ADF Faces Team
 * @mock
 */
abstract public class RequestContext
{
  /**
   * Name of the EL implicit variable ("requestContext") that is used to
   * expose this context object.
   */
  static public final String VARIABLE_NAME =
    "requestContext";

  // Omitted APIs:
  //
  // LocaleContext
  // =============
  // Locale getTranslationLocale
  //
  // DateFormatContext:
  // =================
  // int getTwoDigitYearStart
  // boolean isLenient (very lame API, definitely gone)


  /**
   * Retrieves the RequestContext active for the current thread.
   */
  static public RequestContext getCurrentInstance()
  {
    return _CURRENT_CONTEXT.get();
  }

  

  /**
   * Creates an RequestContext.  RequestContext is abstract
   * and may not be instantiated directly.
   * @see RequestContextFactory
   */
  protected RequestContext()
  {
  }

  //
  // State related APIs
  //

  /**
   * Returns a Map of objects at "pageFlow" scope.
   */
  public abstract Map<String, Object> getPageFlowScope();

  /**
   * @deprecated use getPageFlowScope()
   */
  @Deprecated
  final public Map<String, Object> getProcessScope()
  {
    return getPageFlowScope();
  }


  //
  // Dialog APIs
  //

  /**
   * Returns from a dialog raised by a
   * {@link org.apache.myfaces.trinidad.component.UIXCommand UIXCommand} component,
   * or any component implementing
   * {@link org.apache.myfaces.trinidad.component.DialogSource DialogSource},
   * or any direct calls to {@link #launchDialog launchDialog()}.
   * <p>
   * @see org.apache.myfaces.trinidad.event.ReturnEvent
   * @param returnValue the value to be delivered in the the ReturnEvent
   * @todo Do I need an explicit "cancelled" concept, or
   * is a null returnValue good enough?
   */
  public abstract void returnFromDialog(
    Object returnValue,
    Map<Object, Object> returnParameters);


  /**
   * Returns an DialogService, which exposes a number
   * of APIs needed by component and framework developers.  This
   * will only rarely be needed by page authors.
   */
  public abstract DialogService getDialogService();

  /**
   * Launch a dialog, optionally raising it in a new dialog window.
   * <p>
   * The dialog will receive a new <code>pageFlowScope</code> map,
   * which includes all the values of the currently available pageFlowScope
   * as well as a set of properties passed to this function in
   * the <code>dialogParameters</code> map.  Changes to this newly
   * created scope will not be visible once the dialog returns.
   * <p>
   * @param dialogRoot the UIViewRoot for the page being launched
   * @param dialogParameters a set of parameters to populate the
   *   newly created pageFlowScope
   * @param source the UIComponent that launched the dialog and
   *   should receive the {@link org.apache.myfaces.trinidad.event.ReturnEvent}
   *   when the dialog is complete.
   * @param useWindow if true, use a popup window for the dialog
   *    if available on the current user agent device
   * @param windowProperties the set of UI parameters used to
   *   modify the window, if one is used.  The set of properties that\
   *   are supported will depend on the <code>RenderKit</code>, but
   *   common examples include "width", "height", "top" and "left".
   */
  public abstract void launchDialog(
    UIViewRoot  dialogRoot,
    Map<String, Object> dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map<String, Object> windowProperties);

  //
  // General ADF Faces
  //

  /**
   * Returns true if JSF is currently processing a postback request.
   * <code>isPostback()</code> will return false if this is a request
   * for an initial render of a page (that is, if Apply Request Values
   * never executes), or if during the request the user is navigated
   * to a different page (because of a navigation rule, etc).  For
   * example, during a request that results in a navigation to a new
   * page, <code>isPostback()</code> will return true from Apply
   * Request Values to Invoke Application, then false afterwards;
   * whereas if there was no navigation, it would return true
   * <p>
   * The value of this method is undefined during (or before)
   * the Restore View phase, but can be used in the afterPhase()
   * method of a PhaseListener for Restore View.
   * </p>
   */
  public abstract boolean isPostback();
  
  /**
   * Method to indicate if this current HTTP request is a
   * partial page rendering request.
   * 
   * @param context the <code>FacesContext</code> object for
   * the request we are processing
   * @return is this request a PPR request?
   */
  public abstract boolean isPartialRequest(FacesContext context);
  /**
   * Returns true if output should contain debugging information.
   */
  public abstract boolean isDebugOutput();

  /**
   * Returns true if client-side validation should be disabled.
   */
  public abstract boolean isClientValidationDisabled();

  /**
   * Returns the "output mode" - printable, etc.
   */
  public abstract String getOutputMode();


  /**
   * Returns the name of the preferred skin family.
   */
  public abstract String getSkinFamily();

  /**
   * Returns the name of the current accessibility mode.
   * @todo Use AccessibilityMode object?
   */
  public abstract String getAccessibilityMode();

  //
  //  General localization
  //

  /**
   * Returns true if the user should be shown output in right-to-left.
   * @todo Should this be a java.lang.Boolean to better encapsulate
   *  "default off of Locale"?  Currently, this forces Locale-checking
   *  code into this method.
   */
  public abstract boolean isRightToLeft();

  //
  //  Number formatting
  //

  /**
   * Return the separator used for groups of numbers.  If NUL (zero),
   * use the default separator for the current language.
   */
  public abstract char getNumberGroupingSeparator();

  /**
   * Return the separator used as the decimal point.  If NUL (zero),
   * use the default separator for the current language.
   */
  public abstract char getDecimalSeparator();

  /**
   * Return the ISO 4217 currency code used by default for formatting
   * currency fields when those fields do not specify an explicit
   * currency field via their converter.  If this returns null, the default
   * code for the current locale will be used.
   *
   * @todo Investigate if we need to provide getCurrencySymbol() as well.
   */
  public abstract String getCurrencyCode();

  //
  // DateFormating API
  //
  /**
   * Returns the year offset for parsing years with only two digits.
   * If not set this is defaulted to <code>1950</code>
   * This is used by @link{org.apache.myfaces.trinidad.faces.view.converter.DateTimeConverter}
   * while converting strings to Date object.
   */
  public abstract int getTwoDigitYearStart();

  //
  // Help APIs
  //

  /**
   * Return the URL to an Oracle Help for the Web servlet.
   * @todo Do we need to support non-OHW help systems as first-class
   *  help providers?
   */
  public abstract String getOracleHelpServletUrl();

  /**
   * Returns a Map that will accept topic names as keys, and return
   * an URL as a result.
   */
  public abstract Map<String, Object> getHelpTopic();

  /**
   * Returns a Map that will accept help system properties as keys, and return
   * an URL as a result.
   */
  public abstract Map<String, Object> getHelpSystem();

  //
  // Date formatting
  //

  /**
   * Returns the default TimeZone used for interpreting and formatting
   * date values.
   */
  public abstract TimeZone getTimeZone();

  /**
   * Gets the ChangeManager for the current application.
   */
  public abstract ChangeManager getChangeManager();

  /**
    * Gets the PageFlowScopeProvider for the current application.
    */
  public abstract PageFlowScopeProvider getPageFlowScopeProvider();

  /**
    * Gets the PageResolver for the current application.
    */
  public abstract PageResolver getPageResolver();

  /**
   * Gets the RegionManager for the current application.
   */
  public abstract RegionManager getRegionManager();

  //
  // Partial Page Rendering support
  //
  /**
   * Add a component as a partial target. In response to a partial event, only
   * components registered as partial targets are re-rendered.
   */
  public abstract void addPartialTarget(UIComponent newTarget);

  /**
   * Adds a listener on a set of particular triggering components. If one of
   * the named components gets updated in response to a partial event, then
   * this listener component will be rerendered during the render phase (i.e.
   * it will be added as a partialTarget). The list should consist of names
   * suitable for use with the findComponent method on UIComponent.
   */
  public abstract void addPartialTriggerListeners(UIComponent listener,
                                                  String[] trigger);

  /**
   * Called when any component gets updated. Any partial target components
   * listening on this component will be added to the partialTargets list in
   * the render phase.
   */
  public abstract void partialUpdateNotify(UIComponent updated);

  //
  // Miscellaneous functionality
  //

  public abstract UploadedFileProcessor getUploadedFileProcessor();

  /**
   * Returns a Map that takes color palette names as keys, and returns
   * the color palette as a result.
   */
  public abstract Map<String, List<Color>> getColorPalette();

  /**
   * Returns a Map that performs message formatting with a recursive Map
   * structure.  The first key must be the message formatting mask, and the
   * second the first parameter into the message. (The formatter Map supports
   * only a single parameter at this time.)
   */
  public abstract Map<Object, Map<Object,String>> getFormatter();

  /**
   * Returns the Agent information for the current context
   */
  public abstract Agent getAgent();

  /**
   * Releases the RequestContext object.  This method must only
   * be called by the code that created the RequestContext.
   * @exception IllegalStateException if no RequestContext is attached
   * to the thread, or the attached context is not this object
   */
  public void release()
  {
    Object o = _CURRENT_CONTEXT.get();
    if (o == null)
      throw new IllegalStateException("RequestContext was already released or " +
                                      "had never been attached.");
    if (o != this)
      throw new IllegalStateException("Trying to release a different " +
                     "RequestContext than the current context.");
    
    _CURRENT_CONTEXT.remove();
  }

  /**
   * Attaches a RequestContext to the current thread.  This method 
   * should only be called by a RequestContext object itself.
   * @exception IllegalStateException if an RequestContext is already
   * attached to the thread
   */
  public void attach()
  {
    Object o = _CURRENT_CONTEXT.get();
    if (o != null)
      throw new IllegalStateException("Trying to attach RequestContext to a " +
                                      "thread that already had one.");
    _CURRENT_CONTEXT.set(this);
  }

  static private final ThreadLocal<RequestContext> _CURRENT_CONTEXT = 
    new ThreadLocal<RequestContext>();
  
}
