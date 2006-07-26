/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.convert.DateTimeConverter;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectInputDate;
import org.apache.myfaces.trinidad.context.AdfFacesContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.validator.DateTimeRangeValidator;

import org.apache.myfaces.trinidadinternal.agent.AdfFacesAgent;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;
import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRendererUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.pages.GenericEntry;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.ConfigurationScriptlet;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;


/**
 */
public class SimpleSelectInputDateRenderer
  extends SimpleSelectInputTextRenderer
{
  public SimpleSelectInputDateRenderer()
  {
    super(CoreSelectInputDate.TYPE);
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _chooseIdKey = type.findKey("chooseId");
  }

  protected void queueActionEvent(FacesContext context, UIComponent component)
  {
    FacesBean bean = getFacesBean(component);
    // If there's a non-default action, then just launch away
    if (getAction(bean) != null)
    {
      super.queueActionEvent(context, component);
    }
    // Otherwise, we'll fall back to launching the default dialog
    // (This should only happen on devices without support for
    // custom windows - everything else would have just launched
    // a calendar window with the _ldp JS function)
    else
    {
      Object submittedValue = (String) getSubmittedValue(bean);
      Date date = null;
      try
      {
        Object converted = getConvertedValue(context,
                                             component,
                                             submittedValue);
        if (converted instanceof Date)
          date = (Date) converted;
        else
        {
          GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
          if (fac.isConvertible(converted, Date.class))
            date = (Date) fac.convert(converted, Date.class);
        }
      }
      // Not a big deal;  just means that an invalid value was entered,
      // so we'll launch the dialog showing nothing
      catch (ConverterException ce)
      {
        _LOG.fine(ce);
      }


      AdfFacesContext afContext = AdfFacesContext.getCurrentInstance();
      DateTimeRangeValidator dtrv = _findDateTimeRangeValidator(bean);

      if (date == null)
        date = new Date();

      Map parameters = new HashMap();
      parameters.put(XhtmlConstants.VALUE_PARAM, _getDateAsString(date));
      parameters.put(XhtmlConstants.MIN_VALUE_PARAM,
                     dtrv == null
                     ? null :  _getDateAsString(dtrv.getMinimum()));
      parameters.put(XhtmlConstants.MAX_VALUE_PARAM,
                     dtrv == null
                     ? null :  _getDateAsString(dtrv.getMaximum()));
      parameters.put(GenericEntry.getEntryKeyParam(),
                     GenericEntry.CALENDAR_DIALOG_ENTRY);

      afContext.launchDialog(GenericEntry.getGenericEntryViewRoot(context),
                             parameters,
                             component,
                             true,
                             null);
    }
  }


  /**
   * Give subclasses a chance to override the ReturnEvent.
   */
  protected void queueReturnEvent(
    FacesContext context,
    UIComponent  component,
    ReturnEvent  event)
  {
    Object returnValue = event.getReturnValue();
    GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();

    // If we got passed a Date object, send it back to String
    // land (where it needs to be for submitted values).
    if ((returnValue instanceof Date) || fac.isConvertible(returnValue, Date.class))
    {
      FacesBean bean = getFacesBean(component);
      Converter converter = getConverter(bean);
      if (converter == null)
        converter = getDefaultConverter(context, bean);

      if (converter != null)
      {
        returnValue = converter.getAsString(context,
                                            component,
                                            returnValue);
      }
      else
      {
        returnValue = returnValue.toString();
      }

      event = new ReturnEvent(component,
                              returnValue,
                              event.getReturnParameters());
    }

    event.queue();
  }


  protected void encodeAllAsElement(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    String chooseId = _computeChooseId(context, component, bean);
    arc.getProperties().put(_CACHED_CHOOSE_ID, chooseId);

    // Add the scriptlets required by the date field
    // =-=AEW What's this one?
    XhtmlUtils.addLib(context, arc, "_dfsv()");
    XhtmlUtils.addLib(context, arc, "_fixDFF()");
    super.encodeAllAsElement(context, arc, component, bean);

    if (!getDisabled(bean))
    {
      _checkIfActive(context, arc, component, _getChooseId(arc));
    }
  }

  protected void renderIcon(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // Add the two scriptlets required by the icon
    XhtmlUtils.addLib(context,
                      arc,
                      ConfigurationScriptlet.sharedInstance().getScriptletKey());
    XhtmlUtils.addLib(context,
                      arc,
                      _DATE_TIME_ZONE_OFFSET_KEY);

    super.renderIcon(context, arc, component, bean);
  }

  /**
   * @todo - should the default style be "short" or "default", which
   * may map to "medium"
   * @todo medium / default is  what it is defaulted to in faces
   */
  protected Converter getDefaultConverter(
    FacesContext context,
    FacesBean    bean)
  {
    Converter converter = context.getApplication().
                             createConverter(DateTimeConverter.CONVERTER_ID);

    // for convenience, we will set the time zone of the converter to that
    // specified by the context or, if that is not present, to the time zone
    // of the server
    if(converter instanceof DateTimeConverter)
    {
        DateTimeConverter dtc = (DateTimeConverter) converter;

        boolean adfDTC = _isAdfDateTimeConverter(converter);

        if (!adfDTC)
        {
            // if it is not the ADF DateTimeConverter, set the date style to
            // short
            dtc.setDateStyle("short");
        }

        // if it is not the ADF DateTimeConverter or (it is AND
        // no time zone is set) then we want to set the
        // time zone to the one in the faces context or use
        // the default server time zone on the converter
        if(!adfDTC || dtc.getTimeZone() == null)
        {
            TimeZone tz = null;

            AdfFacesContext adfFacesContext = AdfFacesContext.getCurrentInstance();
            tz = adfFacesContext.getTimeZone();
            if(tz == null)
            {
                tz = TimeZone.getDefault();
            }

            dtc.setTimeZone(tz);
        }
    }

    return converter;
  }

  protected String getOnblur(FacesBean bean)
  {
    String onblur = super.getOnblur(bean);
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    String chooseId = _getChooseId(arc);

    int length = _BLUR_PREFIX.length() + 4;
    if (chooseId != null)
      length += chooseId.length();

    StringBuffer buffer = new StringBuffer(length);
    buffer.append(_BLUR_PREFIX);

    if (chooseId != null)
    {
      buffer.append(",'");
      buffer.append(chooseId);
      buffer.append("'");
    }

    buffer.append(")");

    return XhtmlUtils.getChainedJS(buffer.toString(), onblur, false);
  }

  protected String getOnfocus(FacesBean bean)
  {
    String onfocus = super.getOnfocus(bean);
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    String chooseId = _getChooseId(arc);

    // The special _dff handler is only needed for date fields
    // connected to a chooser;  the blur handler is needed all the time.
    if (chooseId != null)
    {
      int length = _FOCUS_PREFIX.length() + 4;
      if (chooseId != null)
        length += chooseId.length();

      StringBuffer buffer = new StringBuffer(length);
      buffer.append(_FOCUS_PREFIX);

      if (chooseId != null)
      {
        buffer.append(",'");
        buffer.append(chooseId);
        buffer.append("'");
      }

      buffer.append(")");

      return XhtmlUtils.getChainedJS(buffer.toString(), onfocus, false);
    }
    else
    {
      return onfocus;
    }
  }


  protected String getLaunchOnclick(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // If the field has an action, use the default behavior.  Or,
    // if the field doesn't support launching a window at all,
    // use the default behavior.
    if ((getAction(bean) != null) ||
        !Boolean.TRUE.equals(
            arc.getAgent().getCapability(AdfFacesAgent.CAP_MULTIPLE_WINDOWS)))
      return super.getLaunchOnclick(context, arc, component, bean);

    String id = arc.getCurrentClientId();
    if ((id == null) || (arc.getFormData() == null))
      return null;

    // we want something big enough
    StringBuffer onClickBuffer = new StringBuffer(100);

    onClickBuffer.append("_ldp('");
    onClickBuffer.append(arc.getFormData().getName());
    onClickBuffer.append("','");

    onClickBuffer.append(id);
    onClickBuffer.append('\'');

    DateTimeRangeValidator dtrv = _findDateTimeRangeValidator(bean);
    if (dtrv != null)
    {
      String minTime = _getDateAsString(dtrv.getMinimum());
      String maxTime = _getDateAsString(dtrv.getMaximum());


      if ((minTime != null) || (maxTime != null))
      {
        onClickBuffer.append(',');
        if (minTime != null)
        {
          onClickBuffer.append(minTime);
        }
        else
        {
          // placeholder for next parameters
          onClickBuffer.append("(void 0)");
        }
      }

      if (maxTime != null)
      {
        onClickBuffer.append(',');
        onClickBuffer.append(maxTime);
      }
    }

    onClickBuffer.append("); return false");

    return onClickBuffer.toString();
  }


  protected Integer getDefaultColumns(AdfRenderingContext arc, FacesBean bean)
  {
    Converter converter = getConverter(bean);

    // Ignoring the "default" converter code is intentional;  we'll just
    // fall through to _DEFAULT_COLUMNS here to save time
    if (converter instanceof
        org.apache.myfaces.trinidadinternal.convert.DateTimeConverter)
    {
      int columns = ((org.apache.myfaces.trinidadinternal.convert.DateTimeConverter)
              converter).getColumns();
      return IntegerUtils.getInteger(columns);
    }

    return _DEFAULT_COLUMNS;
  }


  protected String getButtonIconName()
  {
    return XhtmlConstants.AF_SELECT_INPUT_DATE_LAUNCH_ICON_NAME;
  }

  protected String getChooseId(FacesBean bean)
  {
    return toString(bean.getProperty(_chooseIdKey));
  }

  protected String getSearchDesc(FacesBean bean)
  {
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    if (isInaccessibleMode(arc))
      return null;

    return arc.getTranslatedString(_LAUNCH_PICKER_TIP_KEY);
  }


  protected String getRootStyleClass(FacesBean bean)
  {
    return "af|selectInputDate";
  }

  protected String getContentStyleClass(FacesBean bean)
  {
    return "af|selectInputDate::content";
  }

  private String _getChooseId(AdfRenderingContext arc)
  {
    return (String) arc.getProperties().get(_CACHED_CHOOSE_ID);
  }

  private String _computeChooseId(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return CoreRendererUtils.getRelativeId(context,
                                           component,
                                           getChooseId(bean));

  }

  // Checks to see whether the current dateField should
  // be active, and if so, renders a script that will activate
  // it.
  private void _checkIfActive(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    String              chooseId
    ) throws IOException
  {
    if (chooseId == null)
      return;

    String id = getClientId(context, component);


    Map activeDateFields = (Map)
      arc.getProperties().get(_ACTIVE_DATE_FIELDS_KEY);

    if (activeDateFields == null)
    {
      activeDateFields = new HashMap();
      arc.getProperties().put(_ACTIVE_DATE_FIELDS_KEY, activeDateFields);
    }

    // The first dateField that is rendered for each inlineDatePicker
    // is the "active" dateField.  Check to see if we already
    // have an active dateField for this inlineDatePicker.
    if (activeDateFields.get(chooseId) == null)
    {
      // We don't already have an active dateField, so
      // this one is it.  Render the script to activate
      // the dateField.
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", component);
      renderScriptTypeAttribute(context, arc);
      renderScriptDeferAttribute(context, arc);

      writer.writeText("_dfa('", null);
      writer.writeText(id, null);
      writer.writeText("','", null);
      writer.writeText(chooseId, null);
      writer.writeText("');", null);
      writer.endElement("script");

      // Mark the inlineDatePicker as having an active dateField
      activeDateFields.put(chooseId, Boolean.TRUE);
    }
  }

  //
  // Find a DateTimeRangeValidator for use in setting up a
  // minimum and maximum value
  //
  private DateTimeRangeValidator _findDateTimeRangeValidator(FacesBean bean)
  {
    Iterator validators = getValidators(bean);
    while (validators.hasNext())
    {
      Object validator = validators.next();
      if (validator instanceof DateTimeRangeValidator)
        return (DateTimeRangeValidator) validator;
    }

    return null;
  }


  private static boolean _isAdfDateTimeConverter(Converter converter)
  {
    return (converter instanceof
            org.apache.myfaces.trinidad.convert.DateTimeConverter);
  }


  /**
   * Stringify the date into canonical form;  we currently
   * use the long integer date.getTime().
   */
  private static String _getDateAsString(Date date)
  {
    if (date == null)
      return null;

    return String.valueOf(_adjustTimeZone(date));
  }

  /**
   * Adjust the specified date, which is in server timeZone to the timeZone
   * found in AdfFacesContext and return the new date long value.
   */
  private static long _adjustTimeZone(Date date)
  {
    // get the current date of the server
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    long dateValueInMs = calendar.getTimeInMillis();

    // adjust the date; first, get this into GMT
    long tzOffset = calendar.get(Calendar.ZONE_OFFSET) +
                   calendar.get(Calendar.DST_OFFSET);
    // get the timeZone specified in adf-faces-config, if any or the
    // client timeZone and find out the difference in timeZone
    TimeZone timeZone = AdfFacesContext.getCurrentInstance().getTimeZone();
    if(timeZone == null)
    {
        timeZone = TimeZone.getDefault();
    }

    // then, adjust for the "local" time zone (either the client's, as
    // specified in AdfFacesContext, or the server's if it wasn't specified
    // in AdfFacesContext)
    tzOffset -= timeZone.getOffset(dateValueInMs);

    // make sure that adjusting to correct timeZone doesn't take the
    // long value out of the range. Calendar too doesn't handle this
    // properly ie. MIN_VALUE < (longValue + tzOffset) < MAX_VALUE.
    if (tzOffset < 0)
    {
     tzOffset = (long)Math.max((float)tzOffset,
                               (float)Long.MIN_VALUE - (float)dateValueInMs);
    }
    else
    {
     tzOffset = (long)Math.min((float)tzOffset,
                               (float)Long.MAX_VALUE - (float)dateValueInMs);
    }

    // adjust the date in ms to the adjusted time zone.
    long adjusted = dateValueInMs + tzOffset;
    return adjusted;
  }

  // this scriptlet is to pass in the time zone raw offset from the locale
  // context in javascript. It will be used in the date picker when
  // we format the date field with time values.
  private static class DateTimeZoneOffsetScriptlet extends Scriptlet
  {

    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    private DateTimeZoneOffsetScriptlet()
    {
    }

    public Object getScriptletKey()
    {
      return _DATE_TIME_ZONE_OFFSET_KEY;
    }

    protected void outputScriptletContent(
      FacesContext context,
      AdfRenderingContext arc)
      throws IOException
    {
      // get the tzOffset for the current date. I will compare this with
      // the tzOffset for the current data in javascript, and use it to
      // manipulate the formatted datefield's value so that it shows the

      // localeContext's timeZone, and not the time zone on the browser.
      TimeZone tz = arc.getLocaleContext().getTimeZone();
      int tzOffsetMinutes = tz.getOffset(System.currentTimeMillis())/(1000*60);
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("var _uixLocaleTZ=", null);
      writer.writeText(String.valueOf(tzOffsetMinutes), null);
      writer.writeText(";", null);
    }

    private static final Scriptlet _sInstance =
      new DateTimeZoneOffsetScriptlet();

  }

  private PropertyKey _chooseIdKey;

  private static final Integer _DEFAULT_COLUMNS = new Integer(10);
  private static final String _BLUR_PREFIX = "_dfb(this";
  private static final String _FOCUS_PREFIX = "_dff(this";

  // AdfRenderingContext property key for the Map which tracks the
  // active date field for each inlineDatePicker
  private static final Object _ACTIVE_DATE_FIELDS_KEY = new Object();

  private static final String _LAUNCH_PICKER_TIP_KEY =
    "af_selectInputDate.LAUNCH_PICKER_TIP";

  // Key for remembering the cached chooseId
  private static final Object _CACHED_CHOOSE_ID = new Object();

  // name for our scriptlet
  private static final String _DATE_TIME_ZONE_OFFSET_KEY = "dateTimeZoneOffset";
  static
  {
    // Register our scriptlet
    DateTimeZoneOffsetScriptlet.sharedInstance().registerSelf();
  }

  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(SimpleSelectInputDateRenderer.class);

}
