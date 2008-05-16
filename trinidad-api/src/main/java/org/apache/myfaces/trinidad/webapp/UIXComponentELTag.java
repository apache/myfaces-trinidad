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
package org.apache.myfaces.trinidad.webapp;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.TimeZone;

import javax.el.MethodExpression;
import javax.el.ValueExpression;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentClassicTagBase;
import javax.faces.webapp.UIComponentELTag;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.Tag;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.change.AddComponentChange;
import org.apache.myfaces.trinidad.change.ChangeMarker;
import org.apache.myfaces.trinidad.change.ComponentChange;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Subclass of UIComponentTag to add convenience methods,
 * and optimize where appropriate.
 */
abstract public class UIXComponentELTag extends UIComponentELTag
{
  public UIXComponentELTag()
  {
  }

  public void setAttributeChangeListener(MethodExpression attributeChangeListener)
  {
    _attributeChangeListener = attributeChangeListener;
  }

  @Override
  public int doStartTag() throws JspException
  {
    int retVal = super.doStartTag();

    //pu: There could have been some validation error during property setting
    //  on the bean, this is the closest opportunity to burst out.
    if (_validationError != null)
      throw new JspException(_validationError);

    return retVal;
  }

  @Override
  public int doEndTag() throws JspException
  {
    UIComponent component = getComponentInstance();
    if (getCreated() || _forceApplyChanges)
      _applyChanges(getFacesContext(), component);
    return super.doEndTag();
  }


  @Override
  protected final void setProperties(UIComponent component)
  {
    if (component instanceof UIViewRoot)
    {
      throw new IllegalStateException(
         "<f:view> was not present on this page; tag " + this +
         "encountered without an <f:view> being processed.");
    }

    super.setProperties(component);

    UIXComponent uixComponent = (UIXComponent) component;

    if (_attributeChangeListener != null)
    {
      uixComponent.setAttributeChangeListener(_attributeChangeListener);
    }

    setProperties(uixComponent.getFacesBean());
  }

  protected void setProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, expression.getValue(null));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.lang.String[].  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of strings.
   * Null values are ignored.
   */
  protected void setStringArrayProperty(
    FacesBean       bean,
    PropertyKey     key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, _parseNameTokens(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.lang.Number.  If the value
   * is an EL expression, it will be stored as a ValueBinding.
   * Otherwise, it will parsed with Integer.valueOf() or Double.valueOf() .
   * Null values are ignored.
   */
  protected void setNumberProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Object value = expression.getValue(null);
      if (value != null)
      { 
        if (value instanceof Number)
        {
          bean.setProperty(key, value);
        }
        else
        {
          String valueStr = value.toString();
          if(valueStr.indexOf('.') == -1)
            bean.setProperty(key, Integer.valueOf(valueStr));
          else
            bean.setProperty(key, Double.valueOf(valueStr));
        }
      }
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type int[].  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as a whitespace-separated series
   * of ints.
   * Null values are ignored.
   */
  protected void setIntArrayProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Object value = expression.getValue(null);
      if (value != null)
      {
        String[] strings = _parseNameTokens(value);
        final int[] ints;
        if (strings != null)
        {
          try
          {
            ints = new int[strings.length];
            for(int i=0; i<strings.length; i++)
            {
              int j = Integer.parseInt(strings[i]);
              ints[i] = j;
            }
          }
          catch (NumberFormatException e)
          {
            _LOG.severe("CANNOT_CONVERT_INTO_INT_ARRAY",value);
            _LOG.severe(e);
            return;
          }
        }
      }
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  /**
   * Set a property of type java.util.Date.  If the value
   * is an EL expression, it will be stored as a ValueExpression.
   * Otherwise, it will parsed as an ISO 8601 date (yyyy-MM-dd).
   * Null values are ignored.
   */
  protected void setDateProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      bean.setProperty(key, _parseISODate(expression.getValue(null)));
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

    /**
   * Set a property of type java.util.Date.  If the value
   * is an EL expression, it will be stored as a ValueBinding.
   * Otherwise, it will parsed as an ISO 8601 date (yyyy-MM-dd)
   * and the time components (hour, min, second, millisecond) maximized.
   * Null values are ignored.
   */
    protected void setMaxDateProperty(
    FacesBean   bean,
    PropertyKey key,
    ValueExpression expression)
  {
    if (expression == null)
      return;

    if (expression.isLiteralText())
    {
      Date d = _parseISODate(expression.getValue(null));
      Calendar c = Calendar.getInstance();
      TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
      if (tz != null)
        c.setTimeZone(tz);
      c.setTime(d);
      // Original value had 00:00:00 for hours,mins, seconds now maximize those
      // to get the latest time value for the date supplied.
      c.set (Calendar.HOUR_OF_DAY, 23);
      c.set (Calendar.MINUTE, 59);
      c.set (Calendar.SECOND, 59);
      c.set (Calendar.MILLISECOND, 999);
      bean.setProperty(key, c.getTime());
    }
    else
    {
      bean.setValueExpression(key, expression);
    }
  }

  protected void setProperties(FacesBean bean)
  {
    // Could be abstract, but it's easier to *always* call super.setProperties(),
    // and perhaps we'll have something generic in here, esp. if we take
    // over "rendered" from UIComponentTag
  }

  /**
   * Sets any fatal validation error that could have happened during property
   *  setting. If this is set, tag execution aborts with a JspException at the
   *  end of doStartTag().
   * @param validationError
   */
  protected void setValidationError(String validationError)
  {
    _validationError = validationError;
  }

  /**
   * Parse a string into a java.util.Date object.  The
   * string must be in ISO 9601 format (yyyy-MM-dd).
   */
  static private final Date _parseISODate(Object o)
  {
    if (o == null)
      return null;

    String stringValue = o.toString();
    try
    {
      return _getDateFormat().parse(stringValue);
    }
    catch (ParseException pe)
    {
      _LOG.info("CANNOT_PARSE_VALUE_INTO_DATE", stringValue);
      return null;
    }
  }

  /**
   * Parses a whitespace separated series of name tokens.
   * @param stringValue the full string
   * @return an array of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   * @todo Move to utility function somewhere (ADF Share?)
   */
  static private final String[] _parseNameTokens(Object o)
  {
    if (o == null)
      return null;

    String stringValue = o.toString();
    ArrayList<String> list = new ArrayList<String>(5);

    int     length = stringValue.length();
    boolean inSpace = true;
    int     start = 0;
    for (int i = 0; i < length; i++)
    {
      char ch = stringValue.charAt(i);

      // We're in whitespace;  if we've just departed
      // a run of non-whitespace, append a string.
      // Now, why do we use the supposedly deprecated "Character.isSpace()"
      // function instead of "isWhitespace"?  We're following XML rules
      // here for the meaning of whitespace, which specifically
      // EXCLUDES general Unicode spaces.
      if (Character.isWhitespace(ch))
      {
        if (!inSpace)
        {
          list.add(stringValue.substring(start, i));
          inSpace = true;
        }
      }
      // We're out of whitespace;  if we've just departed
      // a run of whitespace, start keeping track of this string
      else
      {
        if (inSpace)
        {
          start = i;
          inSpace = false;
        }
      }
    }

    if (!inSpace)
      list.add(stringValue.substring(start));

    if (list.isEmpty())
      return null;

    return list.toArray(new String[list.size()]);
  }

  /**
   * Locate and return the nearest enclosing UIComponentClassicTagBase for a given tag if any;
   *  otherwise, return null.
   * @param tag The tag to start the walk up from
   * @return The nearest enclosing UIXComponentELTag instance
   */
  private UIComponentClassicTagBase _getParentUIComponentClassicTagBase(
    UIComponentClassicTagBase tag)
  {
    if (tag == null)
      return null;

    Tag result = tag.getParent();
    while (result != null)
    {
      if (result instanceof UIComponentClassicTagBase)
        return (UIComponentClassicTagBase) result;
      result = result.getParent();
    }
    return null;
  }

  /**
   * Walks up the tree, starting from a supplied startTag and finds an enclosing tag matching the
   * supplied absolute idPath.
   * @param startTag The tag from where to start the tag tree walk up from.
   * @param idPath   An array of id's that represents the absolute id of the enclosing tag.
   * @return         The UIXComponentELTag that matches the absolute id if find was successful, 
   *                  null otherwise.
   */
  private UIXComponentELTag _findEnclosingTag(
    UIXComponentELTag startTag,
    String[] idPath)
  {
    if (startTag == null || idPath == null || idPath.length == 0)
      return null;

    LinkedList<UIComponentClassicTagBase> enclosingTags = 
      new LinkedList<UIComponentClassicTagBase>();
    // The last entry will be the id of the tag to be found.
    String targetId = idPath[idPath.length - 1];

    // Step 1: Move up and build a list of all enclosing tags for the startTag.
    for(UIComponentClassicTagBase currComponentTag = startTag;
        currComponentTag != null;
        currComponentTag = _getParentUIComponentClassicTagBase(currComponentTag))
    {
      enclosingTags.addFirst(currComponentTag);
    }
    
    // Bail out if we could not build a trace path of enclosing tags.
    if (enclosingTags.isEmpty())
      return null;

    // Step 2: Walk forward the list of enclosing tags to find the target.
    Iterator<UIComponentClassicTagBase> iter = enclosingTags.iterator();

    // Step 2a: If there are tags corresponding to NamingContainers in list, skip the expected
    //  number of such NamingContainer tags.
    int namingContainerIndex = 0;
    for(UIComponentClassicTagBase currComponentTag = iter.next();
        currComponentTag != null;
        currComponentTag = iter.next())
    {
      UIComponent currComponent = currComponentTag.getComponentInstance();
      
      if (currComponent instanceof NamingContainer)
      {
        // If the id of this 'NamingContainer' tag does not match the next segment in sequence,
        //  we would never be able to reach target, bail out.
        if (! currComponent.getId().equals(idPath[namingContainerIndex]))
          return null;
        else
        {
          namingContainerIndex++;
          if (namingContainerIndex == idPath.length - 1)
            break;
        }
      }
    }

    // Did not find all of our path elements, something very messed up.
    if (namingContainerIndex < idPath.length - 1)
      return null;
    
    // Step 2b: Look for the target tag from here on.
    for (UIComponentClassicTagBase currComponentTag = iter.next();
         currComponentTag != null;
         currComponentTag = iter.next())
    {
      if (targetId.equals(currComponentTag.getComponentInstance().getId()))
      {
        // If we are here, we reached the target, and it is very likely a UIXComponentELTag.
        return (currComponentTag instanceof UIXComponentELTag) ? 
          (UIXComponentELTag)currComponentTag : null;
      }
    }

    // If we reached here, we could not find the target.
    return null;
  }

  private void _applyChanges(
    FacesContext facesContext, 
    UIComponent uiComponent)
  {
    RequestContext afc = RequestContext.getCurrentInstance();
    Iterator<ComponentChange> changeIter =
      afc.getChangeManager().getComponentChanges(facesContext, uiComponent);

    if (changeIter == null)
      return;
    while (changeIter.hasNext())
    {
      ComponentChange change = changeIter.next();

      // If this is just a marker change, find and flag the target component.
      if (change instanceof ChangeMarker)
      {
        String id = ((ChangeMarker) change).getChangeTargetComponentScopedId();
        String[] idPath = id.split(String.valueOf(NamingContainer.SEPARATOR_CHAR));
        UIXComponentELTag targetTag = _findEnclosingTag(this, idPath);
        if (targetTag != null)
          targetTag._forceApplyChanges = true;
      }
      else
      {
        change.changeComponent(uiComponent);

        // In case this Change has added a new component/facet, the added
        //  component could have its own Changes, that may need to be applied here.
        if (change instanceof AddComponentChange)
        {
          UIComponent newAddedComponent =
            ((AddComponentChange) change).getComponent();

          if (newAddedComponent != null)
          {
            _applyChanges(facesContext, newAddedComponent);
          }
        }
      }
    }
  }

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(UIXComponentELTag.class);

  // We rely strictly on ISO 8601 formats
  private static DateFormat _getDateFormat()
  {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    TimeZone tz = RequestContext.getCurrentInstance().getTimeZone();
    if (tz != null)
      sdf.setTimeZone(tz);
    return sdf;
  }

  // Use if change application needs to be forced on the tag regardless of whether the 
  //  corresponding component is newly created during tag execution.
  private boolean           _forceApplyChanges;
  private MethodExpression  _attributeChangeListener;
  private String            _validationError;
}
