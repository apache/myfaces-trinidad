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
package org.apache.myfaces.trinidadinternal.taglib;

import java.lang.reflect.Array;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.webapp.UIComponentTag;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspTagException;
import javax.servlet.jsp.tagext.TagSupport;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.webapp.ELContextTag;
import org.apache.myfaces.trinidadinternal.el.Tokenizer;
import org.apache.myfaces.trinidadinternal.el.Tokenizer.Token;

//JSTL Core Library - <c:forEach> Tag
//===================================
//Syntax 1: Iterate over a collection of objects
//
//<c:forEach [var="varName "] items="collection"
//  [varStatus="varStatusName"]
//  [begin="begin"] [end="end"] [step=" step"]>
//  body content
//</c:forEach>
//
//Syntax 2: Iterate a fixed number of times
//
//<c:forEach [var="varName"]
//  [varStatus="varStatusName"]
//  begin=" begin" end="end" [step="step"]>
//  body content
//</c:forEach>

/**
 *
 */
public class ForEachTag extends TagSupport implements ELContextTag
{
  public void setItems(String items)
  {
    if (!items.startsWith("#{") ||
        !items.endsWith("}"))
      throw new IllegalArgumentException(_LOG.getMessage(
        "MUST_BE_SIMPLE_JSF_EL_EXPRESSION"));
    _items = items;
  }

  public void setBegin(String begin)
  {
    _begin = begin;
  }

  public void setEnd(String end)
  {
    _end = end;
  }

  public void setStep(String step)
  {
    _step = step;
  }

  public void setVar(String var)
  {
    if (UIComponentTag.isValueReference(var))
      throw new IllegalArgumentException(_LOG.getMessage(
        "VAR_CANNOT_BE_EXPRESSION"));

    _var = var;
  }

  public void setVarStatus(String varStatus)
  {
    if (UIComponentTag.isValueReference(varStatus))
      throw new IllegalArgumentException(_LOG.getMessage(
        "VARSTATUS_CANNOT_BE_EXPRESSION"));

    _varStatus = varStatus;
  }

  @Override
  public int doStartTag() throws JspException
  {
    _validateAttributes();

    FacesContext context = FacesContext.getCurrentInstance();
    _parentELContext = (ELContextTag)
       findAncestorWithClass(this, ELContextTag.class);
    _currentBegin = _resolveInteger(context, _begin, 0);
    int length;
    if (null != _items)
    {
      Object items = _resolveObject(context, _items);
      //pu: If items is specified and resolves to null, it is treated as an
      //  empty collection, i.e., no iteration is performed.
      if (items == null)
      {
        if (_LOG.isFine())
          _LOG.fine("Items expression " + _items + " resolved to null.");
        return SKIP_BODY;
      }
      // =-=AEW <c:forEach> supports arbitrary collections;  but
      // JSF only supports List in its EL.
      if (items instanceof List)
        length = ((List) items).size();
      else if (items.getClass().isArray())
        length = Array.getLength(items);
      else
        throw new JspException(_LOG.getMessage(
          "MUST_POINT_TO_LIST_OR_ARRAY"));
      if (length == 0)
      {
        if (_LOG.isFine())
          _LOG.fine("Items found at " + _items + " is empty.");
        return SKIP_BODY;
      }
      //pu: If valid 'items' was specified, and so was 'begin', get out if size
      //  of collection were to be less than the begin. A mimic of c:forEach.
      if (length < _currentBegin)
      {
        if (_LOG.isFine())
          _LOG.fine("Size of 'items' is less than 'begin'");
        return SKIP_BODY;
      }
      _currentEnd = _resolveInteger(context, _end, length - 1);
      //pu: If 'end' were specified, but is beyond the size of collection, limit
      //  the iteration to where the collection ends. A mimic of c:forEach and
      //  fix for bug 4029853.
      if (length < _currentEnd)
        _currentEnd = length - 1;
    }
    else
    {
      _currentEnd = _resolveInteger(context, _end, 0);
    }
    _currentIndex = _currentBegin;
    _currentStep = _resolveInteger(context, _step, 1);
    //pu: Now check the valid relation between 'begin','end' and validity of 'step'
    _validateRangeAndStep();
    if (_currentEnd < _currentIndex)
      return SKIP_BODY;

    if (null != _var || null != _varStatus)
    {
      //pu: If items not defined (syntax 2), the return type of 'var' is an
      //  int according to JSTL specs, and apache impl returns index. Mimic.
      _varReplacement = (null == _items)?
        String.valueOf(_currentIndex):
        _items.substring(2, _items.length() - 1) + "[" + _currentIndex + "]";
    }
    //pu: If there is no varStatus set, no point in keeping loop status
    //  variables updated.
    if (null != _varStatus)
    {
      _updateLoopStatus();
      _propertyReplacementMap = new HashMap<String, Object>(9, 1);
      _propertyReplacementMap.put("begin", Integer.valueOf(_currentBegin));
      _propertyReplacementMap.put("end", Integer.valueOf(_currentEnd));
      _propertyReplacementMap.put("step", Integer.valueOf(_currentStep));
      _propertyReplacementMap.put("count", Integer.valueOf(_currentCount));
      _propertyReplacementMap.put("index", Integer.valueOf(_currentIndex));
      _propertyReplacementMap.put("current", _varReplacement);
      _propertyReplacementMap.put(
        "first",
        (_isFirst)? Boolean.TRUE:Boolean.FALSE);
      _propertyReplacementMap.put(
        "last",
        (_isLast)? Boolean.TRUE:Boolean.FALSE);
    }

    if (_LOG.isFiner())
    {
      _LOG.finer("Iterating from " + _currentIndex + " to " + _currentEnd +
                 " by " + _currentStep);
    }
    return EVAL_BODY_INCLUDE;
  }

  @Override
  public int doAfterBody()
  {
    _currentIndex += _currentStep;

    if (null != _var || null != _varStatus)
    {
      //pu: If items not defined (syntax 2), the return type of 'var' is an
      //  int according to JSTL specs, and apache impl returns index. Mimic.
      _varReplacement = (null == _items)?
        String.valueOf(_currentIndex):
        _items.substring(2, _items.length() - 1) + "[" + _currentIndex + "]";
    }

    //pu: if there is no varStatus set, no point in keeping loop status
    //  variables updated.
    if (null != _varStatus)
    {
      //pu: _isFirst is not yet updated after first iteration
      boolean isSecondIteration = (_isFirst)? true:false;
      _updateLoopStatus();
      if (isSecondIteration)
      {
        _propertyReplacementMap.put(
          "first",
          (_isFirst)? Boolean.TRUE:Boolean.FALSE);
      }
      if (_isLast)
      {
        _propertyReplacementMap.put(
          "last",
          (_isLast)? Boolean.TRUE:Boolean.FALSE);
      }
      _propertyReplacementMap.put("count", Integer.valueOf(_currentCount));
      _propertyReplacementMap.put("index", Integer.valueOf(_currentIndex));
      _propertyReplacementMap.put("current", _varReplacement);
    }

    if (_currentEnd < _currentIndex)
      return SKIP_BODY;
    return EVAL_BODY_AGAIN;
  }

  public String transformId(String id)
  {
    if (_parentELContext != null)
      id = _parentELContext.transformId(id);

    // SEPARATOR_CHAR would be nice;  but JSF does not allow
    // the separator char in an ID - just in client IDs.
    //    return id + NamingContainer.SEPARATOR_CHAR + _currentIndex;
    return id + '_' + _currentIndex;
  }
  static String __transformExpression(
    String expression,
    String var,
    String subst)
  {
    String varDot = var + ".";
    Tokenizer tokens = new Tokenizer(expression);
    StringBuffer buf = new StringBuffer(expression.length());
    while(tokens.hasNext())
    {
      Token tok = tokens.next();
      String exp = tok.getText();
      if (tok.type == Tokenizer.VAR_TYPE)
      {
        if (var.equals(exp) || exp.startsWith(varDot))
        {
          buf.append(subst);
          buf.append(exp.substring(var.length()));
          continue;
        }
      }

      buf.append(exp);
    }
    return buf.toString();
  }
  public String transformExpression(String expression)
  {
    if (expression != null)
    {
      String transformedExp = expression;
      int expressionStart = expression.indexOf("#{");
      if (expressionStart >= 0)
      {
        transformedExp = _transformExpression(expression);

        if (_parentELContext != null)
          transformedExp = _parentELContext.transformExpression(transformedExp);
      }

      if (_LOG.isFiner())
        _LOG.finer("Transformed expression:{0} to:{1}",
                   new String[] {expression, transformedExp});
      return transformedExp;
    }

    return null;
  }
  /**
   * Release state.
   */
  @Override
  public void release()
  {
    super.release();
    //=-=pu: Does only the properties that has setters need to be released ?
    //  What about variables like _propertyReplacementMap/_varReplacement etc. ?
    _begin = null;
    _end = null;
    _end = null;
    _items = null;
    _step = null;
    _var = null;
    _varStatus = null;
  }

  protected ValueBinding createValueBinding(
    FacesContext context,
    String       expression)
  {
    if (_parentELContext != null)
      expression = _parentELContext.transformExpression(expression);

    return context.getApplication().createValueBinding(expression);
  }

  private void _validateAttributes() throws JspTagException
  {
    if (null == _items)
    {
      if (null == _begin || null == _end)
      {
        throw new JspTagException(
          "'begin' and 'end' should be specified if 'items' is not specified");
      }
    }
    //pu: This is our own check - c:forEach behavior un-defined & unpredictable.
    if ((_var != null) &&
        _var.equals(_varStatus))
    {
      throw new JspTagException(
        "'var' and 'varStatus' should not have same value");
    }
  }

  private void _validateRangeAndStep() throws JspTagException
  {
    if (_currentBegin < 0)
      throw new JspTagException("'begin' < 0");
    if (_currentStep < 1)
      throw new JspTagException("'step' < 1");
  }
  
  private String _transformExpression(String expression)
  {
    boolean doVar = (_var != null);
    boolean doVarStatus = (_varStatus != null);
    if (!(doVar || doVarStatus))
      return expression;
    StringBuffer buf = new StringBuffer(expression.length());
    // ACW: see bug 3754666:
    Tokenizer tokens = new Tokenizer(expression);
    String varDot = _var+".";
    String varStatusDot = _varStatus+".";
    while(tokens.hasNext())
    {
      Token tok = tokens.next();
      String text = tok.getText();
      if (tok.type == Tokenizer.VAR_TYPE)
      {
        if (doVar && (_var.equals(text) || text.startsWith(varDot)))
        {
          text = _replaceVariableAndPropertiesInExpression(
            text, _var, _varReplacement, null);
        }
        else if (doVarStatus && (_varStatus.equals(text) || text.startsWith(varStatusDot)))
        {
          text = _replaceVariableAndPropertiesInExpression(
            text, _varStatus, null, _propertyReplacementMap);
        }
      }
      buf.append(text);
    }
    return buf.toString();
  }
  /**
   * Replaces all occurance of 'variable' and the property (the key in map)
   * with the property replacement (value in the map).
   * If propertyReplacementMap were to be null, then all the occurance of
   * 'variable' will be replaced by 'variableReplacement'.
   * Returns a string modified thus.
   */
  private String _replaceVariableAndPropertiesInExpression(
    String subExpression,
    String variable,
    String variableReplacement,
    Map<String, Object> propertyReplacementMap)
  {
    int variableLength = variable.length();
    //pu: Now check whether the variable is followed by any property from
    //  the supplied map.
    if (null != propertyReplacementMap)
    {
      String property;
      String propertyReplacement;
      for(Map.Entry<String, Object> entry : propertyReplacementMap.entrySet())
      {
        property = entry.getKey();
        String expressionAfterVar = subExpression.substring(variableLength);
        if (expressionAfterVar.startsWith("."+property))
        {
          int propertyLength = property.length();
          //pu: We found our property, but it could be followed
          //  by an alphanumeric in which case we ignore and move on because
          //  we just found it as a substring
          int endOfReplacement = propertyLength + 1;
          if (expressionAfterVar.length() > endOfReplacement)
          {
            if (Character.isLetterOrDigit(
              expressionAfterVar.charAt(endOfReplacement)))
            {
              continue;
            }
          }
          propertyReplacement = entry.getValue().toString();
          //pu: Replace both the variable plus the property following it.
          subExpression = _replaceSubString(
            subExpression,
            0,
            variableLength + propertyLength + 1,
            propertyReplacement);
          //pu: If we handled atleast one property, break out from here.
          break;
        }
      }
    }
    //pu: If there were no properties to be replaced, replace the variable itself
    else
    {
      subExpression = _replaceSubString(subExpression, 0, variableLength, variableReplacement);
    }


    return subExpression;
  }
  /**
   * Given the 'str', replaces a substring starting from 'beginIndex'
   * of 'noOfChars' length with the string in 'replacement', returns the
   * string modified thus.
   */
  private String _replaceSubString(
    String str,
    int beginIndex,
    int noOfChars,
    String replacement)
  {
    StringBuffer buffer = new StringBuffer(str.length() +
                                           replacement.length() -
                                           noOfChars);
    buffer.append(str.substring(0, beginIndex));
    buffer.append(replacement);
    buffer.append(str.substring(beginIndex + noOfChars));
    return buffer.toString();
  }
  /**
   * Update the loop status variables.
   */
  private void _updateLoopStatus()
  {
    _currentCount = ((_currentIndex - _currentBegin)/_currentStep) + 1;
    _isFirst = (_currentIndex == _currentBegin);
    _isLast = (_currentIndex + _currentStep) > _currentEnd;
  }


  private Object _resolveObject(FacesContext context, String expression)
  {
    ValueBinding vb = createValueBinding(context, expression);
    return vb.getValue(context);
  }

  private int _resolveInteger(
    FacesContext context,
    String       expression,
    int          defaultValue)
  {
    if (expression == null)
      return defaultValue;

    if (UIComponentTag.isValueReference(expression))
    {
      Object o = _resolveObject(context, expression);
      if (o instanceof Number)
        return ((Number) o).intValue();
      if (o == null)
        return defaultValue;

      expression = o.toString();
    }
    return Integer.parseInt(expression);
  }

  private int _currentBegin;
  private int _currentIndex;
  private int _currentEnd;
  private int _currentStep;
  private int _currentCount;
  private boolean _isFirst;
  private boolean _isLast;
  private ELContextTag _parentELContext;

  private String _items;
  private String _begin;
  private String _end;
  private String _step;
  private String _var;
  private String _varStatus;
  //pu: Map for properties referred off from 'varStatus' and their replacements
  private Map<String, Object> _propertyReplacementMap;
  //pu: Represents replacement for 'var' upon every iteration
  private String _varReplacement;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);

}
