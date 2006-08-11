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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import java.text.BreakIterator;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXValue;

import org.apache.myfaces.trinidadinternal.ui.AttributeKey;
import org.apache.myfaces.trinidadinternal.ui.NodeUtils;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;

import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/TextInputRenderer.java#1 $) $Date: 11-nov-2005.14:59:38 $
 * @author The Oracle ADF Faces Team
 */
public class TextInputRenderer extends FormInputRenderer
{
  @Override
  protected void renderAsNonElement(
    UIXRenderingContext context, UINode node) throws IOException
  {
    renderContent(context, node, false);
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    boolean isTextArea = isTextArea(context, node);

    Object columns = getColumns(context, node);

    // determine whether we need to shrink the columns
    boolean shrinkColumns = shrinkColumns(context);

    if (columns == null)
    {
      int defaultColumns = getDefaultColumns();

      // default columns
      columns = getInteger(shrinkColumns
                           ? isTextArea
                             ? (defaultColumns * 3) / 4
                             : (defaultColumns * 3) / 5
                           : defaultColumns);
    }
    else
    {
      if (columns instanceof Number)
      {
        int intCol = ((Number) columns).intValue();
        // keep columns from being set too large.
        if (intCol > _MAX_COLUMNS)
        {
          intCol = _MAX_COLUMNS;
          columns = getInteger(intCol);
        }

        if (shrinkColumns)
        {
          if (isTextArea)
            intCol = ((intCol * 3) + 3) / 4;
          else
            intCol = ((intCol * 3) + 4) / 5;

          columns = getInteger(intCol);
        }
      }
    }

    renderAttribute(context,
                    (isTextArea)
                      ? COLS_ATTRIBUTE
                      : SIZE_ATTRIBUTE,
                    columns);

      // Don't render any validation if we're not actually an
      // element (because we're read-only or disabled)

    if (isTextArea)
    {
      Object rows = node.getAttributeValue(context, ROWS_ATTR);
      if (rows == null)
        rows = getInteger(getDefaultRows());
      else
      {
        if (rows instanceof Number)
        {
          // Some have found it cute to set the rows to
          // an outlandishly large value.  Block this.
          int intRow = ((Number) rows).intValue();
          if (intRow > _MAX_ROWS)
          {
            rows = getInteger(_MAX_ROWS);
          }
        }
      }

      renderAttribute(context,
                      ROWS_ATTRIBUTE,
                      rows);

      renderAttribute(context,
                      node,
                      WRAP_ATTRIBUTE,
                      WRAP_ATTR);

      // render the readonly attribute
      if (supportsReadOnlyFormElements(context))
        renderAttribute(context, "readonly", getReadOnly(context, node));
    }
    else
    {
      // render the autocomplete attribute
      if (supportsAutoCompleteFormElements(context))
      {
        // BUG 4019675: support autocomplete
        boolean noAutoComplete = isNoAutoComplete(context, node);
        if (noAutoComplete)
        {
          renderAttribute(context, "autocomplete", "off");
        }
      }

      Object text = getText(context, node);
      boolean secret =
        Boolean.TRUE.equals(node.getAttributeValue(context, SECRET_ATTR));
      if (secret)
      {
        renderAttribute(context, TYPE_ATTRIBUTE, "password");
        // only render the text if we are not a password field:
        // bug 2748426
        if ((text != null) && !("".equals(text)))
        {
          text = UIConstants.SECRET_FIELD_DEFAULT_VALUE; // bug 3448201
        }
      }
      else
      {
        renderAttribute(context, TYPE_ATTRIBUTE, "text");
      }

      renderAttribute(context, VALUE_ATTRIBUTE, text);
      renderAttribute(context,
                      node,
                      MAXLENGTH_ATTRIBUTE,
                      MAXIMUM_LENGTH_ATTR);
    }
  }


  /**
   * Returns true if the passed in columns should be shrunken when displaying
   */
  protected boolean shrinkColumns(
    UIXRenderingContext context
    )
  {
    // assume that columns don't need to be shrunken
    return false;
  }


  /**
   * Returns the default number of text area rows
   */
  protected int getDefaultRows()
  {
    return 5;
  }


  /**
   * Returns the default number of text input columns
   */
  protected int getDefaultColumns()
  {
    return 30;
  }


  /**
   * Renders event handlers for the node.
   */
  @Override
  protected void renderEventHandlers(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderEventHandlers(context, node);

    renderAttribute(context, "onchange", getOnChange(context, node));
    renderAttribute(context, node, "onselect", ON_SELECT_ATTR);
  }


  /**
   * Get the name for a node. In rare cases, the renderer must specify
   * the name.
   */
  @Override
  protected Object getNodeName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // HTML spec states that form elements with no name are
    // "unsuccessful", meaning their name value pair is not
    // sent to the server, such as an unchecked checkbox.
    // For readonly textareas, twe want to preserve the current
    // semantics of not submiting the form value to the server.
    // Returning null for the name in this case, causes the
    // name attribute not to be rendered, producing the
    // desired behavior.
    return (isTextArea(context, node) &&
            Boolean.TRUE.equals(getReadOnly(context, node)))
              ? null  // make form element "unsuccessful"
              : super.getNodeName(context, node);
  }

  @Override
  protected Object getOnKeyPress(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object onKeyPress = super.getOnKeyPress(context, node);

    if ( supportsScripting(context))
      {
      if (isTextArea(context, node))
      {
        Object maxLength = node.getAttributeValue(context,
                                                  MAXIMUM_LENGTH_ATTR);
        if (maxLength instanceof Number)
        {
          onKeyPress = _getMaxLengthFunction(onKeyPress,
                                             ((Number)maxLength).intValue());
        }
      }
      else
      {

        ClientAction  action = (ClientAction)
              node.getAttributeValue(context, ENTER_CLIENT_ACTION_ATTR);
        if ((action != null) &&
            !action.isTriggerRequired(context, node))
        {
          String actionScript = action.getScript(context, node, Boolean.TRUE);
          actionScript = "if (_getKC(event)==13){" + actionScript + "};";
          onKeyPress = XhtmlLafUtils.getChainedJS(onKeyPress, actionScript, true);
        }
        // we only need to handle autosubmission if no elements that
        // would prevent us from being auto submitted have been rendered
        //
        //
        // Note: This assumes that this code is being called
        // before the rest of FileUploadRenderer.prerender()
        // is called.
        //
      }
    }

    return onKeyPress;
  }


  protected Object getOnChange(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object onChange = node.getAttributeValue(context, ON_CHANGE_ATTR);

    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);

    if ((action != null) && !action.isTriggerRequired(context, node))
    {
      String actionScript = action.getScript(context, node, Boolean.TRUE);

      if (actionScript != null)

      onChange  = XhtmlLafUtils.getChainedJS(onChange,
                                             actionScript,
                                             true);
    }

    if (isTextArea(context, node))
    {
      Object maxLength = node.getAttributeValue(context,
                                                MAXIMUM_LENGTH_ATTR);
      if (maxLength instanceof Number)
      {
        onChange = _getMaxLengthFunction(onChange,
                                         ((Number) maxLength).intValue());
      }
    }

    return onChange;
  }

  /**
   * Override to write any client JavaScript dependencies for the validater.
   */
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // true if we need to update the text input count after
    // calling super.prerender in order to handle suppressing
    // autosubmission
    boolean updateTextInputCount = false;

    if (supportsScripting(context))
    {
      // we are going to need to update the text input count after
      // super.prerender
      updateTextInputCount = true;

      // If we've got a ClientAction, let it write its dependencies
      // before we start rendering the field
      ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                     node);
      if (action != null)
        action.writeDependencies(context, node);
    }



    // actually start rendering this node
    super.prerender(context, node);

    if (updateTextInputCount)
    {
      //keep track because we want Enter to cause a submit if there is one
      //and only one text input
      FormRenderer.incrementInputTextCount();
    }
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    renderContent(context, node, true);
  }

  protected void renderContent(
    UIXRenderingContext context,
    UINode           node,
    boolean          renderAsElement) throws IOException
  {
    if (isTextArea(context, node))
    {
      ResponseWriter writer = context.getResponseWriter();

      // render the element text here
      Object textValue = getText(context, node);

      // If we can't render as an element (because we're disabled, usually),
      // then we're not inside of an element - which means carriage returns
      // get ignored.  But whitespace matters inside a textarea.  Best we
      // can do is turn on "pre"formatted mode.
      if (!renderAsElement)
      {
        writer.startElement("pre", NodeUtils.getUIComponent(context, node));

        if (supportsStyleAttributes(context) &&
            doRenderStyleAttrs(context, node))
        {
          renderStyleAttrs(context, node);
        }

        // And, for a read-only text input - we also want to wrap
        // the text to the number of columns
        if (textValue != null)
        {
          String textString = textValue.toString();
          int textLength = textString.length();

          if (textLength > 0)
          {
            Object wrap = node.getAttributeValue(context, WRAP_ATTR);

            // Only wrap if "wrap" is set to SOFT or HARD
            if (SOFT_WRAP.equals(wrap) ||
                HARD_WRAP.equals(wrap))
            {
              Object columnsObj = getColumns(context, node);

              int columns = ((columnsObj == null)
                             ? getDefaultColumns()
                             : ((Number) columnsObj).intValue());

              // If wrapping is pointless, skip it.
              if ((columns <= 1) || (columns > textLength))
              {
                writer.writeText(textString, UIXValue.VALUE_KEY.getName());
              }
              else
              {
                // Get a BreakIterator.  Note that this code doesn't
                // really work for multi-lingual pages (e.g., an English
                // page that contains some Japanese text).
                BreakIterator breaks = BreakIterator.getLineInstance(
                  context.getLocaleContext().getTranslationLocale());
                breaks.setText(textString);

                _writeTextWithBreaks(context, breaks, textString, columns);
              }
            } // endif wrapping on
            else
            {
              writer.writeText(textString, UIXValue.VALUE_KEY.getName());
            }
          } // endif textLength > 0
        } // endif textvalue != null

        writer.endElement("pre");
      } // endif !renderAsElement
      else
      {
        writer.writeText(textValue, UIXValue.VALUE_KEY.getName());
      }
    }  // endif isTextArea()
    else if (!renderAsElement)
    {
      // Don't render anything for disabled password fields
      if (!Boolean.TRUE.equals(node.getAttributeValue(context,
                                                     SECRET_ATTR)))
      {
        renderStyledText(context, node);
      }
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    super.postrender(context, node);
    // If we have a ClientAction which requires an explicit
    // trigger, render the trigger now...
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if ((action != null) && action.isTriggerRequired(context, node))
    {
      action.renderTrigger(context, node);
    }
  }

  private void _writeTextWithBreaks(
    UIXRenderingContext context,
    BreakIterator    breaks,
    String           textString,
    int              columns) throws IOException
  {
    int start = 0;
    while (true)
    {
      int nextLineBreak = textString.indexOf('\n', start);
      String substring;
      if (nextLineBreak >= 0)
        substring = textString.substring(start, nextLineBreak);
      else
        substring = textString.substring(start);

      _writeTextLineWithBreaks(context, breaks, substring, columns);

      if (nextLineBreak < 0)
        break;

      start = nextLineBreak + 1;
          char[] chars = new char['\n'];
      context.getResponseWriter().write(chars, 0, 1);
    }
  }

  private void _writeTextLineWithBreaks(
    UIXRenderingContext context,
    BreakIterator    breaks,
    String           textString,
    int              columns) throws IOException
  {
    if (textString.length() < columns)
    {
      context.getResponseWriter().writeText(textString, UIXValue.VALUE_KEY.getName());
      return;
    }

    breaks.setText(textString);

    int lastStart = 0;
    int previous = 0;
    while (true)
    {
      int next = breaks.next();
      if (next == BreakIterator.DONE)
      {
        context.getResponseWriter().writeText(textString.substring(lastStart), null);
        break;
      }

      if (next - lastStart > columns)
      {
        // Even if the first string in this line was too long,
        // always output a complete line.
        if (previous == lastStart)
          previous = next;

        String sub = textString.substring(lastStart, previous);
                    char[] chars = new char['\n'];
        context.getResponseWriter().writeText(sub, null);
        context.getResponseWriter().write(chars, 0, 1);

        lastStart = previous;
      }

      previous = next;
    }
  }


  protected Object getColumns(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, COLUMNS_ATTR);
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return isTextArea(context, node)
             ? TEXT_AREA_ELEMENT
             : INPUT_ELEMENT;
  }

  @Override
  protected Object getDefaultStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (_isNumericField(/*context, node*/))
             ? AF_FIELD_NUMBER_STYLE_CLASS
             : AF_FIELD_TEXT_STYLE_CLASS;
  }

  @Override
  protected Object getDefaultDisabledStyleClass(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (_isNumericField(/*context, node*/))
             ? AF_FIELD_NUMBER_DISABLED_STYLE_CLASS
             : AF_FIELD_TEXT_DISABLED_STYLE_CLASS;
  }


  /**
   * @todo - Find a efficient way to identify that this is a numeric field
   */
  private boolean _isNumericField(
//    RenderingContext context,
//    UINode           node
    )
  {
    return false;
  }


  /**
   * Controls whether moving to next sibling doesn't require validation.
   * This is overridden by subclasses that generate the sibling component
   */
  protected boolean isNextSibOK()
  {
    return false;
  }

  // map this node to <input type="text"> or <textarea>?
  protected boolean isTextArea(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // if the node has rows > 1, it's a textarea
    Number rows = (Number)node.getAttributeValue(context,
                                                 ROWS_ATTR);
    if ((rows != null) && (rows.intValue() > 1))
      return true;

    return false;
  }

  @Override
  protected boolean renderReadOnlyAsElement(
    UIXRenderingContext context,
    UINode           node)
  {
    // jrf: hack, hack, hack
    // Bug 2669974.  Apps needs ability to preserve 2.1.1 behavior for
    //               readonly textareas.
    if (node.getRawAttributeValue(context, _HACK_2669974) != null)
      return !Boolean.TRUE.equals(getReadOnly(context, node));

    return (supportsReadOnlyFormElements(context) &&
            isTextArea(context, node));

  }



  protected boolean isNoAutoComplete(
    UIXRenderingContext context,
    UINode           node)
  {
    return (Boolean.TRUE.equals(node.getAttributeValue(context,
                                                       NO_AUTO_COMPLETE_ATTR)));
  }

  static private Object _getMaxLengthFunction(
    Object userFunc,
    int    length
    )
  {
    String functionCall = "return _checkLength(this," +
           IntegerUtils.getString(length) +
           ",event)";

    if (userFunc == null)
      return functionCall;

    return XhtmlLafUtils.getChainedJS(functionCall, userFunc, true);
  }

  static private final int _MAX_COLUMNS = 500;
  static private final int _MAX_ROWS    = 500;

  static private final AttributeKey _HACK_2669974 =
                                  AttributeKey.getAttributeKey("_hack2669974");
}
