/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.logging.ADFLogger;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXNavigationTree;
import org.apache.myfaces.trinidad.component.UIXPage;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.FocusEvent;

import org.apache.myfaces.trinidad.model.RowKeySet;

import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HiddenLabelUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TableRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

/**
 * Class that handles all tree related events
 * @author The Oracle ADF Faces Team
 */
public final class TreeUtils
{
  private TreeUtils()
  {
  }

  // Names of the parameters used by TreeUtils.  The names
  // need to be public so form value can be prepared;
  // the meaning of the parameters are not public
  public static final String EVENT_PARAM = "event";
  public static final String SOURCE_PARAM = "source";


  /**
   * This method is used for writing the depth of node from it's root in
   * the case tree and HGrid, thus making it more accessible.
   */
  public static void writeNodeLevel(
    FacesContext          context,
    AdfRenderingContext   arc,
    int              depth,
    String           nodeLevelTextKey
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("label", null);
    String pattern = arc.getTranslatedString(nodeLevelTextKey);
    String levelInfo =
      XhtmlUtils.getFormattedString(pattern,
                                    new String[]
                                    {
                                      IntegerUtils.getString(depth)
                                    });

    if (HiddenLabelUtils.supportsHiddenLabels(arc))
      XhtmlRenderer.renderStyleClass(context, arc,
                         XhtmlConstants.HIDDEN_LABEL_STYLE_CLASS);

    writer.writeText(levelInfo, null);
    writer.endElement("label");
  }

  // TODO: this method should be removed.
  public static Object getFocusRowKey(UIXTreeTable treeTable)
  {
    Object path = treeTable.getFocusRowKey();
    if (path == null)
    {
      int oldIndex = treeTable.getRowIndex();
      treeTable.setRowIndex(0);
      Object rowKey = treeTable.getRowKey();
      treeTable.setRowIndex(oldIndex);
      return rowKey;
    }
    else
      return path;
  }

  /**
   * writes the JS function needed for generating tree events.
   * @param buff the function source is appended to this buffer
   * @return
   */
  public static StringBuffer setupTreeCollectionComponent(
    StringBuffer buff,
    boolean      validate)
  {
    String validateString = validate?"1":"0";
    buff.append
      ("CollectionComponent.defineTree('"+
        EVENT_PARAM+"','"+
        SOURCE_PARAM+"','"+
        _PATH_PARAM+"','"+
        _START_PARAM+"','"+
        _GOTO+"','"+
        _FOCUS+"'," +
        validateString + ");"
      );

    return buff;
  }

  public static String setupJSTreeCollectionComponent(boolean validate)
  {
    String validateString = validate?"1":"0";
    return
       "CollectionComponent.defineTree('"+
        EVENT_PARAM+"','"+
        SOURCE_PARAM+"','"+
        _PATH_PARAM+"','"+
        _START_PARAM+"','"+
        _GOTO+"','"+
        _FOCUS+ "'," +
        validateString + ")";
  }

  public static String setupJSMultiSelectCollectionComponent(
    String selectedKey,
    String selectedModeKey,
    boolean autoSubmit)
  {
    return
      "CollectionComponent.defineMultiSelect('"+
        selectedKey+"','"+
        selectedModeKey+"',"+(autoSubmit ? "true" : "false")+")";
  }

  public static String createNewJSCollectionComponentState(String formName, String treeClientId)
  {
    // must not end with a ";". This is because this might be passed as an
    // argument to some other function:
    return "new CollectionComponent('"+formName+"','"+treeClientId+"')";
  }

  public static String callJSExpandNode(UIXHierarchy tree, String jsVarName,
                                        boolean isExpand)
  {
    String path = _getPathParam(tree);
    return jsVarName+".action('"+(isExpand ? _SHOW : _HIDE)+
      "','"+path+"',this);return false;";
  }

  public static String callJSGotoNode(UIXHierarchy tree, String jsVarName,
                                      int rangeStart)
  {
    String path = _getPathParam(tree);
    return jsVarName+".range('"+path+"',"+rangeStart+");return false;";
  }

  public static String callJSFocusNode(UIXHierarchy tree, String jsVarName)
  {
    String path = _getPathParam(tree);
    return jsVarName+".focus('"+path+"',this);return false;";
  }

  public static String callJSExpandAll(UIXHierarchy tree, String jsVarName,
                                      boolean isExpand)
  {
    return jsVarName+".action('"+(isExpand ? _SHOW : _HIDE)+
      "','"+_ALL_VALUE+"',this);return false;";
  }

  public static String callJSSelectAll(String jsVarName, boolean isSelectAll)
  {
    return jsVarName+".multiSelect("+(isSelectAll ? "true" : "false")+")";
  }

  public static void decodeGotoEvent(final Map parameters,
                                     UIComponent tree)
  {
    Object event = parameters.get(EVENT_PARAM);
    if (_GOTO.equals(event))
    {
      PreserveState preserve = new PreserveState()
      {
        protected void process(UIXHierarchy tree)
        {
          final int newStart;
          String startParam = (String) parameters.get(_START_PARAM);
          if ((startParam == null) || ("".equals(startParam)))
          {
            // this must be a root level range change:
            startParam = (String) parameters.get(XhtmlConstants.VALUE_PARAM);
            newStart = Integer.parseInt(startParam) - 1; // value is based at one.
            tree.setRowKey(tree.getFocusRowKey());
            tree.setRowIndex(newStart);
            // queue a focusChange event as well as range change event:
            new FocusEvent(tree).queue();
          }
          else // large record set navigation
          {
            // set the currency to be the container that was scrolled:
            _restorePathFromParam(parameters, tree);
            newStart = Integer.parseInt(startParam);
          }
          TableRenderer.createRangeChangeEvent(tree, newStart).queue();
        }
      };
      preserve.run((UIXHierarchy) tree);
    }
  }

  public static void decodeFocusEvent(final Map parameters,
                                      UIComponent tree)
  {
    Object event = parameters.get(EVENT_PARAM);
    if (_FOCUS.equals(event))
    {
      PreserveState preserve = new PreserveState()
      {
        protected void process(UIXHierarchy tree)
        {
          _restorePathFromParam(parameters, tree);
          new FocusEvent(tree).queue();
        }
      };
      preserve.run((UIXHierarchy) tree);
    }
  }

  public static void decodeExpandEvents(final Map parameters,
                                        final UIComponent tree,
                                        final Object focusRowKey)
  {
    Object event = parameters.get(EVENT_PARAM);
    final Boolean expand;
    if (_HIDE.equals(event))
    {
      expand = Boolean.FALSE;
    }
    else if (_SHOW.equals(event))
    {
      expand = Boolean.TRUE;
    }
    else
    {
      return;
    }

    PreserveState preserve = new PreserveState()
    {
      protected void process(UIXHierarchy tree)
      {
        final FacesEvent event;

        Object key = parameters.get(_PATH_PARAM);
        if (_ALL_VALUE.equals(key)) // expandAll event
        {
          if (focusRowKey == null)
          {
            _LOG.severe("Unexpected tree state: focus rowKey is empty on an " +
                        "expand/collapse all request.");
            return;
          }
          else
          {
            tree.setRowKey(focusRowKey);
            RowKeySet old = _getExpandedRowKeys(tree);
            RowKeySet newset = old.clone();
            if (expand)
              newset.addAll();
            else
              newset.removeAll();
            event = new RowDisclosureEvent(old, newset, tree);
          }
        }
        else  // expand/collapse event
        {
          _restorePathFromParam(parameters, tree);
          RowKeySet old = _getExpandedRowKeys(tree);
          RowKeySet newset = old.clone();
          newset.setContained(expand);
          event = new RowDisclosureEvent(old, newset, tree);
        }
        event.queue();
      }
    };

    preserve.run((UIXHierarchy) tree);
  }

  private static RowKeySet _getExpandedRowKeys(UIXHierarchy tree)
  {
    if (tree instanceof UIXTree)
      return ((UIXTree) tree).getDisclosedRowKeys();
    if (tree instanceof UIXNavigationTree)
      return ((UIXNavigationTree) tree).getDisclosedRowKeys();
    if (tree instanceof UIXPage)
      return ((UIXPage) tree).getDisclosedRowKeys();
    throw new IllegalArgumentException("Don't know how to get disclosedRowKeys from:"+tree);
  }

  private static void _restorePathFromParam(Map parameters, UIXHierarchy tree)
  {
    String currencyString = (String) parameters.get(_PATH_PARAM);
    tree.setCurrencyString(currencyString);
  }

  private static String _getPathParam(UIXHierarchy tree)
  {
    String currencyString = tree.getCurrencyString();
    return currencyString;
  }

  private abstract static class PreserveState
  {
    public void run(UIXHierarchy tree)
    {
      Object oldPath = tree.getRowKey();
      try
      {
        process(tree);
      }
      finally
      {
        tree.setRowKey(oldPath);
      }
    }

    protected abstract void process(UIXHierarchy tree);
  }

  private static final String _GOTO = "goto";
  private static final String _HIDE = "hide";
  private static final String _SHOW = "show";
  private static final String _FOCUS = "focus";
  private static final String _ALL_VALUE = "all";
  private static final String _PATH_PARAM = "path";
  private static final String _START_PARAM = "start";

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(TreeUtils.class);
}
