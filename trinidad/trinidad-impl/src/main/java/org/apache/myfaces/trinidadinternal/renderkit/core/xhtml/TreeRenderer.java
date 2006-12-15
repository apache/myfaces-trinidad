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

package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.core.data.CoreTree;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TreeUtils;

/**
 * Renderer for trees. 
 *
 * @author The Oracle ADF Faces Team
 */
public class TreeRenderer extends XhtmlRenderer
{
  public TreeRenderer()
  {
    this(CoreTree.TYPE);
  }

  protected TreeRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _immediateKey = type.findKey("immediate");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * @todo do not mess with selection here. queue an event.
   */
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> parameters =
      context.getExternalContext().getRequestParameterMap();
    String source = parameters.get(XhtmlConstants.SOURCE_PARAM);

    if (!component.getClientId(context).equals(source))
      return;

    TreeUtils.decodeExpandEvents(parameters, component, 
                                 Collections.emptyList());
    String currencyStrParam = 
      source + NamingContainer.SEPARATOR_CHAR + SELECTED_PARAM;
    String currencyStr = parameters.get(currencyStrParam);
    if ((currencyStr != null) && (!"".equals(currencyStr)))
    {
      UIXTree tree = (UIXTree) component;
      Object oldPath = tree.getRowKey();
      tree.setClientRowKey(currencyStr);
      tree.getSelectedRowKeys().clear();
      tree.getSelectedRowKeys().add();
      tree.setRowKey(oldPath);
    }

    RequestContext.getCurrentInstance().addPartialTarget(component);
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,    
    UIComponent      component,
    FacesBean        bean) throws IOException
  {
    UIXHierarchy tree = (UIXHierarchy) component;
    TreeUtils.expandFocusRowKey((UIXTree) component);

    Object oldPath = tree.getRowKey();
    try
    {
      boolean continueRendering = setInitialPath(tree, bean);
      if (!continueRendering)
        return;

      _renderContent(context, rc, tree, bean);
    }
    finally
    {
      tree.setRowKey(oldPath);
    }
  }

  protected boolean shouldRenderId(FacesContext context,
                                   UIComponent component)
  {
    return true;
  }
  
  private void _renderContent(FacesContext context,
                              RenderingContext rc,
                              UIXHierarchy tree,
                              FacesBean bean)
    throws IOException
  {
    FormData fd = rc.getFormData();
    if (fd == null)
    {
      _LOG.warning("The tree component must be used inside of a form.");
      return;
    }

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", tree);
    renderId(context, tree);
    renderAllAttributes(context, rc, bean);
    
    final String id = getClientId(context, tree);
    UIComponent stamp = getFacet(tree, CoreTree.NODE_STAMP_FACET);

    //@todo - will this tree.getFocusPath survive?
    //    List focusPath = getFocusPath(context, node);
    Object focusPath = tree.getFocusRowKey();
    String formName = fd.getName();

    // Bug 3931544:  don't use colons in Javascript variable names.
    // We'll just replace colons with underscores;  not perfect, but adequate
    final String varName = "_adftree" + XhtmlUtils.getJSIdentifier(id);

    boolean leftToRight = !rc.isRightToLeft();
    int rootSize = tree.getRowCount();
    RowKeySet state = getExpandedRowKeys(tree);
    Map<Object, Boolean> selectedPaths = getSelectedPaths(focusPath);

    // render each of the root nodes
    for (int i = 0; i < rootSize; i++)
    {
      tree.setRowIndex(i);
      _renderNode(context, rc, tree, bean, stamp, varName, state, 
                  selectedPaths, new Boolean[_DEFAULT_TREE_DEPTH], 
                  leftToRight, (i == 0), (i == rootSize - 1), 0);
    }

    rw.startElement("script", null);
    renderScriptDeferAttribute(context, rc);
    renderScriptTypeAttribute(context, rc);

    _renderTreeJS(context, rc, bean);

    //out.writeText("_setNodes('"+name+"','"+nodesRendered+"');");

    String selectedParam = 
      id + NamingContainer.SEPARATOR_CHAR + SELECTED_PARAM;

    rw.writeText("var " + varName + " = " + 
                     _createNewJSSelectionState(formName, id, 
                                                selectedParam), null);
    rw.endElement("script");
    rw.endElement("div");
    
    fd.addNeededValue(selectedParam);
    fd.addNeededValue(_PATH_PARAM);


  }


  // return whether to continue with rendering

  protected boolean setInitialPath(UIXHierarchy tree, FacesBean bean)
  {
    tree.setRowKey(null);
    return true;
  }


  private boolean _isShownSelected(UIXHierarchy tree, 
                                   Map<Object, Boolean> selectedPaths, 
                                   Object currPath)
  {

    boolean selected = false;
    if (tree instanceof UIXTree)
      selected = ((UIXTree) tree).getSelectedRowKeys().isContained();

    if (selected)
      return true;

    Object value = selectedPaths.get(currPath);

    if (value != null)
      return true;

    return false;
  }

  protected Map<Object, Boolean> getSelectedPaths(Object focusPath)
  {
    if (focusPath == null)
      return new HashMap<Object, Boolean>(0);

    Map<Object, Boolean> selectedPaths = new HashMap<Object, Boolean>(1);

    selectedPaths.put(focusPath, Boolean.TRUE);
    return selectedPaths;
  }

  protected RowKeySet getExpandedRowKeys(UIXHierarchy tree)
  {
    return ((UIXTree) tree).getDisclosedRowKeys();
  }

  protected String getConnectingBackgroundIcon(boolean isLine, 
                                               boolean leftToRight)
  {
    return null;
  }

  protected String getIconBackgroundIcon(int expand, boolean isLeftToRight)
  {
    return null;
  }


  // render the correct icon for a specific node

  protected void renderExpandCell(FacesContext context, 
                                  RenderingContext rc,
                                  int expanded, String onclick)
    throws IOException
  {


    Object altText = null;

    String text = null;

    boolean isMacOS = rc.getAgent().getPlatformName().equals(Agent.PLATFORM_MACOS);
    // add in the expandability
    switch (expanded)
    {
      case NO_CHILDREN:
        break;
      case EXPAND_CLOSED:
        // "\u21D2"; // Double Arrow right

        if (isMacOS)
          // single arrow left
          text = 
            rc.isRightToLeft()? "\u2190": "\u2192"; // single arrow right
        else // triangle left
          text = 
            rc.isRightToLeft()? "\u25C4": "\u25BA"; // triangle right
        altText = rc.getTranslatedString(_EXPAND_TIP_KEY);
        break;
      case EXPAND_OPEN:
        //"\u21D3"; // double arrow down
        if (isMacOS)
          text = "\u2193"; // single arrow down
        else
          text = "\u25BC"; // triangle down
        altText = rc.getTranslatedString(_COLLAPSE_TIP_KEY);
        break;
      case EXPAND_ALWAYS:
        if (isMacOS)
          text = "\u2193"; // single arrow down
        else
          text = "\u25BC"; // triangle down
        altText = rc.getTranslatedString(_DISABLED_COLLAPSE_TIP_KEY);
        break;
    }

    _renderTextCell(context, rc, text, altText, _ICON_WIDTH, onclick, 
                    SkinSelectors.TREE_DISCLOSED_SYMBOL_STYLE_CLASS);

  }


  private void _renderTextCell(FacesContext context,
                               RenderingContext rc,
                               String text, 
                               Object altText, String width, 
                               String onclick, String styleClass)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);
    writer.writeAttribute("title", altText, null);
    renderStyleClass(context, rc, styleClass);

    if (onclick != null)
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
    }

    writer.writeText(text, null);

    if (onclick != null)
    {
      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }


  protected void renderIconCell(FacesContext context, RenderingContext rc,
                                UIXHierarchy tree, String backgroundIcon, 
                                String icon, boolean isIconAbsoluteURI, 
                                Object altText, String width, 
                                String height, String onclick)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);

    if (backgroundIcon != null)
    {
      String backgroundIconURI = getAbsoluteImageUri(context, rc, backgroundIcon);
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, 
                            _BACKGROUND_IMAGE_URL + backgroundIconURI + 
                            _END_FUNC, null);
    }

    if (onclick != null)
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
      writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      String treeName = getClientId(context, tree);
      String id = treeName + NamingContainer.SEPARATOR_CHAR + "lnk";
      writer.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, id, null);
    }

    _renderIcon(context, rc, icon, isIconAbsoluteURI, altText, width, height);

    if (onclick != null)
    {
      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }

    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
  }


  private static String _createNewJSSelectionState(String formName, 
                                                   String treeClientId, 
                                                   String selectParam)
  {
    return "new _adfTreeSelector('" + selectParam + "'," + 
      TreeUtils.createNewJSCollectionComponentState(formName, 
                                                    treeClientId) + ");";
  }

  private static String _callJSSelect(UIXHierarchy tree, String jsVarName)
  {
    String currencyStr = tree.getClientRowKey();
    return jsVarName + ".select(this,'" + currencyStr + "');";
  }

  private void _renderTreeJS(FacesContext context,
                             RenderingContext rc,
                             FacesBean bean)
    throws IOException
  {
    if (!rc.getProperties().containsKey(_JS_RENDERED_KEY))
    {
      rc.getProperties().put(_JS_RENDERED_KEY, Boolean.TRUE);
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("function _adfTreeSelector(selectParam,tState) {" + 
                       "this._selectParam = selectParam;" + 
                       "this._pTag = null;" + "this.treeState = tState;" + 
                       "}" + 
                       "_adfTreeSelector.prototype.select = function(tag,path) {" + 
                       "if (this._pTag != null) {" + 
                       "this._pTag.className='" + SkinSelectors.TREE_ROW_STYLE_CLASS + 
                       "';" + "}" + "this._pTag = tag;" + 
                       "document.forms[this.treeState.getFormName()][this._selectParam].value=path;" + 
                       "tag.className='" + SkinSelectors.TREE_ROW_SELECTED_STYLE_CLASS + 
                       "';" + "};", null);


      // _setSelection(..) and _getSelection(..) are called by the
      // ClientStateTreeDataProxy (if selection is enabled for this particular
      // tree). _setSelection is called to set the initial selection (as
      // determined by the proxy). _getSelection is called to get at the
      // current selection state (to send back to the server by the proxy)

      // @param source the name or ID of the tree
      // @param sel something that identifies the current selected node
      // _setSelection(source,sel)

      // @param source the name or ID of the tree
      // @return something that identifies the current selected node
      // _getSelection(source)

      //      writer.writeText
      //        ("var _treeSel = new Object();"  +
      //         "var _treeNodes = new Object();"  +
      //         "function _setSelection(source,sel) {"
      //         + "_treeSel[source]=sel;"  +
      //         "}"  +
      //         "function _getSelection(source) {"
      //         + "return _treeSel[source];"  +
      //         "}"  +
      //
      //         // _setNodes is used to indicate the number of tree nodes that are
      //         // currently visible. This is so that _clearSelection knows exactly
      //         // how many elements to clear
      //         "function _setNodes(source,nodes) {"
      //         + "_treeNodes[source]=nodes;"  +
      //         "}"  +
      //         "function _getNodes(source) {"
      //         + "return _treeNodes[source];"  +
      //         "}",
      //         null  );
      //
      //      writer.writeText
      //        ("function _select(name,index,nodeID) {"
      //         + "_clearSelection(name);"
      //         + "var e =_getElementById(document,name+index);"
      //         + "e.className = '",
      //         null  );
      //      writer.writeText(TREE_ROW_SELECTED_STYLE_CLASS+"';", null);
      //      writer.writeText
      //        ("  _setSelection(name,nodeID);return true;"  +
      //         "}",
      //         null  );
      //
      //      writer.writeText
      //        ("function _clearSelection(name) {"
      //         + "var sz = _getNodes(name);"
      //         + "for (var i = 0; i < sz; i++) {"
      //         +   "var e =_getElementById(document,name+i);"
      //         +   "e.className='",
      //         null);
      //      writer.writeText(TREE_ROW_STYLE_CLASS+"';", null);
      //      writer.writeText
      //        ("  }"  +
      //         "}",
      //         null);

      boolean immediate = getImmediate(bean);
      String buff = 
        TreeUtils.setupJSTreeCollectionComponent(!immediate) + ";";
      writer.writeText(buff, null);
    }
  }

  // render one row of the tree

  private void _renderNode(FacesContext context,
                           RenderingContext rc,
                           UIXHierarchy tree, 
                           FacesBean bean,
                           UIComponent stamp,
                           final String varName, 
                           RowKeySet state, 
                           Map<Object, Boolean> selectedPaths, 
                           Boolean[] prepend, boolean leftToRight, 
                           boolean isFirstSibling, boolean isLastSibling, 
                           int nodeDepth)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // each row is a table
    writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
    OutputUtils.renderLayoutTableAttributes(context, rc, "0", "0", "0", null);
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);


    // render the prepend
    _prependIcons(context, rc, tree, prepend, leftToRight);


    String onclickExpand = null;
    int expand = _getExpandValue(tree, state);

    if ((expand != NO_CHILDREN) && supportsNavigation(rc))
    {
      onclickExpand = 
          TreeUtils.callJSExpandNode(tree, varName + ".treeState", 
                                     (expand == EXPAND_CLOSED));
    }

    renderExpandCell(context, rc, expand, onclickExpand);


    //    DataObject curData = BeanAdapterUtils.getAdapter(context, tree.getRowData());
    String treeStyle = SkinSelectors.TREE_ROW_STYLE_CLASS;


    // location was a colon separated list of IDs
    //boolean selected = proxy.isSelected(context, node, location);
    Object currPath = tree.getRowKey();
    boolean selected = _isShownSelected(tree, selectedPaths, currPath);

    String onClick = _callJSSelect(tree, varName);

    //    if ( proxy.selectionEnabled(context) )
    //    {
    //      // selection with the proxy doesn't work on netscape
    //      // filed as bug 1817185 - so far we have not figured
    //      // out a way without using layers and we are seeing nodes
    //      // jump around with layers so disabling selection on netscape
    //      if ( isNetscape(context) )
    //        selected = false;
    //      else
    //      {
    //        if (supportsNavigation(context))
    //          onClick = "return _select('" + treename + "'," + renderedIndex +
    //            ",'" + location + "');";
    //      }
    //    }

    if (selected)
    {
      treeStyle = SkinSelectors.TREE_ROW_SELECTED_STYLE_CLASS;
    }

    // render the icon
    if (true/*icon != null*/)
    {
      String backgroundIcon = getIconBackgroundIcon(expand, leftToRight);

      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);

      if (backgroundIcon != null)
      {
        String backgroundIconURI = 
          getAbsoluteImageUri(context, rc, backgroundIcon);

        StringBuffer backgroundStyle = 
          new StringBuffer(_BACKGROUND_IMAGE_URL.length() + 
                           backgroundIconURI.length() + 
                           _END_FUNC.length());

        backgroundStyle.append(_BACKGROUND_IMAGE_URL);
        backgroundStyle.append(backgroundIconURI);
        backgroundStyle.append(_END_FUNC);

        writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, backgroundStyle.toString(), 
                              null);
      }

      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

      // render space between icon and node stamp
      // alt Text
      renderIconCell(context, rc, tree, null, TRANSPARENT_GIF, false, null, 
                     _NODE_SPACER, _ICON_HEIGHT, null);
    }

    // render the node stamp
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(XhtmlConstants.NOWRAP_ATTRIBUTE, Boolean.FALSE, null);
    renderStyleClass(context, rc, SkinSelectors.TREE_NODE_ADJUST_STYLE_CLASS);


    writer.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    //    out.writeAttribute(ID_ATTRIBUTE,
    //                       treename + IntegerUtils.getString(renderedIndex));
    renderStyleClass(context, rc, treeStyle);
    writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onClick, null);

    // if screen reader mode render the stamp with level of node from root
    _renderStampBasedOnAccessibilty(context, rc, stamp, nodeDepth);

    writer.endElement(XhtmlConstants.SPAN_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

    // end row
    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
    //end table
    writer.endElement(XhtmlConstants.TABLE_ELEMENT);

    // render children
    if ((expand == EXPAND_OPEN) || (expand == EXPAND_ALWAYS))
    {
      tree.enterContainer();
      int childCount = tree.getRowCount();

      if (childCount > 0)
      {
        // prepare the prepended icons for the child nodes
        prepend = 
            _appendIcon(prepend, (isLastSibling)? Boolean.FALSE: Boolean.TRUE);
        Boolean[] currClone;


        ++nodeDepth; // increment the depth of the child from the root

        int oldIndex = tree.getRowIndex();
        for (int i = 0; i < childCount; i++)
        {
          currClone = new Boolean[prepend.length];
          System.arraycopy(prepend, 0, currClone, 0, prepend.length);

          tree.setRowIndex(i);
          _renderNode(context, rc, tree, bean, stamp, varName, state, 
                      selectedPaths, currClone, leftToRight, false, 
                      (i == childCount - 1), nodeDepth);
        }
        tree.setRowIndex(oldIndex);
        --nodeDepth;
      }
      tree.exitContainer();
    }
  }

  // is this row childless, open, or closed?

  private int _getExpandValue(UIXHierarchy tree, RowKeySet state)
  {
    if (tree.isContainer())
    {
      if (state.isContained())
        return EXPAND_OPEN;
      else
        return EXPAND_CLOSED;
    }

    return NO_CHILDREN;
  }


  // render an icon with our own special formatting

  private void _renderIcon(FacesContext context, RenderingContext rc, String icon, 
                           boolean isIconAbsoluteURI, Object text, 
                           String width, String height)
    throws IOException
  {
    if (icon != null)
    {
      ResponseWriter writer = context.getResponseWriter();

      // TODO: change 
      writer.startElement("img", null);
      renderStyleClass(context, rc, SkinSelectors.TREE_ICON_STYLE_CLASS);
      writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, width, null);
      writer.writeAttribute(XhtmlConstants.HEIGHT_ATTRIBUTE, height, null);

      // Convert iconURL to an absolute uri
      if (!isIconAbsoluteURI)
        icon = getAbsoluteImageUri(context, rc, icon);
    
      renderEncodedResourceURI(context, "src", icon);

      // Ensure that we're never rendering null;  see bug 4161181
      // why this logic is not in renderAltAndTooltipForImage().
      // This is, in essence, re-introducing a more restricted version
      // of that bug.
      OutputUtils.renderAltAndTooltipForImage(context, rc, text == null? "": text);

      writer.writeAttribute("border", "0", null);
      writer.endElement("img");
    }
  }


  // add a boolean flag to the chain of icons.
  // the chain is rendered before each icon

  private Boolean[] _appendIcon(Boolean[] prepend, Boolean isLine)
  {
    int currLength = prepend.length;

    if (prepend[currLength - 1] != null)
    {
      // resize, incrementing should be fine
      Boolean[] newBools = 
        new Boolean[currLength + _DEFAULT_TREE_INCREMENT];
      System.arraycopy(prepend, 0, newBools, 0, currLength);
      prepend = newBools;
    }

    for (int i = 0; i < currLength; i++)
    {
      if (prepend[i] == null)
      {
        prepend[i] = isLine;
        break;
      }
    }

    return prepend;
  }


  private void _prependIcons(FacesContext context, RenderingContext rc,
                             UIXHierarchy tree, Boolean[] prepend, 
                             boolean leftToRight)
    throws IOException
  {
    int currLength = prepend.length;
    Boolean isLine;

    for (int i = 0; i < currLength; i++)
    {

      isLine = prepend[i];

      if (isLine != null)
      {
        String icon = TRANSPARENT_GIF;

        String backgroundIcon = 
          getConnectingBackgroundIcon(isLine.booleanValue(), leftToRight);

        // alt text
        renderIconCell(context, rc, tree, backgroundIcon, icon, false, null, 
                       _ICON_WIDTH, _ICON_HEIGHT, null);
      }
    }

  }


  protected boolean getImmediate(FacesBean bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }



  protected String getDefaultIconName()
  {
    return null;
  }


  private void _renderStampBasedOnAccessibilty(FacesContext context,
                                               RenderingContext rc,
                                               UIComponent stamp,
                                               int depth)
    throws IOException
  {
    if (isScreenReaderMode(rc))
    {
      RenderingContext arc = RenderingContext.getCurrentInstance();
      FacesContext fc = FacesContext.getCurrentInstance();
      if (rc.isRightToLeft())
      {
        //TODO: do we need default stamp support???
        encodeChild(context, stamp);
        TreeUtils.writeNodeLevel(fc, arc, depth, _NODE_LEVEL_TEXT_KEY);
      }
      else
      {
        TreeUtils.writeNodeLevel(fc, arc, depth, _NODE_LEVEL_TEXT_KEY);
        encodeChild(context, stamp);
      }
    }
    else
      encodeChild(context, stamp);
  }

  private static final String _BACKGROUND_IMAGE_URL = 
    "background-image:url(";
  private static final String _END_FUNC = ");";

  private static final String _ICON_WIDTH = "16";
  private static final String _ICON_HEIGHT = "22";
  private static final String _NODE_ICON_HEIGHT = "16";
  private static final String _NODE_SPACER = "6";


  // expanded states
  protected static final int NO_CHILDREN = 0;
  protected static final int EXPAND_CLOSED = 1;
  protected static final int EXPAND_OPEN = 2;
  protected static final int EXPAND_ALWAYS = 3;

  // prepend chain constants
  private static final int _DEFAULT_TREE_DEPTH = 10;
  private static final int _DEFAULT_TREE_INCREMENT = 5;

  // =-= ACW: this key is used to make sure that certain javascript functions
  // used by this renderer, are rendered only once per render cycle.
  private static final Object _JS_RENDERED_KEY = new Object();

  // Key used by StyledTextBean to query style class
  static final String _STYLE_CLASS_KEY = "_styleClass";


  private PropertyKey _immediateKey;

  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY = 
    "af_tree.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY = "af_tree.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY = "af_tree.EXPAND_TIP";
  private static final String _FOLDER_TIP_KEY = "af_tree.FOLDER_TIP";
  private static final String _NODE_LEVEL_TEXT_KEY = "af_tree.NODE_LEVEL";

  private static final String _PATH_PARAM = "path";
  public static final String SELECTED_PARAM = "_selected";


  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(TreeRenderer.class);
}
