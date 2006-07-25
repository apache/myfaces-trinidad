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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;


import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.AttributeKey;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FrameBorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:53:53 $
 * @author The Oracle ADF Faces Team
 */
public class FrameBorderLayoutRenderer extends XhtmlLafRenderer
{
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    ResponseWriter writer = context.getResponseWriter();

    // this is a "secret" attribute set in FredJSP to fix
    // 4339153 DIALOGS IN FIREFOX HAVE WHITE LINE AT THE BOTTOM
    if (Boolean.FALSE.equals(node.getAttributeValue(context,
                                                    UIConstants.BORDER_ATTR)))
    {
      writer.writeAttribute("frameborder","0", null);
    }

    // Explicitly render rows to ensure onUnload handler will fire
    writer.writeAttribute(ROWS_ATTRIBUTE, "100%,*", null);

    if (XhtmlLafRenderer.supportsScripting(context))
    {
      writer.writeAttribute("onload",
                            node.getAttributeValue(context, ON_LOAD_ATTR),
							null);

      /**
       * =-= bts
       * We can't chain the JavaScript because we have no way of
       * making sure that Core.js has been included in the HEAD
       * tag
       */
      // =-=AEW We no longer attempt to chain in _checkUnload() (contrary
      // to the comment from bts, we were doing so).  Note that
      // _checkUnload() only needs to be called from a modal window.
      // But the only framesets we use on modal window are created
      // by fred.jsp (and its ancestor, frameRedirect.jsp).  These
      // use HTMLWebBean to create the frameset.
      // We're still left with the possibility that some clients
      // will have used FrameBorderLayoutRenderer as the top piece
      // of a modal window, and went to the effort of adding
      // a ScriptBean to the head section to load Core.js.
      // We could help these people out quite a bit by modifying
      // _checkUnload() so that firing on the BodyBean of one
      // of the contained frames is sufficient (it should be), but let's
      // be conservative for now about mucking around with our Javascript
      writer.writeAttribute("onunload",
                             node.getAttributeValue(context,
                                                    ON_UNLOAD_ATTR),
							 null);
    }
  }

  public boolean isSupportedNode(
    RenderingContext context,
    UINode           node
    )
  {
    // only supported if advanced frames are supported
    return supportsFrames(context);
  }

    /**
  protected void prerender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

     * =-= bts This code is hosed because FrameSets, at least in Mozilla,
     *         don't allow <script> tags to be entered into their contents
     *         willy, nilly.
     * =-= aew And IE 5.5 doesn't either (it ignores the <script> tag)

    // we depend on the window library. so include it after the
    // frameset tag is opened
    if (XhtmlLafRenderer.supportsScripting(context))
    {
      XhtmlLafUtils.addLib(context, XhtmlLafUtils.CORE_LIB);
    }
  }
    */

  protected void postrender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // render the stuff that deals with browsers without frames support
    writer.startElement("noframes", null);
    renderNamedChild(context, node, ALTERNATE_CONTENT_CHILD);
    writer.endElement("noframes");

    super.postrender(context, node);
  }

  /**
   * overwrites the superclass method so that no children are written except
   * for the named ones.
   */
  protected void renderContent(
    RenderingContext context,
    UINode           node
    )
    throws IOException
  {
    String leftName       = LEFT_CHILD;
    String innerLeftName  = INNER_LEFT_CHILD;
    String rightName      = RIGHT_CHILD;
    String innerRightName = INNER_RIGHT_CHILD;

    UINode center = getNamedChild(context, node, CENTER_CHILD);
    UINode top    = getNamedChild(context, node, TOP_CHILD);
    UINode bottom = getNamedChild(context, node, BOTTOM_CHILD);
    UINode left   = getNamedChild(context, node, leftName);
    UINode right  = getNamedChild(context, node, rightName);
    UINode innerLeft   = getNamedChild(context, node, innerLeftName);
    UINode innerRight  = getNamedChild(context, node, innerRightName);

    int dir = context.getLocaleContext().getReadingDirection();
    boolean r2l = (dir == LocaleUtils.DIRECTION_RIGHTTOLEFT);

    if (left == null)
    {
      leftName = r2l ? END_CHILD : START_CHILD;
      left     = getNamedChild(context, node, leftName);
    }

    if (right == null)
    {
      rightName = r2l ? START_CHILD : END_CHILD;
      right     = getNamedChild(context, node, rightName);
    }

    if (innerLeft == null)
    {
      innerLeftName = r2l ? INNER_END_CHILD : INNER_START_CHILD;
      innerLeft     = getNamedChild(context, node, innerLeftName);
    }

    if (innerRight == null)
    {
      innerRightName = r2l ? INNER_START_CHILD : INNER_END_CHILD;
      innerRight     = getNamedChild(context, node, innerRightName);
    }

    ResponseWriter writer = context.getResponseWriter();

    //sizes is set to be "top.height , * , bottom.height"
    String sizes = _getSizeString(context,
                                  top,
                                  null,
                                  null,
                                  bottom,
                                  HEIGHT_ATTR);

    // if sizes=="*" then there is no need for a frameset since there is only
    // the center frame
    boolean renderTopBottomFrame = (sizes.length() > 1);

    if (renderTopBottomFrame)
    {
      // this frameset contains the top and bottom children
      writer.startElement("frameset", null);
      if (!isInaccessibleMode(context))
      {
        writer.writeAttribute(
          "title",
          context.getTranslatedString(
            "af_frameBorderLayout.HORIZONTAL_FRAMESET_LAYOUT_CONTAINER_TITLE"),
          null);
      }
      writer.writeAttribute(ROWS_ATTRIBUTE, sizes, null);
    }

    renderNamedChild(context, node, top, TOP_CHILD);

    // sizes is set to be
    // "left.width , innerLeft.width , * , innerRight.width , right.width"
    //
    sizes = _getSizeString(context,
                           left,
                           innerLeft,
                           innerRight,
                           right,
                           WIDTH_ATTR);
    // if sizes=="*" then there is no need for a frameset since there is only
    // the center frame
    boolean renderLeftRightFrame = (sizes.length() > 1);

    if (renderLeftRightFrame)
    {
      // this frameset renders the left, center and right children
      writer.startElement("frameset", null);
      if (!isInaccessibleMode(context))
      {
        writer.writeAttribute(
          "title",
          context.getTranslatedString(
            "af_frameBorderLayout.VERTICAL_FRAMESET_LAYOUT_CONTAINER_TITLE"),
          null);
      }
      writer.writeAttribute(COLS_ATTRIBUTE, sizes, null);
    }

    renderNamedChild(context, node, left, leftName);
    renderNamedChild(context, node, innerLeft, innerLeftName);
    renderNamedChild(context, node, center, CENTER_CHILD);
    renderNamedChild(context, node, innerRight, innerRightName);
    renderNamedChild(context, node, right, rightName);

    if (renderLeftRightFrame)
    {
      // end the left-center-right frameset
      writer.endElement("frameset");
    }

    renderNamedChild(context, node, bottom, BOTTOM_CHILD);

    if (renderTopBottomFrame)
    {
      // end the top-bottom frameset
      writer.endElement("frameset");
    }
  }

  protected String getElementName(
    RenderingContext context,
    UINode node
    )
  {
    return "frameset";
  }

  /**
   * The UINodes may be null.
   * @return a String of the form
   *  "outer1.attr , inner1.attr, * , inner2.attr , outer2.attr"
   * @param attr the attribute to get.
   */
  private String _getSizeString(
    RenderingContext context,
    UINode           outer1,
    UINode           inner1,
    UINode           inner2,
    UINode           outer2,
    AttributeKey     attr
    )
  {
    StringBuffer buf = new StringBuffer(17);

    if (outer1 != null)
      _getAttrValue(context, outer1, attr, buf).append(',');

    if (inner1 != null)
      _getAttrValue(context, inner1, attr, buf).append(',');

    buf.append('*');

    if (inner2 != null)
      _getAttrValue(context, inner2, attr, buf.append(','));

    if (outer2 != null)
      _getAttrValue(context, outer2, attr, buf.append(','));

    return buf.toString();
  }

  private StringBuffer _getAttrValue(RenderingContext context,
                                     UINode frame,
                                     AttributeKey attr,
                                     StringBuffer result)
  {
    Object val = frame.getAttributeValue(context, attr);

    if ((val==null))
    {
      if (_LOG.isWarning())
        _LOG.warning("frame:" + getIDOrName(context, frame)
                     + " is missing attribute:" + attr);
      val="0";
    }
    result.append(val);
    return result;
  }
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(FrameBorderLayoutRenderer.class);
}
