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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.component.UIXHierarchy;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FormValueRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ProcessUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ProcessTrainRenderer.java#0 $) $Date: 10-nov-2005.18:56:11 $
 * @author The Oracle ADF Faces Team

 */
public class ProcessTrainRenderer extends HtmlLafRenderer
{

  protected UIXHierarchy getHierarchyBase(
    RenderingContext context,
    UINode           node
  )
  {
    return (UIXHierarchy) node.getUIComponent();
  }


  protected UINode getStamp(
    RenderingContext context,
    UINode           node
    )
  {
    return node.getNamedChild(context, NODE_STAMP_CHILD);
  }


  protected boolean setNewPath(
    RenderingContext context,
    UINode           node,
    UIXHierarchy    component
  )
  {
    Object focusPath = component.getFocusRowKey();
    component.setRowKey(focusPath);
    return true;


  }


  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
    renderLayoutTableAttributes(context, "0", null);
    context.getResponseWriter().writeAttribute("align", "center", null);
  }

  /* this is how we can render both the user defined styleClass and our
   * component style class
   */
  protected void renderStyleAttrs(
    RenderingContext context,
    UINode           node
  ) throws IOException
  {
    renderStyleAttrs(context, node, AF_PROCESS_TRAIN_STYLE_CLASS);
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "table";
  }

  /**
   * Overrride to render in two passes.  The first pass is the graphical
   * elements.  The second pass is the text elements.
   */
  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    UIXHierarchy component = getHierarchyBase(context, node);
    UINode stamp = getStamp(context, node);

    if(stamp != null)
    {
      Object oldPath = component.getRowKey();
      boolean isNewPath = setNewPath(context, node, component);
      if (isNewPath)
      {

        ProcessTrainRenderer.TrainState state =
            getTrainState(context, node, component);


        renderHiddenFields( context, node, state);

        renderContent(context, node, component, stamp, state);

        component.setRowKey(oldPath);
      }
    }
  }

  protected void logProcessTrainModelTypeError()
  {
    if (_LOG.isFine())
      _LOG.fine("The value model type for process components " +
                "must be a List of Page objects");
  }

  protected void renderHiddenFields(
    RenderingContext context,
    UINode           node,
    TrainState       state
  )throws IOException
  {

    if ((state.formName != null) && supportsScripting(context))
    {
      // render hidden fields to hold the form data
      FormValueRenderer.addNeededValue( context,
                                        state.formName,
                                        state.eventKey,
                                        state.sourceKey,
                                        state.valueKey,
                                        state.sizeKey);

      // Render script submission code.
      ProcessUtils.renderNavSubmitScript(context);
    }
  }

  /**
   * Gather up the train state: selectedIndex, maxVisitedIndex, startIndex,
   *  isSubTrain, readOnly, formName, id, eventKey, sourceKey, valueKey
   *  This way all the parameters we need to pass around to various methods
   *  are all in one place, the TrainState
   * @param context RenderingContext
   * @param node the processTrain node.
   * @param pages a List of Page objects.
   * @return
   */
  protected TrainState getTrainState(
    RenderingContext context,
    UINode           node,
    UIXHierarchy    component
  )
  {
    ProcessTrainRenderer.TrainState state =
      new ProcessTrainRenderer.TrainState();
    state.selectedIndex = component.getRowIndex();

    // get highest node in train visited
    state.maxVisitedIndex = _getMaxVisitedIndex(context, node);
    //
    // default to selectedIndex if it wasn't set
    // or if it was set to be greater than selectedIndex
    if (state.maxVisitedIndex == NO_CHILD_INDEX ||
        state.maxVisitedIndex < state.selectedIndex)
    {
      state.maxVisitedIndex = state.selectedIndex;
    }

    int totalPages = component.getRowCount();
    state.startIndex = _getStartIndex(totalPages,
                                      state.selectedIndex);

    state.subTrain = _isSubTrain(context, node);

    state.readOnly = _isReadOnly(context, node);

    state.formName = XhtmlLafUtils.getSubmitFormName(context, node);
    Object id = getID(context, node);
    state.id = (id != null) ? id.toString() : null;

    URLEncoder encoder = context.getURLEncoder();
    state.eventKey  = encoder.encodeParameter(EVENT_PARAM);
    state.sourceKey = encoder.encodeParameter(SOURCE_PARAM);
    state.valueKey  = encoder.encodeParameter(VALUE_PARAM);
    state.sizeKey  = encoder.encodeParameter(SIZE_PARAM);

    return state;
  }


  /**
   * return what the starting index into the stations List.
   */
  private int _getStartIndex(
    int numPages,
    int originalSelectedIndex)
  {
    int currentMinIndex = 0;

    if (numPages <= _MAX_NUM_LINK_INDEX)
      return currentMinIndex;

    int selectedIndex = originalSelectedIndex;


    int currentMaxIndex = _MAX_NUM_LINK_INDEX - 1;

    if(selectedIndex < currentMaxIndex)
    {
      return currentMinIndex; //0
    }
    // the algorithm below works, but I thought it was too cryptic
    // return (selectedIndex -
    //            (((selectedIndex-1)%(_MAX_NUM_LINK_INDEX-2))+1));

    // loop until the selectedIndex range is found or
    // we have gone past the number of nodes in the train.
    // Then we'll know what index to start the visible portion of the train.
    while(numPages > currentMaxIndex)
    {
      currentMinIndex = currentMaxIndex-1;
      currentMaxIndex += (_MAX_NUM_LINK_INDEX-2);
      if(selectedIndex > currentMinIndex && selectedIndex < currentMaxIndex)
        return currentMinIndex;
    }

    return currentMinIndex;

  }


  /**
  * Get the subTrain attribute from the node and return it.
  */
  private boolean _isSubTrain(
    RenderingContext context,
    UINode node)
  {
    UIXHierarchy component = getHierarchyBase(context, node);
    Object focusRowKey = component.getFocusRowKey();
    if ( focusRowKey != null && (component.getDepth(focusRowKey) > 0))
      return true;

    return false;
  }

  /**
  * Returns true if READ_ONLY_ATTR is set to Boolean.TRUE
  */
  private static boolean _isReadOnly(
    RenderingContext context,
    UINode           node
    )
  {
    return
      getBooleanAttributeValue(context, node, READ_ONLY_ATTR, Boolean.FALSE);
  }

  /**
  * Returns the MAX_VISITED_ATTR
  * @todo =-=jmw Hopefully the controller will tell us this someday.
  */
  protected static Object _getMaxVisited(
    RenderingContext context,
    UINode           node
    )
  {
   // return node.getAttributeValue(context, MAX_VISITED_ATTR);
   return null;
  }

  /**
  * Get the maxVisited attribute from the node and return it.
  */
  private int _getMaxVisitedIndex(
    RenderingContext context,
    UINode node)
  {
    int maxVisitedIndex = NO_CHILD_INDEX;
    Integer  maxVisited = (Integer)_getMaxVisited(context, node);
    if (maxVisited != null)
    {
      maxVisitedIndex = maxVisited.intValue();
    }
    return maxVisitedIndex;
  }





  /**
   *
   *
   */
  protected void renderContent(
    RenderingContext context,
    UINode           node,
    UIXHierarchy    component,
    UINode           stamp,
    TrainState       train
  ) throws IOException
  {

    int length = component.getRowCount();
    if (length == 0)
      return;

    ResponseWriter writer = context.getResponseWriter();

    // start FOR SUBTRAIN
    // If subTrain, add a row and on the first and last cells, render
    // a border which looks like a sub-train
    if (train.subTrain)
    {
      _renderSubTrainRow(context, train, length, writer);

      writer.startElement("tr", null);
      writer.startElement("td", null);
      writer.endElement("td");
    }
    else
    {
      writer.startElement("tr", null);
    }

    // loop through each rendered station.

   int lastTrainIndex = train.startIndex + getMaxLinks(context, node);
    if (length <= lastTrainIndex)
      lastTrainIndex = length;
    else
      lastTrainIndex++; // length of train is larger than the visible
                        // number of train stations, so make room for the more
                        // by adding one to the lastTrainIndex.

    int currVisChildIndex =  Math.max(0, train.startIndex - 1);
    boolean isPrevVisChildDisabled = false;
    boolean isCurrVisChildDisabled = false;
    boolean isNextVisChildDisabled = false;

    component.setRowIndex(currVisChildIndex);
    isCurrVisChildDisabled = getBooleanAttributeValue(context,
                                                    stamp,
                                                    DISABLED_ATTR,
                                                    false);
    for (; currVisChildIndex < lastTrainIndex; currVisChildIndex++)
    {
      //
      // get index of the child and
      // determine if it is within the range in which it will be rendered.
      //
      int prevVisChildIndex = ((currVisChildIndex == 0)
                               ? NO_CHILD_INDEX
                               : currVisChildIndex  - 1);
      int nextVisChildIndex = ((currVisChildIndex == length - 1)
                               ? NO_CHILD_INDEX
                               : currVisChildIndex  + 1);

      component.setRowIndex(nextVisChildIndex);
      isNextVisChildDisabled = getBooleanAttributeValue(context,
                                                      stamp,
                                                      DISABLED_ATTR,
                                                      false);

      component.setRowIndex(currVisChildIndex);

      // initialized state of the station
      ProcessTrainRenderer.StationState station = train.station;
      initializeStationState(
         context,
         train,
         station,
         currVisChildIndex,
         prevVisChildIndex,
         nextVisChildIndex,
         isCurrVisChildDisabled,
         isPrevVisChildDisabled,
         isNextVisChildDisabled
         );

      // set up for next pass
      isPrevVisChildDisabled = isCurrVisChildDisabled;
      isCurrVisChildDisabled = isNextVisChildDisabled;

      Object label = stamp.getAttributeValue(context, TEXT_ATTR);

      String currVisChildText = null;

      // Get text from link, or Previous or More text if appropriate
      currVisChildText = getTextForStation(context, station, label);

      String currVisChildID = null;

      component.setRowIndex(currVisChildIndex);
      renderLink(context,
                 stamp,
                 writer,
                 train,
                 currVisChildText,
                 currVisChildID,
                 station);

    }

    if (train.subTrain)
    {
      writer.startElement("td", null);
      writer.endElement("td");
    }

    writer.endElement("tr");
  }



  /**
   * Renders the link under the train node
   *
   */
  protected void renderLink(
    RenderingContext context,
    UINode           stamp,
    ResponseWriter   writer,
    ProcessTrainRenderer.TrainState train,
    String           currVisChildText,
    String           currVisChildID,
    ProcessTrainRenderer.StationState station
    ) throws IOException
  {


    //
    // Write the link under the train node.
    //
    writer.startElement("td", null);
    writer.writeAttribute("colspan", "2", null);

    String styleClass =  (station.isSelected)
                          ? AF_PROCESS_TRAIN_ACTIVE_STYLE_CLASS
                          : (station.isDisabled && !station.isMoreLink)
                          ? AF_PROCESS_TRAIN_DISABLED_STYLE_CLASS
                          : (station.isVisited)
                          ? AF_PROCESS_TRAIN_VISITED_STYLE_CLASS
                          : AF_PROCESS_TRAIN_UNVISITED_STYLE_CLASS;

    renderStyleClassAttribute(context, styleClass);

    Map originalResourceKeyMap = context.getSkinResourceKeyMap();
    try
    {
      context.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      renderChild(context, stamp);
    }
    finally
    {
      context.setSkinResourceKeyMap(originalResourceKeyMap);
    }
/*
    // if there should be a link, render it
    // If the processTrain is read-only, then don't render the link, but
    // still render the text.
    if (!train.readOnly && (
        station.isPreviousLink ||
        (station.isMoreLink&&station.isVisited) ||
        (!station.isSelected && !station.isDisabled &&
        (station.isVisited || station.isNext) )))
    {

      UINode currLink = null;

      // If we don't support navigation (e.g., printable pages),
      // lie and claim we support scripting (even though we probably don't).
      // This will give us the highest fidelity output - that is,
      // we avoid creating submit buttons.

      if (supportsScripting(context) ||
          !supportsNavigation(context))
      {

        String destination =
          ProcessUtils.getSubmitScriptCall(
              context,
              train.formName,
              train.eventKey,
              train.sourceKey,
              train.id,
              train.valueKey,
              station.index,
              train.sizeKey,
              (station.index > train.selectedIndex)
              ? 1 : 0,// 0 means back, 1 means next
              false, null, null);

        currLink = _createSingleItemLink(
                      currVisChildText,
                      currVisChildID,
                      destination);
      }
      else
      {
        currLink= ProcessUtils.createSubmitButton(
                                          context,
                                          currVisChildText,
                                          null,
                                          currVisChildID,
                                          train.formName,
                                          false,
                                          train.eventKey,
                                          train.sourceKey,
                                          train.id,
                                          train.valueKey,
                                          station.index,
                                          train.sizeKey,
                                          (station.index > train.selectedIndex)
                                          ? 1 : 0);

      }


      currLink.render(context);
    }
    else
    {
      writer.startElement(SPAN_ELEMENT, null);
      renderID(context, currVisChildID, false);
      writer.writeText(currVisChildText, null);
      writer.endElement(SPAN_ELEMENT);
    }*/

    writer.endElement("td");
  }


  /**
  * return the string to use for the text of the station
  * it is the text of the link or "Previous" or "More"
  */
  protected String getTextForStation(RenderingContext context,
                                    ProcessTrainRenderer.StationState station,
                                    Object textObj)
  {
    String textValue = (textObj == null) ? null : textObj.toString();
    final String currText;

    if (textValue != null && !station.isPreviousLink && !station.isMoreLink)
    {
      // if we are in screen reader mode, then we must render more descriptive
      // text.
      // see bug 1801348 REMOVE ONE SET OF TRAIN TEXT IN ACCESSIBLE MODE
      if (isScreenReaderMode(context))
      {
        currText = _getDisabledUserText(context, station, textValue);
      }
      else
      {
        currText = textValue;
      }
    }
    else if(station.isPreviousLink)
    {
      currText = getTranslatedString(context, _PREVIOUS_KEY);
    }
    else if(station.isMoreLink)
    {
      currText = getTranslatedString(context, _MORE_KEY);
    }
    else
      currText = null;

    return currText;
  }

  /**
  * return the string to use for the text of the station
  * it is the text of the link or "Previous" or "More"
  */
  protected static String getIDForStation(
    RenderingContext context,
    ProcessTrainRenderer.StationState station,
    Object idValue)
  {

    if (idValue != null && !station.isPreviousLink && !station.isMoreLink)
    {
      return idValue.toString();
    }

    return null;
  }

  private String _getDisabledUserText(RenderingContext context,
                                      ProcessTrainRenderer.StationState station,
                                      String textString)
  {
    String altTextKey = station.isSelected
      ? _ACTIVE_KEY
      : station.isVisited
      ? _VISITED_KEY
      : _NEXT_KEY;

    String[] parameters = new String[]
    {
      textString
    };

    String altText = formatString(context,
                                  getTranslatedString(context, altTextKey),
                                  parameters);
    return altText;
  }

  protected String getAltText(
    RenderingContext context,
    ProcessTrainRenderer.StationState station,
    String textString)
  {
    // if we are in screen reader mode, then we must not render any alt text
    // on the image. However, we can't return null since OAC will
    // complain, so return "":
    // see bug 1801348 REMOVE ONE SET OF TRAIN TEXT IN ACCESSIBLE MODE

    return isInaccessibleMode(context)
      ? null
      : isScreenReaderMode(context)
      ? ""
      : _getDisabledUserText(context, station, textString);
  }

  /**
  * Returns the max number of links to show
  */
  protected int getMaxLinks(
    RenderingContext context,
    UINode           node
    )
  {
    return _MAX_NUM_LINK_INDEX;
  }

  /**
  * Initialize the station state
  */
  protected void initializeStationState(
    RenderingContext context,
    ProcessTrainRenderer.TrainState train,
    ProcessTrainRenderer.StationState station,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    boolean          isCurrChildDisabled,
    boolean          isPrevChildDisabled,
    boolean          isNextChildDisabled
)
  {


    station.isPreviousLink = false;
    station.isMoreLink = false;
    station.isDisabled = false;
    station.isNextDisabled = false;
    station.isPrevDisabled = false;
    station.index = currVisChildIndex;

    // train.startIndex is the index into the List that is the
    // start of the train. The algorithm is dependent upon the BLAF spec.
    if( currVisChildIndex == train.startIndex-1)
    {
      station.isPreviousLink = true;
    }
    else if (currVisChildIndex == train.startIndex+_MAX_NUM_LINK_INDEX)
    {
      station.isMoreLink = true;
    }

    // selected nodes cannot be disabled,
    // so don't bother getting disabled attribute for the selected node

    if (currVisChildIndex != NO_CHILD_INDEX &&
        currVisChildIndex != train.selectedIndex)
    {
      station.isDisabled = isCurrChildDisabled;
    }


    // get disabled information about the previous and the next child.
    // selectedIndex cannot act disabled
    //
    if (prevVisChildIndex != NO_CHILD_INDEX &&
        prevVisChildIndex != train.selectedIndex)
    {
      station.isPrevDisabled =  isPrevChildDisabled;
    }

    if (nextVisChildIndex != NO_CHILD_INDEX &&
        nextVisChildIndex != train.selectedIndex)
    {
      station.isNextDisabled = isNextChildDisabled;

    }

    //
    // get the selected and visited flags for our node
    //
    station.isSelected = (currVisChildIndex == train.selectedIndex);
    station.isVisited  = (currVisChildIndex <= train.maxVisitedIndex);
    station.isNextVisited = (currVisChildIndex < train.maxVisitedIndex);
    station.isNext = (currVisChildIndex == (train.maxVisitedIndex+1));
    // if previous station is "next", and disabled, mark this as "next".
    if ((currVisChildIndex-1 == (train.maxVisitedIndex+1)) &&
        (station.isPrevDisabled))
    {
      station.isNext = true;
    }
  }

  protected static class TrainState
  {
    public TrainState()
    {
      station = new ProcessTrainRenderer.StationState();
    }
    public int startIndex;
    public int maxVisitedIndex;
    public int selectedIndex;
    public boolean subTrain;
    public boolean readOnly;
    public String formName;
    public String id;
    public String eventKey;
    public String sourceKey;
    public String valueKey;
    public String sizeKey;
    public ProcessTrainRenderer.StationState station;
  }

  protected static class StationState
  {

    public boolean isSelected;
    // is this the station that is right AFTER the selected station.
    public boolean isNext;
    public boolean isVisited;// has this station been visited already?
    public boolean isPreviousLink; // is this the Previous link?
    public boolean isMoreLink;  // is this the More link?
    public boolean isDisabled; // is this station disabled?
    public boolean isNextDisabled; // is the next station disabled?
    public boolean isPrevDisabled;// is the previous station disabled?
    public boolean isNextVisited; // is the next station visited?
    public int index; // the index of this node
  } //end StationState

  private void _renderSubTrainRow(
    RenderingContext         context,
    ProcessTrainRenderer.TrainState train,
    int                      length,
    ResponseWriter           writer
    ) throws IOException
  {
    boolean isRTL = isRightToLeft(context);

    writer.startElement("tr", null);

    if (isRTL)
      _renderSubTrainCell(context, TRAIN_SUB_RIGHT_STYLE_CLASS, writer);
    else
      _renderSubTrainCell(context, AF_PROCESS_TRAIN_SUB_START_STYLE_CLASS, writer);

    _renderSubTrainBlankCells(train, length, writer);

    if (isRTL)
      _renderSubTrainCell(context, AF_PROCESS_TRAIN_SUB_START_STYLE_CLASS, writer);
    else
      _renderSubTrainCell(context, TRAIN_SUB_RIGHT_STYLE_CLASS, writer);

    writer.endElement("tr");
  }

  private void _renderSubTrainCell(
    RenderingContext context,
    String           style,
    ResponseWriter   writer
    ) throws IOException
  {
    writer.startElement("td", null);
    renderStyleClassAttribute(context, style);
    renderSpacer(context, "14", "2");
    writer.endElement("td");
  }
  /**
   * renders a td with colSpan equal to the number of visible stations
   * including the Previous and More if they are there.
   * @param context
   * @param length
   * @param output
   * @param train
   * @throws IOException
   */
  private void _renderSubTrainBlankCells(
    ProcessTrainRenderer.TrainState train,
    int                      length,
    ResponseWriter           writer
    ) throws IOException
  {
    writer.startElement("td", null);

    // figure out the number of stations
    int startIndex = Math.max(0, train.startIndex - 1);
    int lastTrainIndex = train.startIndex + _MAX_NUM_LINK_INDEX;
    if (length <= lastTrainIndex)
      lastTrainIndex = length;
    else
    {
      // when the length of train is larger than the visible
      // number of train stations, we render a More link.
      // so make room for the more
      // by adding one to the lastTrainIndex.
      lastTrainIndex++;
    }
    String numberOfStations = Integer.toString((lastTrainIndex - startIndex)*2);
    writer.writeAttribute("colspan", numberOfStations, null);
    writer.endElement("td");
  }



  private static final int _MAX_NUM_LINK_INDEX = 6; //number of visible links

  /**
   * The following keys are used to get at the corresponding translated
   * strings.
   */
  private static final String _VISITED_KEY  = "af_processTrain.VISITED_TIP";
  private static final String _ACTIVE_KEY   = "af_processTrain.ACTIVE_TIP";
  private static final String _NEXT_KEY     = "af_processTrain.NEXT_TIP";
  private static final String _MORE_KEY     = "af_processTrain.MORE";
  private static final String _PREVIOUS_KEY = "af_processTrain.PREVIOUS";


  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ProcessTrainRenderer.class);

  // for now keep the OraLink/OraDisabledLink styles on the 'a', and
  // append train link style class.
  private static final Map _RESOURCE_KEY_MAP  =  new HashMap();
  private static final String _TRAIN_DISABLED_LINK =
    XhtmlConstants.LINK_DISABLED_STYLE_CLASS +
    " " + XhtmlConstants.AF_PROCESS_TRAIN_LINK_STYLE_CLASS;
  private static final String _TRAIN_ENABLED_LINK =
    XhtmlConstants.LINK_STYLE_CLASS +
    " " + XhtmlConstants.AF_PROCESS_TRAIN_LINK_STYLE_CLASS;
  static
  {
    _RESOURCE_KEY_MAP.put(
      XhtmlConstants.LINK_DISABLED_STYLE_CLASS,
      _TRAIN_DISABLED_LINK);
    _RESOURCE_KEY_MAP.put(
      XhtmlConstants.LINK_STYLE_CLASS,
      _TRAIN_ENABLED_LINK);
  }

}
