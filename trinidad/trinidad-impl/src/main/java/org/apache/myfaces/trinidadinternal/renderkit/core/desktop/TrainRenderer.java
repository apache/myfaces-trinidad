/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.renderkit.core.desktop;


import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXProcess;
import org.apache.myfaces.trinidad.component.core.nav.CoreTrain;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.renderkit.FormData;
import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ProcessUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * Renderer for process train components
 * * <p>
 */
public class TrainRenderer
  extends XhtmlRenderer
{
  /**
   * Constructor.
   */
  public TrainRenderer()
  {
    super(CoreTrain.TYPE);
  }
  
  /**
   */
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> requestMap = 
      context.getExternalContext().getRequestParameterMap();

    Object event = requestMap.get(XhtmlConstants.EVENT_PARAM);

    if ((event != null) && event.equals(XhtmlConstants.GOTO_EVENT))
    {
      Object source = requestMap.get(XhtmlConstants.SOURCE_PARAM);

      if (source != null && source.equals(component.getClientId(context)))
      {

        Object valueObject = requestMap.get(XhtmlConstants.VALUE_PARAM);

        // we piggyback on the size parameter.
        // 0 means we are moving to a previous step, 1 means we are
        // moving to the next step.
        Object sizeObject = requestMap.get(XhtmlConstants.SIZE_PARAM);

        if (valueObject != null)
        {
          int value = -1;

          try
          {
            value = Integer.parseInt(valueObject.toString());
          }
          catch (NumberFormatException nfe)
          {
            _LOG.severe(nfe);
          }

          int size = 0;

          try
          {
            size = Integer.parseInt(sizeObject.toString());
          }
          catch (NumberFormatException nfe)
          {
            _LOG.warning(nfe);
          }

          if (size < 0)
            size = 0;

          if (value >= 0)
          {
            UIXProcess process = (UIXProcess) component;
            Object oldPath = process.getRowKey();
            Object focusPath = process.getFocusRowKey();
            process.setRowKey(focusPath);
            UIComponent stamp = process.getNodeStamp();
            int index = process.getRowIndex();

            if (size == 0)
            {
              index = ProcessUtils.getBackIndex(process, stamp, index);
            }
            else
            {
              index = ProcessUtils.getNextIndex(process, stamp, index);
            }

            process.setRowIndex(index);
            new ActionEvent(stamp).queue();
            process.setRowKey(oldPath);
          }
        }
      }
    }
  }
  
  /**
   * @return
   */
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext context, 
    RenderingContext arc, 
    UIComponent component, 
    FacesBean bean)
    throws IOException
  {
    UIXProcess process = (UIXProcess) component;
    UIComponent stamp = process.getNodeStamp();

    if (stamp != null)
    {
      Object oldPath = process.getRowKey();
      Object newPath = null;
      boolean isNewPath = _setNewPath(process);
      if (isNewPath)
      {
        ResponseWriter writer = context.getResponseWriter();

        TrainRenderer.TrainState trainState = 
        _getTrainState(context, arc, process);
        _renderHiddenFields(context, arc, trainState);

        writer.startElement("table", component);
        OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
        writer.writeAttribute("align", "center", null);
        newPath = process.getRowKey();
        process.setRowKey(oldPath);
        renderId(context, component);
        process.setRowKey(newPath);
        renderAllAttributes(context, arc, bean);

        int length = process.getRowCount();

        if (length == 0)
          return;

        _encodeChildren(context, arc, process, stamp, trainState, length);

        writer.endElement("table");

        process.setRowKey(oldPath);
      }
    }
  }

  /**
   * This is how we can render both the user defined styleClass and our
   * component style class
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext context, 
    RenderingContext arc, 
    FacesBean bean)
    throws IOException
  {
    renderStyleAttributes(context, arc, bean, 
                          SkinSelectors.AF_PROCESS_TRAIN_STYLE_CLASS);
  }


  /**
  * Initialize the station state
  */
  private void _initializeStationState(
    FacesContext context, 
    RenderingContext arc, 
    TrainRenderer.TrainState train, 
    TrainRenderer.StationState station, 
    int currVisChildIndex, 
    int prevVisChildIndex, 
    int nextVisChildIndex, 
    boolean isCurrChildDisabled, 
    boolean isPrevChildDisabled, 
    boolean isNextChildDisabled)
  {
    station.isPreviousLink = false;
    station.isMoreLink = false;
    station.isDisabled = false;
    station.isNextDisabled = false;
    station.isPrevDisabled = false;
    station.index = currVisChildIndex;

    // train.startIndex is the index into the List that is the
    // start of the train. The algorithm is dependent upon the BLAF spec.
    if (currVisChildIndex == train.startIndex - 1)
    {
      station.isPreviousLink = true;
    }
    else if (currVisChildIndex == train.startIndex + _MAX_NUM_LINK_INDEX)
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
      station.isPrevDisabled = isPrevChildDisabled;
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
    station.isVisited = (currVisChildIndex <= train.maxVisitedIndex);
    station.isNextVisited = (currVisChildIndex < train.maxVisitedIndex);
    station.isNext = (currVisChildIndex == (train.maxVisitedIndex + 1));
    // if previous station is "next", and disabled, mark this as "next".
    if ((currVisChildIndex - 1 == (train.maxVisitedIndex + 1)) && 
        (station.isPrevDisabled))
    {
      station.isNext = true;
    }
  }

  /**
   * Returns the MAX_VISITED_ATTR
   * @todo =-=jmw Hopefully the controller will tell us this someday.
   */
  private static Object _getMaxVisited(
    RenderingContext arc, 
    UIComponent component)
  {
    // return component.getAttributes().get("maxVisited");
    return null;
  }

  /**
   * Get the maxVisited attribute from the node and return it.
   */
  private int _getMaxVisitedIndex(
    RenderingContext arc, 
    UIComponent component)
  {
    int maxVisitedIndex = NO_CHILD_INDEX;
    Integer maxVisited = (Integer) _getMaxVisited(arc, component);
    if (maxVisited != null)
    {
      maxVisitedIndex = maxVisited.intValue();
    }
    return maxVisitedIndex;
  }

  /**
   * Return what the starting index into the stations List.
   */
  private int _getStartIndex(int numPages, int originalSelectedIndex)
  {
    int currentMinIndex = 0;

    if (numPages <= _MAX_NUM_LINK_INDEX)
      return currentMinIndex;

    int selectedIndex = originalSelectedIndex;
    int currentMaxIndex = _MAX_NUM_LINK_INDEX - 1;

    if (selectedIndex < currentMaxIndex)
    {
      return currentMinIndex; //0
    }

    // the algorithm below works, but I thought it was too cryptic
    // return (selectedIndex -
    //            (((selectedIndex-1)%(_MAX_NUM_LINK_INDEX-2))+1));

    // loop until the selectedIndex range is found or
    // we have gone past the number of nodes in the train.
    // Then we'll know what index to start the visible portion of the train.
    while (numPages > currentMaxIndex)
    {
      currentMinIndex = currentMaxIndex - 1;
      currentMaxIndex += (_MAX_NUM_LINK_INDEX - 2);
      if (selectedIndex > currentMinIndex && 
          selectedIndex < currentMaxIndex)
        return currentMinIndex;
    }

    return currentMinIndex;
  }


  /**
   * Gather up the train state: selectedIndex, maxVisitedIndex, startIndex,
   *  isSubTrain, formName, id,.
   *  This way all the parameters we need to pass around to various methods
   *  are all in one place, the TrainState
   * @param arc RenderingContext
   * @param context FacesContext
   * @param process the UIXProcess component.
   * @return
   */
  private TrainState _getTrainState(
    FacesContext context, 
    RenderingContext arc, 
    UIXProcess process)
  {
    TrainRenderer.TrainState state = 
      new TrainRenderer.TrainState();

    state.selectedIndex = process.getRowIndex();

    // get highest node in train visited
    state.maxVisitedIndex = _getMaxVisitedIndex(arc, process);

    // default to selectedIndex if it wasn't set
    // or if it was set to be greater than selectedIndex
    if (state.maxVisitedIndex == NO_CHILD_INDEX || 
        state.maxVisitedIndex < state.selectedIndex)
    {
      state.maxVisitedIndex = state.selectedIndex;
    }

    int totalPages = process.getRowCount();
    state.startIndex = _getStartIndex(totalPages, state.selectedIndex);

    state.subTrain = _isSubTrain(process);

    state.formName = arc.getFormData().getName();

    String id = process.getClientId(context);
    state.id = (id != null)? id: null;

    return state;
  }


  /**
   * Get the subTrain attribute from the node and return it.
   */
  private boolean _isSubTrain(UIXProcess component)
  {
    Object focusRowKey = component.getFocusRowKey();
    if (focusRowKey != null && (component.getDepth(focusRowKey) > 0))
      return true;

    return false;
  }


  private void _encodeChildren(
    FacesContext context, 
    RenderingContext arc, 
    UIXProcess process, 
    UIComponent stamp, 
    TrainState train, 
    int length)
    throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();

    // start FOR SUBTRAIN
    // If subTrain, add a row and on the first and last cells, render
    // a border which looks like a sub-train
    if (train.subTrain)
    {
      _renderSubTrainRow(context, arc, train, length, writer);

      writer.startElement("tr", null);
      writer.startElement("td", null);
      writer.endElement("td");
    }
    else
    {
      writer.startElement("tr", null);
    }

    // loop through each rendered station.
    int lastTrainIndex = train.startIndex + _getMaxLinks(arc, process);

    if (length <= lastTrainIndex)
    {
      lastTrainIndex = length;
    }
    else
    {
      lastTrainIndex++; // length of train is larger than the visible
      // number of train stations, so make room for the more
      // by adding one to the lastTrainIndex.
    }
    int currVisChildIndex = Math.max(0, train.startIndex - 1);
    boolean isPrevVisChildDisabled = false;
    boolean isCurrVisChildDisabled = false;
    boolean isNextVisChildDisabled = false;

    process.setRowIndex(currVisChildIndex);

    isCurrVisChildDisabled = 
        Boolean.TRUE.equals(stamp.getAttributes().get("disabled"));
    // getBooleanAttributeValue(context, stamp, DISABLED_ATTR, false);

    for (; currVisChildIndex < lastTrainIndex; currVisChildIndex++)
    {
      //
      // get index of the child and
      // determine if it is within the range in which it will be rendered.
      //
      int prevVisChildIndex = 
        ((currVisChildIndex == 0)? NO_CHILD_INDEX: currVisChildIndex - 1);
      int nextVisChildIndex = 
        ((currVisChildIndex == length - 1)? NO_CHILD_INDEX: 
         currVisChildIndex + 1);

      process.setRowIndex(nextVisChildIndex);

      isNextVisChildDisabled = 
          Boolean.TRUE.equals(stamp.getAttributes().get("disabled"));

      process.setRowIndex(currVisChildIndex);

      // initialized state of the station
      TrainRenderer.StationState station = train.station;
      _initializeStationState(context, arc, train, station, 
                             currVisChildIndex, prevVisChildIndex, 
                             nextVisChildIndex, isCurrVisChildDisabled, 
                             isPrevVisChildDisabled, 
                             isNextVisChildDisabled);

      // set up for next pass
      isPrevVisChildDisabled = isCurrVisChildDisabled;
      isCurrVisChildDisabled = isNextVisChildDisabled;

      Object label = stamp.getAttributes().get("text");

      String currVisChildText = null;

      // Get text from link, or Previous or More text if appropriate
      currVisChildText = _getTextForStation(arc, station, label);

      String currVisChildID = null;

      process.setRowIndex(currVisChildIndex);
      _renderLink(context, arc, stamp, writer, train, currVisChildText, 
                 currVisChildID, station);

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
  private void _renderLink(
    FacesContext context, 
    RenderingContext arc, 
    UIComponent stamp, 
    ResponseWriter writer, 
    TrainRenderer.TrainState train, 
    String currVisChildText, 
    String currVisChildID, 
    TrainRenderer.StationState station)
    throws IOException
  {
    //
    // Write the link under the train node.
    //
    writer.startElement("td", null);
    writer.writeAttribute("colspan", "2", null);

    String styleClass = 
      (station.isSelected)? SkinSelectors.AF_PROCESS_TRAIN_ACTIVE_STYLE_CLASS: 
      (station.isDisabled && !station.isMoreLink)? 
      SkinSelectors.AF_PROCESS_TRAIN_DISABLED_STYLE_CLASS: 
      (station.isVisited)? 
      SkinSelectors.AF_PROCESS_TRAIN_VISITED_STYLE_CLASS: 
      SkinSelectors.AF_PROCESS_TRAIN_UNVISITED_STYLE_CLASS;

    renderStyleClass(context, arc, styleClass);

    Map<String, String> originalResourceKeyMap = arc.getSkinResourceKeyMap();
    try
    {
      arc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      _renderStamp(context, stamp);
    }
    finally
    {
      arc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
    writer.endElement("td");
  }

  /**
   * Called to render a child.  This method does not update the
   * rendering context (by calling pushChild() and popChild()
   * as needed);  subclasses need to use renderIndexedChild() or
   * renderNamedChild() for that purpose.
   * <p>
   * @param context the faces context
   * @param child the child under consideration
   */
  private void _renderStamp(FacesContext context, UIComponent child)
    throws IOException
  {
    if (child != null)
    {
      encodeChild(context, child);
      // child.render(context);
    }
  }

  /**
  * return the string to use for the text of the station
  * it is the text of the link or "Previous" or "More"
  */
  private String _getTextForStation(
    RenderingContext arc, 
    TrainRenderer.StationState station, 
    Object textObj)
  {
    String textValue = (textObj == null)? null: textObj.toString();
    final String currText;

    if (textValue != null && !station.isPreviousLink && 
        !station.isMoreLink)
    {
      // if we are in screen reader mode, then we must render more descriptive
      // text.
      // see bug 1801348 REMOVE ONE SET OF TRAIN TEXT IN ACCESSIBLE MODE
      if (isScreenReaderMode(arc))
      {
        currText = _getDisabledUserText(arc, station, textValue);
      }
      else
      {
        currText = textValue;
      }
    }
    else if (station.isPreviousLink)
    {
      currText = arc.getTranslatedString(_PREVIOUS_KEY);
    }
    else if (station.isMoreLink)
    {
      currText = arc.getTranslatedString(_MORE_KEY);
    }
    else
      currText = null;

    return currText;
  }

  private String _getDisabledUserText(
    RenderingContext arc, 
    TrainRenderer.StationState station, 
    String textString)
  {
    String altTextKey = 
      station.isSelected? _ACTIVE_KEY: station.isVisited? _VISITED_KEY: 
                                       _NEXT_KEY;

    String[] parameters = new String[]
      { textString };

    String altText = 
      XhtmlUtils.getFormattedString(arc.getTranslatedString(altTextKey), 
                                    parameters);
    return altText;
  }

  /**
   * Returns the max number of links to show
   */
  private int _getMaxLinks(
    RenderingContext arc, 
    UIComponent component)
  {
    return _MAX_NUM_LINK_INDEX;
  }

  private void _renderSubTrainRow(
    FacesContext context, 
    RenderingContext arc, 
    TrainRenderer.TrainState train, 
    int length, 
    ResponseWriter writer)
    throws IOException
  {
    boolean isRTL = arc.getLocaleContext().isRightToLeft();

    writer.startElement("tr", null);

    if (isRTL)
      _renderSubTrainCell(context, arc, 
                          SkinSelectors.TRAIN_SUB_RIGHT_STYLE_CLASS, 
                          writer);
    else
      _renderSubTrainCell(context, arc, 
                          SkinSelectors.AF_PROCESS_TRAIN_SUB_START_STYLE_CLASS, 
                          writer);

    _renderSubTrainBlankCells(train, length, writer);

    if (isRTL)
      _renderSubTrainCell(context, arc, 
                          SkinSelectors.AF_PROCESS_TRAIN_SUB_START_STYLE_CLASS, 
                          writer);
    else
      _renderSubTrainCell(context, arc, 
                          SkinSelectors.TRAIN_SUB_RIGHT_STYLE_CLASS, 
                          writer);

    writer.endElement("tr");
  }

  private void _renderSubTrainCell(
    FacesContext context, 
    RenderingContext arc, 
    String style, 
    ResponseWriter writer)
    throws IOException
  {
    writer.startElement("td", null);
    renderStyleClass(context, arc, style);
    renderSpacer(context, arc, "14", "2");
    writer.endElement("td");
  }

  /**
   * renders a td with colSpan equal to the number of visible stations
   * including the Previous and More if they are there.
   * @param train
   * @param length
   * @param writer
   * @throws IOException
   */
  private void _renderSubTrainBlankCells(TrainRenderer.TrainState train, 
                                         int length, ResponseWriter writer)
    throws IOException
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
    String numberOfStations = 
      Integer.toString((lastTrainIndex - startIndex) * 2);
    writer.writeAttribute("colspan", numberOfStations, null);
    writer.endElement("td");
  }


  private void _renderHiddenFields(
    FacesContext context, 
    RenderingContext arc, 
    TrainState train)
    throws IOException
  {
    if ((train.formName != null) && supportsScripting(arc))
    {
      // render hidden fields to hold the form data
      FormData fData = arc.getFormData();
      if (fData != null)
      {
        fData.addNeededValue(XhtmlConstants.EVENT_PARAM);
        fData.addNeededValue(XhtmlConstants.SOURCE_PARAM);
        fData.addNeededValue(XhtmlConstants.VALUE_PARAM);
        fData.addNeededValue(XhtmlConstants.SIZE_PARAM);
      }

      // Render script submission code.
      ProcessUtils.renderNavSubmitScript(context, arc);
    }
  }


  private boolean _setNewPath(UIXProcess component)
  {
    Object focusPath = component.getFocusRowKey();
    component.setRowKey(focusPath);
    return true;
  }

  protected static class TrainState
  {
    public TrainState()
    {
      station = new TrainRenderer.StationState();
    }
    public int startIndex;
    public int maxVisitedIndex;
    public int selectedIndex;
    public boolean subTrain;
    public String formName;
    public String id;
    public TrainRenderer.StationState station;
  }

  protected static class StationState
  {

    public boolean isSelected;
    // is this the station that is right AFTER the selected station.
    public boolean isNext;
    public boolean isVisited; // has this station been visited already?
    public boolean isPreviousLink; // is this the Previous link?
    public boolean isMoreLink; // is this the More link?
    public boolean isDisabled; // is this station disabled?
    public boolean isNextDisabled; // is the next station disabled?
    public boolean isPrevDisabled; // is the previous station disabled?
    public boolean isNextVisited; // is the next station visited?
    public int index; // the index of this node
  } //end StationState

  private static final int  _MAX_NUM_LINK_INDEX = 
    6; //number of visible links

  /**
 * The following keys are used to get at the corresponding translated
 * strings.
 */
  private static final String _VISITED_KEY = "af_train.VISITED_TIP";
  private static final String _ACTIVE_KEY = "af_train.ACTIVE_TIP";
  private static final String _NEXT_KEY = "af_train.NEXT_TIP";
  private static final String _MORE_KEY = "af_train.MORE";
  private static final String _PREVIOUS_KEY = "af_train.PREVIOUS";

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(TrainRenderer.class);

  // for now keep the OraLink/OraDisabledLink styles on the 'a', and
  // append train link style class.
  private static final Map<String, String> _RESOURCE_KEY_MAP = 
    new HashMap<String, String>();
  private static final String _TRAIN_DISABLED_LINK = 
    SkinSelectors.LINK_DISABLED_STYLE_CLASS + " " + 
    SkinSelectors.AF_PROCESS_TRAIN_LINK_STYLE_CLASS;
  private static final String _TRAIN_ENABLED_LINK = 
    SkinSelectors.LINK_STYLE_CLASS + " " + 
    SkinSelectors.AF_PROCESS_TRAIN_LINK_STYLE_CLASS;
  
  static {
    _RESOURCE_KEY_MAP.put(SkinSelectors.LINK_DISABLED_STYLE_CLASS, 
                          _TRAIN_DISABLED_LINK);
    _RESOURCE_KEY_MAP.put(SkinSelectors.LINK_STYLE_CLASS, 
                          _TRAIN_ENABLED_LINK);
  }

}
