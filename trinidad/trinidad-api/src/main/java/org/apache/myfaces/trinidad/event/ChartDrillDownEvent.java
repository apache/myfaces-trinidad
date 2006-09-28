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
package org.apache.myfaces.trinidad.event;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;

import org.apache.myfaces.trinidad.model.SortCriterion;

/**
 * The Event generated when a Collection is to be sorted.
 * @author The Oracle ADF Faces Team
 */
public final class ChartDrillDownEvent extends FacesEvent
{
  public ChartDrillDownEvent(
    UIComponent source, 
    int[] seriesIndices, 
    int[] yValueIndices, 
    double[] yValues, 
    double[] xValues)
  {
    super(source);
    _seriesIndices = seriesIndices;
    _yValueIndices = yValueIndices;
    _yValues = yValues;
    _xValues = xValues;
  }


  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof ChartDrillDownListener);
  }

  @Override
  public void processListener(FacesListener listener)
  {
    ((ChartDrillDownListener) listener).processChartDrillDown(this);
  }
  
  /**
   * returns an array of series indices where the user clicked.
   * In most cases only index 0 is applicable. Currently multiple indices
   * are applicable only to areaChart and radarArea chart types.
   * 
   * @return the array of series indices as an int array
   */
  public int[] getSeriesIndices()
  {
    return _seriesIndices;
  }

  /**
   * returns an array of yvalue indices where the user clicked.
   * In most cases only index 0 is applicable. Currently multiple indices
   * are applicable only to areaChart and radarArea chart types.
   * This parameter might be null for charts where the interpolated value is
   * computed(for e.g. area, line etc.)
   * 
   * @return the array of yvalue indices as an int array
   */
  public int[] getYValueIndices()
  {
    return _yValueIndices;
  }

  /**
   * returns an array of yvalues where the user clicked.
   * The values in this event might be different from the
   * data set used for displaying the chart since it might represent
   * interpolated values (for e.g. area, line etc.)
   * 
   * @return the array of yvalues as a float array
   */
  public double[] getYValues()
  {
    return _yValues;
  }

  /**
   * returns an array of xvalues where the user clicked. This is currently 
   * only applicable to XYLine and scatterPlot
   * The values in this event might be different from the
   * data set used for displaying the chart since it might represent
   * interpolated values (XYLine)
   * 
   * @return the array of xvalues as a float array
   */
  public double[] getXValues()
  {
    return _xValues;
  }
  
  private final int[] _seriesIndices;
  private final int[] _yValueIndices; 
  private final double[] _yValues; 
  private final double[] _xValues;

}
