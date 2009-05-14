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
package org.apache.myfaces.trinidaddemo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.io.StringWriter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.data.CoreChart;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ChartDrillDownEvent;
import org.apache.myfaces.trinidad.model.ChartModel;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;


/**
 * Managed bean for chat component demos.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/ChartBean.java#1 $) $Date: 16-aug-2005.15:12:27 $
 */
public class ChartBean implements Serializable
{
  public ChartModel getValue()
  {
    return _chartModel;
  }
  
  public ComponentEditorHandler getEditor()
  {
    return _editorBean;
  }
  
  public String updatePartial()
  {
    _editorBean.update();
    RequestContext afContext = RequestContext.getCurrentInstance();
    afContext.addPartialTarget(_editorBean.getComponent());
    return null;
  }
  
  public String nextChartType()
  {
    _currentTypeIndex++;
    if(_currentTypeIndex >= _chartTypes.length)
      _currentTypeIndex = 0;
    CoreChart chart = (CoreChart)_editorBean.getComponent();
    chart.setType(_chartTypes[_currentTypeIndex]);
    return null;
  }
  
  public boolean isLargerDataSet()
  {
    return _largerDataSet;
  }

  public void setLargerDataSet(boolean b)
  {
    _largerDataSet = b;
    CoreChart chart = (CoreChart)_editorBean.getComponent();
    if(_largerDataSet)
      chart.setXMajorGridLineCount(1);
    else
      chart.setXMajorGridLineCount(-1);
  }
  
  public void drillDown(ChartDrillDownEvent event)
  {
    FacesContext context = FacesContext.getCurrentInstance();

    String message =  "seriesIndices : " + _getEventIntsAsString(event.getSeriesIndices()) +
        " yValueIndices : " + _getEventIntsAsString(event.getYValueIndices()) +
        " yValues :" + _getEventDoublesAsString(event.getYValues()) +
        " xValues :" + _getEventDoublesAsString(event.getXValues());
    ExtendedRenderKitService erks = 
      Service.getRenderKitService(context,
                                  ExtendedRenderKitService.class);
    erks.addScript(context,
                   "alert('Confirmation from ChartBean: " + 
                   message +
                   "');");
  }
  
  private String _getEventIntsAsString(int[] intArray)
  {
    if(intArray!=null)
    {
      StringWriter sw = new StringWriter();
      for(int i=0; i<intArray.length; ++i)
      {
        if(i!=0)
          sw.append(',');
        sw.append(String.valueOf(intArray[i]));
      }
      return sw.toString();
    }
    else
      return null;
  }
  
  private String _getEventDoublesAsString(double[] intArray)
  {
    if(intArray!=null)
    {
      StringWriter sw = new StringWriter();
      for(int i=0; i<intArray.length; ++i)
      {
        if(i!=0)
          sw.append(',');
        sw.append(String.valueOf(intArray[i]));
      }
      return sw.toString();
    }
    else
      return null;
  }


  private class MyChartModel extends ChartModel implements Serializable
  {
    @Override
    public List<String> getSeriesLabels()
    {
      if(_largerDataSet)
        return _largeSeriesLabels;
      else
        return _seriesLabels;
    }

    @Override
    public List<String> getGroupLabels()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()) || 
         !_largerDataSet)
        return _groupLabels;
      else
        return _largeGroupLabels;
    }
        
    @Override
    public List<List<Double>> getXValues()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("XYLine".equals(chart.getType()) || "scatterPlot".equals(chart.getType()))
      {
        if(_largerDataSet)
        {
          if(_largerXValues == null)
          {
            _largerXValues = new ArrayList<List<Double>>();
            _loadLargerValues(_largerXValues, "/components/chartData/NasdaqXData.txt");
          }
          return _largerXValues;
        }
        return _chartXValues;
      }
      else
        return null;
    }
  
    @Override
    public List<List<Double>> getYValues()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()))
        return _dialchartYValues;
      else
      {
        if(_largerDataSet)
        {
          if(_largerYValues == null)
          {
            _largerYValues = new ArrayList<List<Double>>();
            _loadLargerValues(_largerYValues, "/components/chartData/NasdaqData.txt");
          }
          return _largerYValues;
        }
        return _chartYValues;
      }
    }
  
    private void _loadLargerValues(ArrayList<List<Double>> valuesList, String file)
    {
      FacesContext context = FacesContext.getCurrentInstance();
      InputStream in = context.getExternalContext().getResourceAsStream(file);
      if(in != null)
      {
        
        BufferedReader rd
                  = new BufferedReader(new InputStreamReader(in));
        
        String line;
        try
        {
          while((line = rd.readLine())!=null)
          {
            String[] strings = line.split("\t");
            ArrayList<Double> values = new ArrayList<Double>(strings.length);
            for(String s:strings)
            {
              values.add(Double.parseDouble(s));
            }
            valuesList.add(values);
          }         
        }
        catch(IOException ioe)
        {
          System.out.println("error reading chart data");
        }
      }
    }

    @Override
    public Double getMaxYValue()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()))
        return 200.0;
      else if("stackedVerticalBar".equals(chart.getType()) || 
         "stackedHorizontalBar".equals(chart.getType()) ||
         "stackedArea".equals(chart.getType()))
      {
        if(_largerDataSet)
          return 7500.0;
        else
          return 500000.0;
      }
      else
      {
        if(_largerDataSet)
          return 2500.0;
        else
          return 200000.0;
      }  
    }
  
    @Override
    public Double getMinYValue()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if(!"circularGauge".equals(chart.getType()) && 
         !"semiCircularGauge".equals(chart.getType()) &&
         !"stackedVerticalBar".equals(chart.getType()) && 
         !"stackedHorizontalBar".equals(chart.getType()) &&
         !"stackedArea".equals(chart.getType()) &&
         _largerDataSet)
      {
          return 2000.0;
      }
      return 0.0; 
    }
  
    @Override
    public Double getMaxXValue()
    {
      if(_largerDataSet)
        return 54.0;
      else
        return 10.0; 
    }
  
    @Override
    public Double getMinXValue()
    {
      if(_largerDataSet)
        return 0.0;
      else
        return 6.0; 
    }
  
    @Override
    public String getTitle()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()))
        return "Values in 1000s";
      else
      {
        if(_largerDataSet)
          return "NASDAQ Q4 2005 - Q3 2006";
        else
          return "Title";
      }
    }
  
    @Override
    public String getSubTitle()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()) || _largerDataSet)
        return null;
      else
        return "SubTitle"; 
    }
  
    @Override
    public String getFootNote()
    {
      CoreChart chart = (CoreChart)_editorBean.getComponent();
      if("circularGauge".equals(chart.getType()) || 
         "semiCircularGauge".equals(chart.getType()) || _largerDataSet)
        return null;
      else
        return "FootNote"; 
    }
    
    private final List<String> _groupLabels = 
      Arrays.asList(new String[]{"June", "July", "August", "September","October"});

    private final List<String> _largeGroupLabels = 
      Arrays.asList(new String[]{"Q4-2005", "Q1-2006", "Q2-2006", "Q3-2006"});
  
    private final List<String> _seriesLabels = 
      Arrays.asList(new String[]{"Previous", "Target", "Actual"});
    
    private final List<String> _largeSeriesLabels = 
      Arrays.asList(new String[]{"Opening", "Low", "High"});
      
    private final ArrayList<List<Double>> _chartYValues;
    private final ArrayList<List<Double>> _chartXValues; 
    private final ArrayList<List<Double>> _dialchartYValues;
    private ArrayList<List<Double>> _largerYValues;
    private ArrayList<List<Double>> _largerXValues;
    {
      _chartYValues = new ArrayList<List<Double>>();
      _chartYValues.add(Arrays.asList(new Double[]{135235.0, 155535.0, 141725.0}));
      _chartYValues.add(Arrays.asList(new Double[]{106765., 131725., 127868.}));
      _chartYValues.add(Arrays.asList(new Double[]{108456., 119326., 139326.}));
      _chartYValues.add(Arrays.asList(new Double[]{136765., 147265., 184349.})); 
      _chartYValues.add(Arrays.asList(new Double[]{107868., 113968., 174349.}));

      _dialchartYValues = new ArrayList<List<Double>>();
      _dialchartYValues.add(Arrays.asList(new Double[]{135.}));
      _dialchartYValues.add(Arrays.asList(new Double[]{106.}));
    
      _chartXValues = new ArrayList<List<Double>>();
      _chartXValues.add(Arrays.asList(new Double[]{6.1, 6.3, 6.5}));
      _chartXValues.add(Arrays.asList(new Double[]{6.8, 7.1, 7.3}));
      _chartXValues.add(Arrays.asList(new Double[]{7.6, 7.8, 8.0}));
      _chartXValues.add(Arrays.asList(new Double[]{8.25, 8.55, 8.78}));
      _chartXValues.add(Arrays.asList(new Double[]{9.23, 9.48, 9.88}));
    }
    
  } 
  
  private boolean _largerDataSet = false;
  private int     _currentTypeIndex = 0;
  private static final String[] _chartTypes = 
        new String[]{"verticalBar", "horizontalBar", "stackedVerticalBar","stackedHorizontalBar", 
        "pie", "area", "stackedArea", "line", "barLine", "XYLine", "scatterPlot", "radar",
        "radarArea", "funnel", "circularGauge", "semiCircularGauge"};

  private final ChartModel _chartModel = new MyChartModel(); 
  private final ComponentEditorHandler _editorBean = new ComponentEditorHandler();
}
