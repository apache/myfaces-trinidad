/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidaddemo.components.output.chart;

import org.apache.myfaces.trinidad.model.ChartModel;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.Serializable;

/**
 *
 */
public class ChartBean implements Serializable {

    public ChartModel getValue() {
        return _chartModel;
    }

    private class MyChartModel extends ChartModel implements Serializable {
        @Override
        public List<String> getSeriesLabels() {
            if (_largerDataSet)
                return _largeSeriesLabels;
            else
                return _seriesLabels;
        }

        @Override
        public List<String> getGroupLabels() {

            return _groupLabels;
        }

        @Override
        public List<List<Double>> getXValues() {

            return null;
        }

        @Override
        public List<List<Double>> getYValues() {

            return _chartYValues;
        }   

        @Override
        public Double getMaxYValue() {

            return 200000.0;
        }

        @Override
        public Double getMinYValue() {

            return 0.0;
        }

        @Override
        public Double getMaxXValue() {
            if (_largerDataSet)
                return 54.0;
            else
                return 10.0;
        }

        @Override
        public Double getMinXValue() {
            if (_largerDataSet)
                return 0.0;
            else
                return 6.0;
        }

        @Override
        public String getTitle() {

            return "Title";
        }

        @Override
        public String getSubTitle() {

            return "SubTitle";
        }

        @Override
        public String getFootNote() {

            return "FootNote";
        }

        private final List<String> _groupLabels =
                Arrays.asList(new String[]{"June", "July", "August", "September", "October"});

        private final List<String> _seriesLabels =
                Arrays.asList(new String[]{"Previous", "Target", "Actual"});

        private final List<String> _largeSeriesLabels =
                Arrays.asList(new String[]{"Opening", "Low", "High"});

        private final ArrayList<List<Double>> _chartYValues;
        private final ArrayList<List<Double>> _chartXValues;
        private final ArrayList<List<Double>> _dialchartYValues;

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

    private final ChartModel _chartModel = new MyChartModel();


}
