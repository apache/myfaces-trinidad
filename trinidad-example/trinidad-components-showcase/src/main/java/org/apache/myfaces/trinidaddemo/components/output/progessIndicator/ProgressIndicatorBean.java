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
package org.apache.myfaces.trinidaddemo.components.output.progessIndicator;

import org.apache.myfaces.trinidad.model.BoundedRangeModel;
import org.apache.myfaces.trinidad.model.DefaultBoundedRangeModel;

import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 */
public class ProgressIndicatorBean implements Serializable {
    protected volatile DefaultBoundedRangeModel __model;
    protected volatile ProcessThread __processThread;
    protected boolean finished = false;


    public BoundedRangeModel getProgressModel() {
        return __model;
    }

    public boolean isFinished() {
        return finished;
    }

    public void beginProcess() {
        if (null == __model) {
            prepare();
        }
    }

    public void endProcess() {
        __processThread = null;
        __model = null;
        finished = false;
    }

    public void finishProcess() {
        finished = true;
    }

    protected void prepare() {
        __model = new DefaultBoundedRangeModel(-1, 125);
        //pu: simulate asynchronous model updates on a different thread
        __processThread = new ProcessThread(500, 0);
        __processThread.start();
    }

    protected class ProcessThread extends Thread implements Serializable {        
        private long _updateIntervalFactor;
        private long _updateValueFactor;

        /**
         * @param updateIntervalFactor - controls the speed of the thread
         * @param updateValueFactor    - The value by which the 'value' from the
         *                             model should be incremented for every cycle. Randomizes the increment
         *                             if updateValueFactor supplied is '0'.
         */
        ProcessThread(long updateIntervalFactor, long updateValueFactor) {
            _updateIntervalFactor = updateIntervalFactor;
            _updateValueFactor = updateValueFactor;
        }

        @Override
        public void run() {
            try {
                //pu: Be in indeterminate mode for some time to start with
                sleep(3000);
                //pu: Take care to get out if we are the discarded thread upon endProcess()
                while ((__processThread == Thread.currentThread()) &&
                        (__model != null) &&
                        (__model.getValue() < __model.getMaximum())
                        ) {
                    long sleepFactor = Math.round(Math.random() * 10);
                    long updatedValue = __model.getValue() +
                            ((_updateValueFactor == 0) ? sleepFactor : _updateValueFactor);
                    long maximum = __model.getMaximum();
                    if (updatedValue > maximum) {
                        updatedValue = maximum;
                    }
                    __model.setValue(updatedValue);
                    sleep(sleepFactor * _updateIntervalFactor);
                }
            }
            catch (InterruptedException ie) {
                _LOG.log(Level.WARNING, "Background task thread interrupted", ie);
            }
            __model = null;
        }

    }


    static private final Logger _LOG = Logger.getLogger(
            ProgressIndicatorBean.class.getName());

}
