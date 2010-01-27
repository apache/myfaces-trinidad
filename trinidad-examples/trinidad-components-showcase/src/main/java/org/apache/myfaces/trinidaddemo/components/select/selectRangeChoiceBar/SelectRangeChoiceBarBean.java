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
package org.apache.myfaces.trinidaddemo.components.select.selectRangeChoiceBar;

import org.apache.myfaces.trinidad.event.RangeChangeEvent;
import java.util.ArrayList;

/**
 *
 */
public class SelectRangeChoiceBarBean implements java.io.Serializable {
    private ArrayList<String> stringList;
    private String stringSublist;
    private static int beginIndex = 0;
    private static int endIndex = 5;

    public SelectRangeChoiceBarBean() {
        stringSublist = new String();
        stringList = new ArrayList<String>();
        stringList.add("Anaconda");
        stringList.add("Antelope");
        stringList.add("Baboon");
        stringList.add("Butterfly");
        stringList.add("Catfish");
        stringList.add("Deer mouse");
        stringList.add("Elephant");
        stringList.add("Frog");
        stringList.add("Grasshopper");
        stringList.add("Hyena");
        stringList.add("Jellyfish");
        stringList.add("Mockingbird");
        stringList.add("Zebra");
    }

    public String getStringSublist() {
        stringSublist = stringList.get(beginIndex);
        for (int i = beginIndex+1; i < endIndex; i++)
            stringSublist = stringSublist + ", " + stringList.get(i);
        beginIndex = 0;
        endIndex = 5;
        return stringSublist;
    }

    public void rangeChange(RangeChangeEvent rce) {
        beginIndex = rce.getNewStart();
        if (rce.getNewEnd() >= 13) endIndex = 13;
        else endIndex = rce.getNewEnd();
    }

    public ArrayList<String> getStringList() {
        return stringList;
    }
    
}

