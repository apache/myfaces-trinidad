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
package org.apache.myfaces.trinidaddemo.components.table.column;

import java.util.ArrayList;

/**
 *
 */
public class Monarch {
    private ArrayList<String> name;
    private ArrayList<String> birth;
    private ArrayList<String> death;

    public Monarch(){
        name = new ArrayList<String>();
        name.add("Charles II");
        name.add("James II");
        name.add("William III");
        name.add("Mary II");
        name.add("Anne");

        birth = new ArrayList<String>();
        birth.add("29 May 1630");
        birth.add("14 October 1633");
        birth.add("4 November 1650");
        birth.add("30 April 1662");
        birth.add("6 February 1665");

        death = new ArrayList<String>();
        death.add("6 February 1685");
        death.add("16 September 1701");
        death.add("8 March 1702");
        death.add("28 December 1694");
        death.add("1 August 1714");        
    }

    public String getName(int index) {
        return name.get(index);
    }

    public String getBirth(int index) {
        return birth.get(index);
    }

    public String getDeath(int index) {
        return death.get(index);
    }

    public void setName(ArrayList<String> name) {
        this.name = name;
    }

    public void setBirth(ArrayList<String> birth) {
        this.birth = birth;
    }

    public void setDeath(ArrayList<String> death) {
        this.death = death;
    }
}
