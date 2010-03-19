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
public class TableColumnBean implements java.io.Serializable {

    private static final long serialVersionUID = 6955236745608138415L;
    private ArrayList<Monarch> monarchs;

    public TableColumnBean() {
        monarchs = new ArrayList<Monarch>();
        monarchs.add(new Monarch("1","Charles II","29 May 1630","6 February 1685","King of England, Scotland, and Ireland. Charles II's father King Charles I was executed at Whitehall on 30 January 1649, at the climax of the English Civil War."));
        monarchs.add(new Monarch("2","James II","14 October 1633","16 September 1701","King of England and Ireland as James II, and Scotland as James VII. He was the last Catholic monarch to reign over the Kingdoms of England, Scotland, and Ireland."));
        monarchs.add(new Monarch("3","Mary II","30 April 1662","28 December 1694","Queen of England, Scotland, and Ireland from 1689 until her death. Mary, a Protestant, came to the thrones following the Glorious Revolution, which resulted in the deposition of her Roman Catholic father, James II and VII."));
        monarchs.add(new Monarch("4","William III","4 November 1650","8 March 1702","Prince of Orange by birth. From 1672 he governed as Stadtholder William III of Orange over Holland, Zeeland, Utrecht, Guelders, and Overijssel of the Dutch Republic."));
        monarchs.add(new Monarch("5","Anne","6 February 1665","1 August 1714","Queen of England, Scotland and Ireland on 8 March 1702, succeeding her brother-in-law, William III of England and II of Scotland."));
    }

    public ArrayList<Monarch> getMonarchs(){
        return monarchs;
    }

    public void setMonarchs(ArrayList<Monarch> monarchs){
        this.monarchs = monarchs;    
    }

    public class Monarch {
        private String id;
        private String name;
        private String birth;
        private String death;
        private String details;

        public Monarch(String id, String name, String birth, String death, String details) {
            this.id = id;
            this.name = name;
            this.birth = birth;
            this.death = death;
            this.details = details;
        }

        public String getId() {
            return id;
        }

        public void setId(String id){
            this.id = id;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getBirth() {
            return birth;
        }

        public void setBirth(String birth) {
            this.birth = birth;
        }

        public String getDeath() {
            return death;
        }

        public void setDeath(String death) {
            this.death = death;
        }

        public String getDetails() {
            return details;
        }

        public void setDetails(String details) {
            this.details = details;
        }
    }

}

