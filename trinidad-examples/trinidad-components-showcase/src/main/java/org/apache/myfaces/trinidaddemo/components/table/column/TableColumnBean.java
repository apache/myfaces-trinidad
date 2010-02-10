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

/**
 *
 */
public class TableColumnBean implements java.io.Serializable {
    static private int count = 0;
    static private Monarch monarchs;
    private int rowNo;
    private String monarchName;
    private String monarchBirth;
    private String monarchDeath;

    public TableColumnBean() {
        monarchs = new Monarch();
        monarchName = monarchs.getName(count);
        monarchBirth = monarchs.getBirth(count);
        monarchDeath = monarchs.getDeath(count);
        rowNo=(count++)+1;
    }

    public int getRowNo() {
        return rowNo;
    }

    public String getMonarchName() {
        return monarchName;
    }

    public String getMonarchBirth() {
        return monarchBirth;
    }

    public String getMonarchDeath() {
        return monarchDeath;
    }

    public static void setCount(int count) {
        TableColumnBean.count = count;
    }

    public void setMonarchName(String monarchName) {
        this.monarchName = monarchName;
    }

    public void setMonarchBirth(String monarchBirth) {
        this.monarchBirth = monarchBirth;
    }

    public void setMonarchDeath(String monarchDeath) {
        this.monarchDeath = monarchDeath;
    }
}

