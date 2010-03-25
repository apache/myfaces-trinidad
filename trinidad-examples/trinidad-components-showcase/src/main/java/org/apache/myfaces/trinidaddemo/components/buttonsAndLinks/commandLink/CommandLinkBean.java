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
package org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.commandLink;

/**
 *
 */
public class CommandLinkBean {
    private float celsiusDegrees;
    private float fahrenheitDegrees;

    public CommandLinkBean() {
        this.celsiusDegrees = 0;
        this.fahrenheitDegrees = 32;
    }

    public float getCelsiusDegrees() {
        return celsiusDegrees;
    }

    public float getFahrenheitDegrees() {
        return fahrenheitDegrees;
    }

    public void setCelsiusDegrees(float celsiusDegrees) {
        this.celsiusDegrees = celsiusDegrees;
    }

    public void setFahrenheitDegrees(float fahrenheitDegrees) {
        this.fahrenheitDegrees = fahrenheitDegrees;
    }

    public void convertToFahrenheit(){
         fahrenheitDegrees = (9*celsiusDegrees)/5+32;
    }
}
