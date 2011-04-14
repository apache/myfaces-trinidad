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
package org.apache.myfaces.trinidaddemo.support.impl;

import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;

import javax.faces.context.FacesContext;
import java.io.InputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 *
 */
public class ComponentVariantDemoDescriptionProvider {

    private static final Logger _LOG = Logger.getLogger(ComponentVariantDemoDescriptionProvider.class.getName());

    /**
     * @param facesContext
     * @param componentVariantDemo
     * @return
     */
    public static String getDescription(FacesContext facesContext, IComponentVariantDemo componentVariantDemo) {
        StringBuilder description = new StringBuilder();

        String summaryPath = componentVariantDemo.getSummaryResourcePath();
        BufferedReader reader = null;

        try {
            URL summaryURL = getResourceURL(facesContext, summaryPath);
            InputStream in = summaryURL.openStream();

            reader = new BufferedReader(new InputStreamReader(in));             

            try {
                String line = reader.readLine();               

                int index = 0;
                while (line != null && index < 4) {
                    line = line.trim();

                    if (!line.contains("ui:composition") &&
                       !line.contains("xmlns") &&
                        line.length() != 0) {
                            description.append(line);
                            description.append(" ");
                            index += 1;
                    }

                    line = reader.readLine();
                }                
            }
            finally {
                try {
                    if (reader != null) {
                        reader.close();
                    }
                }
                catch (IOException exc) {
                    _LOG.log(Level.SEVERE, "Error while closing file reader", exc);
                }
            }
        }
        catch (Exception exc) {
            _LOG.log(Level.INFO, "Error while reading file", exc);
        }

        return description.toString();
    }

    /**
     * @param facesContext
     * @param path
     * @return
     * @throws MalformedURLException
     */
    public static URL getResourceURL(FacesContext facesContext, String path) throws MalformedURLException {
        URL url = facesContext.getExternalContext().getResource(path);
        return url;
    }
}
