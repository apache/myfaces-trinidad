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
package org.apache.myfaces.trinidaddemo.support.util;

import org.apache.myfaces.trinidaddemo.ComponentDemoInitializer;
import org.apache.myfaces.trinidaddemo.ComponentDemoRegistry;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;
import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;

import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility class for generating the corresponding sitemap.xml for this demo app.
 *
 * Example mvn commands:
 *   -> mvn exec:java
 *      -Dexec.args="http://example.irian.at/trinidad-components-showcase"
 *      -Dexec.mainClass="org.apache.myfaces.trinidaddemo.support.util.SitemapGenerator"
 *
 *   -> mvn clean package -P generateSitemap -DsitemapUrlBasePath=http://example.irian.at/trinidad-components-showcase
 */
public class SitemapGenerator {

    private static final String DEFAULT_PAGE_PATH = "/faces/pages/demoStart.xhtml";
    private static final String COMPONENT_DEMO_PAGE_PATH = "/component-demo";

    private String targetDir;
    private String loc;
    private String priority;
    private String changeFreq;

    /**
     * Constructor.
     *
     * @param targetDir
     * @param loc
     */
    public SitemapGenerator(String targetDir, String loc) {
        this(targetDir, loc, "0.5", "daily");
    }

    /**
     * Constructor.
     *
     * @param targetDir
     * @param loc
     * @param priority
     * @param changeFreq
     */
    public SitemapGenerator(String targetDir, String loc, String priority, String changeFreq) {
        this.targetDir = targetDir;
        this.loc = loc;
        this.priority = priority;
        this.changeFreq = changeFreq;
    }

    /**
     * Performs the generation of the sitemap.xml file.
     */
    public void generateSitemap() {
        List<String> urls = getSitemapUrls();

        try {
            File outputDir = new File(targetDir);
            if (!outputDir.exists()) {
                outputDir.mkdirs();
            }

            File outputFile = new File(outputDir, "sitemap.xml");

            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile),"UTF8"));
            
            out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            out.write("<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n");                 

            Iterator<String> urlsIterator = urls.listIterator();
            while (urlsIterator.hasNext()) {
                out.write(urlsIterator.next());
            }

            out.write(getSitemapUrl(loc + DEFAULT_PAGE_PATH));
            out.write("</urlset>");

            out.flush();
            out.close();
        }
        catch (Exception exc) {
            exc.printStackTrace();
        }
    }

    /**
     * @return
     */
    private List<String> getSitemapUrls() {
        List<String> list = new ArrayList<String>();

        ComponentDemoInitializer initializer = ComponentDemoInitializer.getInstance();

        ComponentDemoRegistry registry = ComponentDemoRegistry.getInstance();
        initializer.registerComponentDemos(registry);

        Map<ComponentDemoId, Set<IComponentDemoVariantId>> variantsAdded = new HashMap<ComponentDemoId, Set<IComponentDemoVariantId>>();

        Iterator<IComponentDemoCategory> categories = registry.getDemoCategories().iterator();
        while (categories.hasNext()) {
            IComponentDemoCategory aCategory = categories.next();
            Iterator<IComponentDemo> componentDemos = aCategory.getComponentDemos().iterator();
            while (componentDemos.hasNext()) {
                IComponentDemo aComponentDemo = componentDemos.next();

                Set<IComponentDemoVariantId> alreadyAddedVariants = variantsAdded.get(aComponentDemo.getId());
                if (alreadyAddedVariants == null) {
                    alreadyAddedVariants = new HashSet<IComponentDemoVariantId>();
                    variantsAdded.put(aComponentDemo.getId(), alreadyAddedVariants);
                }

                Iterator<IComponentVariantDemo> variants = aComponentDemo.getVariants().iterator();
                while (variants.hasNext()) {
                    IComponentVariantDemo aVariantDemo = variants.next();

                    if (!alreadyAddedVariants.contains(aVariantDemo.getVariantId())) {
                        list.add(getSitemapUrl(getUrl(aVariantDemo)));
                        alreadyAddedVariants.add(aVariantDemo.getVariantId());
                    }
                }
            }
        }

        return list;
    }

    /**
     * @param componentVariantDemo
     * @return
     */
    private String getUrl(IComponentVariantDemo componentVariantDemo) {
        StringBuilder builder = new StringBuilder();
        builder.append(loc);
        builder.append(COMPONENT_DEMO_PAGE_PATH);
        builder.append("/");
        builder.append(componentVariantDemo.getId());
        builder.append("-");       
        builder.append(componentVariantDemo.getVariantId());

        return builder.toString();
    }

    /**
     * @param url
     * @return
     */
    private String getSitemapUrl(String url) {
        StringBuilder builder = new StringBuilder();

        builder.append("<url>").append("\n");
            builder.append("\t<loc>");
            builder.append(url);
            builder.append("</loc>").append("\n");        

            builder.append("\t<priority>");
            builder.append(priority);
            builder.append("</priority>").append("\n");

            builder.append("\t<changefreq>");
            builder.append(changeFreq);
            builder.append("</changefreq>").append("\n");
        builder.append("</url>").append("\n");

        return builder.toString();
    }    

    public static void main(String[] args) {
        DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
        String date = dateFormat.format(Calendar.getInstance().getTime());

        String targetDir = "/src/main/webapp"; //default value
        String loc = "http://example.irian.at/trinidad-components-showcase-"+date; //default value        

        if (args != null && args.length == 2) {
            if (args[0] != null) {
                targetDir = args[0];
            }

            if (args[1] != null) {
                loc = args[1];
            }
        }

        SitemapGenerator generator = new SitemapGenerator(targetDir, loc);
        generator.generateSitemap();        
    }
}
