/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfbuild.plugin.mock.maker;

import com.thoughtworks.qdox.JavaDocBuilder;
import com.thoughtworks.qdox.Searcher;
import com.thoughtworks.qdox.model.DocletTag;
import com.thoughtworks.qdox.model.JavaClass;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Writer;

import java.text.MessageFormat;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.myfaces.adfbuild.plugin.mock.maker.structure.SourceClassStructure;

public class MockDoclet {

  private File sourceDir;
  private File destDir;
  private Configuration config = new RealConfiguration(RealConfiguration.DEFAULT_CONFIGURATION_FILENAME);
  private List files = new LinkedList();
  private List classLoaders = new LinkedList();
  private PrintStream log;

  public void setLog(PrintStream log) {
    this.log = log;
  }

  public File getSourceDir() {
    return sourceDir;
  }

  public void setSourceDir(File sourceDir) {
    this.sourceDir = sourceDir;
  }

  public void addFile(File file) {
    files.add(file);
  }

  public File getDestDir() {
    return destDir;
  }

  public void setDestDir(File destDir) {
    this.destDir = destDir;
  }

  public void setConfigProperty(String propertyName, String value) {
    config.set(propertyName, value);
  }

  public void generateMocks() throws Exception {
    long start = System.currentTimeMillis();

    JavaDocBuilder builder = new JavaDocBuilder();

    // set classloader
    for (Iterator iterator = classLoaders.iterator(); iterator.hasNext();) {
      ClassLoader classLoader = (ClassLoader)iterator.next();
      builder.getClassLibrary().addClassLoader(classLoader);
    }

    // parse source tree
    if (sourceDir != null) {
      builder.addSourceTree(sourceDir);
    }
    for (Iterator iterator = files.iterator(); iterator.hasNext();) {
      File file = (File)iterator.next();
      builder.addSource(new FileReader(file));
    }

    // find @mock tags
    List mocksNeeded = builder.search(new Searcher() {
      public boolean eval(JavaClass cls) {
        final DocletTag[] tags = cls.getTags();
        for (int i = 0; i < tags.length; i++) {
          String tagName = tags[i].getName();
          if (tagName.startsWith("mock")) return true;
        }
        return false;
      }
    });

    // mock them up
    for (Iterator iterator = mocksNeeded.iterator(); iterator.hasNext();) {
      JavaClass javaClass = (JavaClass)iterator.next();
      report("Mocking up " + javaClass.getName());
      Writer out = new FileWriter(getDestinationFile(javaClass));
      CodeWriter output = new RealCodeWriter(new PrintWriter(out));
      MockMaker mockMaker = new MockMaker(SourceClassStructure.create(javaClass), output, config);
      mockMaker.setIncludePackage(true);
      mockMaker.make();
      out.close();
    }

    long end = System.currentTimeMillis();
    report("Time taken to generate mocks : " + (end - start) + "ms");
  }

  private File getDestinationFile(JavaClass javaClass) {
    File outDir = new File(destDir, getDestinationDirectoryName(javaClass.getPackage()));
    outDir.mkdirs();
    File outFile = new File(outDir, getDestinationFileName(javaClass.getName()));
    return outFile;
  }

  public String getDestinationFileName(String cls) {
    return MessageFormat.format(config.classNameFormat(), new String[] {cls}) + ".java";
  }

  public String getDestinationDirectoryName(String pkg) {
    pkg = MessageFormat.format(config.packageNameFormat(), new String[] {pkg});
    int dot;
    while((dot = pkg.indexOf('.')) > -1) {
      pkg = pkg.substring(0, dot) + "/" + pkg.substring(dot + 1);
    }
    return pkg;
  }

  public void addClassLoader(ClassLoader classLoader) {
    classLoaders.add(classLoader);
  }

  private void report(String msg) {
    if (log != null) {
      log.println(msg);
    }
  }

  public static void main(String[] args) throws Exception {
    MockDoclet d = new MockDoclet();
    d.setLog(System.out);
    d.setSourceDir(new File(args[0]));
    d.setDestDir(new File(args[1]));
    d.generateMocks();
  }

}
