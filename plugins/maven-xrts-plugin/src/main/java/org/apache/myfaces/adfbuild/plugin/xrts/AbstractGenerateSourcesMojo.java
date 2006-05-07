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
package org.apache.myfaces.adfbuild.plugin.xrts;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import java.util.Arrays;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;


import oracle.bali.rts.tools.RTSWriter;
import oracle.bali.rts.tools.XRTSGenerator;

import oracle.xml.parser.v2.SAXParser;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

import org.codehaus.plexus.util.DirectoryScanner;

import org.xml.sax.InputSource;

/**
 * @version $Id$
 * @phase generate-resources
 * @description Goal which generates the XRTS Bundles
 */
abstract public class AbstractGenerateSourcesMojo extends AbstractMojo
{
  /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      generateBundles();
    }
    catch (IOException e)
    {
      throw new MojoExecutionException(e.getMessage());
    }
  }

  abstract protected String getTargetType();

  abstract protected String[] getExcludes();

  abstract protected File getSourceDirectory();

  abstract protected File getTargetDirectory();

  abstract protected void addCompileSourceRoot() throws IOException;

  private void generateBundles() throws IOException, MojoExecutionException
  {
    File sourceDirectory = getSourceDirectory();
    if (sourceDirectory.exists())
    {
      // Include generated sources in compilation
      addCompileSourceRoot();

      // Scan for .xrts sources
      DirectoryScanner scanner = new DirectoryScanner();
      scanner.setBasedir(sourceDirectory);
      scanner.addDefaultExcludes();
      String[] excludes = getExcludes();
      scanner.setExcludes(excludes);
      scanner.setIncludes(new String[] { "**/*.xrts" });
      scanner.scan();

      String[] xrtsFiles = scanner.getIncludedFiles();
      if (xrtsFiles.length > 0)
      {
        RTSWriter writer = getRTSWriter();
        Dictionary params = new Hashtable();

        List dirtyXRTS = new LinkedList(Arrays.asList(xrtsFiles));
        for (Iterator i=dirtyXRTS.iterator(); i.hasNext();)
        {
          String xrtsFile = (String)i.next();
          File targetFile = getTargetFile(xrtsFile);
          if (targetFile.exists())
          {
            File sourceFile = getSourceFile(xrtsFile);
            if (targetFile.lastModified() >= sourceFile.lastModified())
            {
              i.remove();
            }
          }
        }

        if (dirtyXRTS.isEmpty())
        {
          getLog().info("Nothing to generate - all XRTS bundles are up to date");
        }
        else
        {
          getLog().info("Generating " + dirtyXRTS.size() + " XRTS bundles to " + getTargetDirectory());

          for (Iterator i=dirtyXRTS.iterator(); i.hasNext();)
          {
            String xrtsFile = (String)i.next();
            File sourceFile = getSourceFile(xrtsFile);
            File targetFile = getTargetFile(xrtsFile);
            String baseName = getBasename(xrtsFile);

            params.put("outFile", targetFile);
            params.put("outName", baseName);
            params.put("srcName", baseName);
            params.put("validate", Boolean.FALSE);
            params.put("quietMode", Boolean.TRUE);

            SAXParser parser = new SAXParser();
            InputSource source = new InputSource(new FileInputStream(sourceFile));
            // setup relative systemId resolution for local rts.dtd files.
            source.setSystemId(sourceFile.getParentFile().toURL().toString());

            try
            {
              if (targetFile.exists())
                targetFile.delete();

              targetFile.getParentFile().mkdirs();
              XRTSGenerator.generate(parser, source, writer, params);
              targetFile.setReadOnly();
            }
            catch (Throwable t)
            {
              throw new MojoExecutionException(t.getMessage());
            }
          }
        }
      }
    }
  }

  private RTSWriter getRTSWriter() throws IOException, MojoExecutionException
  {
    InputStream config = XRTSGenerator.class.getResourceAsStream("Config.txt");
    if (config == null)
      throw new IllegalStateException("Missing Config.text");

    String implClassName;

    String targetType = getTargetType();
    if (targetType.startsWith("class:"))
    {
      implClassName = targetType.substring("class:".length());
    }
    else
    {
      Properties props = new Properties();
      props.load(config);
      config.close();

      implClassName = (String)props.get(targetType);

      if (implClassName == null)
        throw new IllegalArgumentException("Unrecognized targetType: " + targetType);
    }

    try
    {
      Class implClass = Class.forName(implClassName);
      return (RTSWriter)implClass.newInstance();
    }
    catch (InstantiationException e)
    {
      e.printStackTrace();
      throw new MojoExecutionException(e.getMessage());
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
      throw new MojoExecutionException(e.getMessage());
    }
    catch (IllegalAccessException e)
    {
      e.printStackTrace();
      throw new MojoExecutionException(e.getMessage());
    }
  }

  private File getSourceFile(String xrtsFilename)
  {
    return new File(getSourceDirectory(), xrtsFilename);
  }

  private String getBasename(String sourcePath)
  {
    int start = sourcePath.lastIndexOf(File.separatorChar) + 1;
    return sourcePath.substring(start, sourcePath.length() - ".xrts".length());
  }

  private File getTargetFile(String sourcePath)
  {
    String basename = sourcePath.substring(0, sourcePath.length() - ".xrts".length());
    String targetPath = basename + ".java";
    return new File(getTargetDirectory(), targetPath);
  }
}
