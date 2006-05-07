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
package org.apache.myfaces.adfbuild.plugin.mock;

import com.thoughtworks.qdox.JavaDocBuilder;
import com.thoughtworks.qdox.Searcher;
import com.thoughtworks.qdox.directorywalker.DirectoryScanner;
import com.thoughtworks.qdox.directorywalker.FileVisitor;
import com.thoughtworks.qdox.directorywalker.Filter;
import com.thoughtworks.qdox.directorywalker.SuffixFilter;
import com.thoughtworks.qdox.model.DocletTag;
import com.thoughtworks.qdox.model.JavaClass;
import com.thoughtworks.qdox.model.JavaSource;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import java.net.URL;
import java.net.URLConnection;

import java.text.MessageFormat;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.myfaces.adfbuild.plugin.mock.maker.CodeWriter;
import org.apache.myfaces.adfbuild.plugin.mock.maker.Configuration;
import org.apache.myfaces.adfbuild.plugin.mock.maker.MockMaker;
import org.apache.myfaces.adfbuild.plugin.mock.maker.RealCodeWriter;
import org.apache.myfaces.adfbuild.plugin.mock.maker.RealConfiguration;
import org.apache.myfaces.adfbuild.plugin.mock.maker.structure.SourceClassStructure;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * @version $Id$
 * @goal generate-mock-sources
 * @phase generate-test-sources
 * @description Goal which generates the mock objects
 */
public class GenerateMockSourcesMojo extends AbstractMojo
{
  /**
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * @parameter expression="${project.compileSourceRoots}"
   * @required
   * @readonly
   */
  private List compileSourceRoots;

  /**
   * @parameter expression="src/mock/java"
   * @required
   * @readonly
   */
  private File sourceDirectory;

  /**
   * @parameter expression="${project.build.directory}/maven-mock-plugin/mock/java"
   * @required
   * @readonly
   */
  private File targetDirectory;

  /**
   * @parameter
   */
  private String[] excludes;

  /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      project.addTestCompileSourceRoot(sourceDirectory.getCanonicalPath());
      project.addTestCompileSourceRoot(targetDirectory.getCanonicalPath());

      final JavaDocBuilder builder = new JavaDocBuilder();

      // iterate over the compile source roots
      // to find all the .java files
      for (Iterator i = compileSourceRoots.iterator(); i.hasNext(); )
      {
        String sourceRoot = (String)i.next();
        File rootFile = new File(sourceRoot);

        if (rootFile.isDirectory())
        {
          DirectoryScanner scanner = new DirectoryScanner(rootFile);
          if (excludes != null)
            scanner.addFilter(new ExcludesFilter(excludes));
          scanner.addFilter(new SuffixFilter(".java"));
          scanner.scan(
		    new FileVisitor()
            {
              public void visitFile(File currentFile)
              {
                try
                {
                  builder.addSource(currentFile);
                }
                catch (IOException e)
                {
                  throw new RuntimeException("Cannot read file : " + currentFile.getName());
                }
              }
		    });
        }
      }

      // extract list of java sources with @mock javadoc tag
      List mockSources = builder.search(_MOCK_TAG_SEARCHER);

      if (!mockSources.isEmpty())
      {
        Configuration config = new RealConfiguration(RealConfiguration.DEFAULT_CONFIGURATION_FILENAME);
        config.set("keepUsingLastReturnValue", "true");

        List dirtyMockSources = new LinkedList(mockSources);
        for (Iterator i=dirtyMockSources.iterator(); i.hasNext();)
        {
          JavaClass javaClass = (JavaClass)i.next();
          File target = getFile(config, javaClass);
          if (isUptodate(javaClass, target))
            i.remove();
        }

        if (dirtyMockSources.isEmpty())
        {
          getLog().info("Nothing to generate - all mock objects are up to date");
        }
        else
        {
          getLog().info("Generating " + dirtyMockSources.size() + " mock objects to " + targetDirectory);

          // generate mocks for each out-of-date java source
          for (Iterator i=dirtyMockSources.iterator(); i.hasNext();)
          {
            JavaClass javaClass = (JavaClass)i.next();
            File target = getFile(config, javaClass);
            Writer out = new FileWriter(target);
            CodeWriter codeOut = new RealCodeWriter(new PrintWriter(out));
            SourceClassStructure structure = SourceClassStructure.create(javaClass);
            MockMaker mockMaker = new MockMaker(structure, codeOut, config);
            mockMaker.setIncludePackage(true);
            mockMaker.make();
            out.close();
          }
        }
      }
    }
    catch (IOException e)
    {
      throw new MojoExecutionException(e.getMessage());
    }
    catch (ClassNotFoundException e)
    {
      throw new MojoExecutionException(e.getMessage());
    }
  }


  private File getFile(
    Configuration config,
    JavaClass     javaClass)
  {
    String dirName = getDirectoryName(config, javaClass.getPackage());
    File packageDirectory = new File(targetDirectory, dirName);
    packageDirectory.mkdirs();
    String fileName = getFileName(config, javaClass.getName());
    return new File(packageDirectory, fileName);
  }

  public String getDirectoryName(
    Configuration config,
    String        packageName)
  {
    String packageNameFormat = config.packageNameFormat();
    String mockPackageName = MessageFormat.format(packageNameFormat, new String[] {packageName});
    return mockPackageName.replace('.', '/');
  }

  private String getFileName(
    Configuration config,
    String        className)
  {
    String classNameFormat = config.classNameFormat();
    String baseFileName = MessageFormat.format(classNameFormat, new String[] {className});
    return baseFileName + _JAVA_EXTENSION;
  }

  private boolean isUptodate(
    JavaClass javaClass,
    File      mockSource) throws IOException
  {
    if (!mockSource.exists())
      return false;

    JavaSource parentSource = javaClass.getParentSource();

    if (parentSource != null)
    {
      long lastModified = mockSource.lastModified();
      URL url = parentSource.getURL();
      URLConnection conn = url.openConnection();
      // uptodate if javaSource <= mockSource
      return (conn.getLastModified() <= lastModified);
    }

    return false;
  }

  static private class MockTagSearcher implements Searcher
  {
    public boolean eval(
      JavaClass javaClass)
    {
      DocletTag[] tags = javaClass.getTags();

      for (int i=0; i < tags.length; i++)
      {
        if (_MOCK_TAG_NAME.equals(tags[i].getName()))
          return true;
      }

      return false;
    }
  }

  static private class ExcludesFilter implements Filter
  {
	public ExcludesFilter(
	  String[] excludes)
	{
	  _excludes = excludes;
	}

    public boolean filter(File file)
    {
      // TODO: process _excludes generically
	  return (file.getPath().indexOf(".ade_path") == -1);
	}

	private final String[] _excludes;
  }

  static private final String   _JAVA_EXTENSION = ".java";
  static private final Searcher _MOCK_TAG_SEARCHER = new MockTagSearcher();
  static private final String   _MOCK_TAG_NAME = "mock";
}
