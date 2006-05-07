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

import java.io.File;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codehaus.plexus.compiler.util.scan.SimpleSourceInclusionScanner;
import org.codehaus.plexus.compiler.util.scan.SourceInclusionScanner;
import org.codehaus.plexus.compiler.util.scan.StaleSourceScanner;

/**
 * @version $Id$
 * @goal mock-compile
 * @requiresDependencyResolution test
 * @phase process-test-resources
 * @description Goal compiles the mock classes
 */
public class MockCompileMojo extends AbstractCompilerMojo
{
  /**
   * @parameter expression="src/mock/java"
   * @required
   * @readonly
   */
  private File sourceDirectory;

  /**
   * The source directories containing the sources to be compiled.
   *
   * @parameter expression="${project.build.directory}/maven-mock-plugin/mock/java"
   * @required
   * @readonly
   */
  private File generatedSourceDirectory;

  /**
   * Project classpath.
   *
   * @parameter expression="${project.testClasspathElements}"
   * @required
   * @readonly
   */
  private List classpathElements;

  /**
   * The directory for compiled classes.
   *
   * @parameter expression="${project.build.directory}/mock-classes"
   * @required
   * @readonly
   */
  private File outputDirectory;

  /**
   * A list of inclusion filters for the compiler.
   *
   * @parameter
   */
  private Set includes = new HashSet();

  /**
   * A list of exclusion filters for the compiler.
   *
   * @parameter
   */
  private Set excludes = new HashSet();

  protected List getCompileSourceRoots()
  {
    try
    {
      return Arrays.asList(new String[]
                           { sourceDirectory.getCanonicalPath(),
                             generatedSourceDirectory.getCanonicalPath() });
    }
    catch (IOException e)
    {
      getLog().error("Unable to get mock compile source roots");
      return Collections.EMPTY_LIST;
    }
  }

  protected List getClasspathElements()
  {
    return classpathElements;
  }

  protected File getOutputDirectory()
  {
    return outputDirectory;
  }

  protected SourceInclusionScanner getSourceInclusionScanner(
    int staleMillis)
  {
    SourceInclusionScanner scanner = null;

    if (includes.isEmpty() && excludes.isEmpty())
    {
        scanner = new StaleSourceScanner(staleMillis);
    }
    else
    {
      if (includes.isEmpty())
      {
        includes.add( "**/*.java" );
      }
      scanner = new StaleSourceScanner(staleMillis, includes, excludes);
    }

    return scanner;
  }

  protected SourceInclusionScanner getSourceInclusionScanner(
    String inputFileEnding)
  {
    SourceInclusionScanner scanner = null;

    if (includes.isEmpty() && excludes.isEmpty())
    {
      includes = Collections.singleton( "**/*." + inputFileEnding );
      scanner = new SimpleSourceInclusionScanner( includes, Collections.EMPTY_SET );
    }
    else
    {
      if (includes.isEmpty())
      {
        includes.add( "**/*." + inputFileEnding );
      }
      scanner = new SimpleSourceInclusionScanner(excludes, excludes);
    }

    return scanner;
  }
}
