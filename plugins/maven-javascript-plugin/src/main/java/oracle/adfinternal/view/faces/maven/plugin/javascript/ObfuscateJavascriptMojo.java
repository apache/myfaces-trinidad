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
package oracle.adfinternal.view.faces.maven.plugin.javascript;

//~--- non-JDK imports --------------------------------------------------------

import oracle.adfinternal.view.faces.maven.plugin.javascript.obfuscator.Obfuscator;
import oracle.adfinternal.view.faces.maven.plugin.javascript.obfuscator.configuration.ConfigException;
import oracle.adfinternal.view.faces.maven.plugin.javascript.obfuscator.configuration.ObfuscatorConfig;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

//~--- JDK imports ------------------------------------------------------------

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


//~--- classes ----------------------------------------------------------------

/**
 * @version $Id$
 * @requiresDependencyResolution compile
 * @goal obfuscate-javascript
 * @phase generate-resources
 * @description Goal which obfuscates the Javascript sources
 */
public class ObfuscateJavascriptMojo
  extends AbstractMojo
{

  //~--- methods ------------------------------------------------------------

  public void execute()
    throws MojoExecutionException
  {
    List compileSourceRoots = new ArrayList();
    compileSourceRoots.add(sourceDirectory);

    try
    {
      _obfuscator =
        new Obfuscator(obfuscate, stripComments, stripWhitespaces,
                       stripNewlines, stripSpecialKeywords, replaceCharLiterals,
                       obfuscatorConfig);

      File outputDirectory = new File(targetDirectory, targetPath);

      outputDirectory.mkdirs();

      for (Iterator i = compileSourceRoots.iterator(); i.hasNext(); )
      {
        File sourceRoot = (File) i.next();
        File sourceDirectory = new File(sourceRoot, sourcePath);
        if (sourceDirectory.exists())
        {
          _obfuscator.process(sourceDirectory, outputDirectory);
        }
      }
    }
    catch (ConfigException e)
    {
      throw new MojoExecutionException("Invalid configuration parameters",
                                       e);
    }
  }
  private Obfuscator _obfuscator;


  /**
   * @parameter
   * @required
   */
  private boolean obfuscate = false;

  /**
   * @parameter
   * @required
   */
  private boolean replaceCharLiterals = false;

  /**
   * @parameter
   * @required
   */
  private boolean stripComments = false;

  /**
   * @parameter
   * @required
   */
  private boolean stripNewlines = false;

  /**
   * @parameter
   * @required
   */
  private boolean stripSpecialKeywords = false;

  /**
   * @parameter
   * @required
   */
  private boolean stripWhitespaces = false;

  /**
   * @parameter expression="src/main/javascript"
   * @required
   */
  private File sourceDirectory;

  /**
   * @parameter
   * @required
   */
  private String sourcePath;

  /**
   * @parameter expression="${project.build.directory}/classes"
   * @required
   */
  private File targetDirectory;

  /**
   * @parameter
   * @required
   */
  private String targetPath;

 /**
  * @parameter
  * @required
  */
  private ObfuscatorConfig obfuscatorConfig;
}

