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

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectHelper;

import org.codehaus.plexus.archiver.Archiver;
import org.codehaus.plexus.archiver.manager.ArchiverManager;

/**
 * @version $Id$
 * @goal mock-package
 * @phase package
 * @description Goal creates the mock JAR
 */
public class MockPackageMojo extends AbstractMojo
{
  private static final String[] DEFAULT_EXCLUDES = new String[]{"**/package.html"};

  private static final String[] DEFAULT_INCLUDES = new String[]{"**/**"};

  /**
   * @parameter expression="${project.build.directory}"
   * @required
   * @readonly
   */
  private File targetDirectory;

  /**
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * @parameter alias="jarName" expression="${project.artifactId}-${project.version}-mock"
   * @required
   */
  private String finalName;

  /**
   * @parameter expression="${project.build.directory}/mock-classes"
   * @required
   * @readonly
   */
  private File mockOutputDirectory;

  /**
   * To look up Archiver/UnArchiver implementations
   *
   * @parameter expression="${component.org.codehaus.plexus.archiver.manager.ArchiverManager}"
   * @required
   */

  private ArchiverManager archiverManager;

  /**
   * Maven ProjectHelper
   *
   * @parameter expression="${component.org.apache.maven.project.MavenProjectHelper}"
   * @required
   * @readonly
   */
  private MavenProjectHelper projectHelper;

 /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      if (!mockOutputDirectory.exists())
      {
        getLog().warn( "Mock JAR will be empty - no content was marked for inclusion!" );
      }
      else
      {
        File jarFile = new File(targetDirectory, finalName + ".jar");
        Archiver archiver = archiverManager.getArchiver("jar");
        archiver.setDestFile(jarFile);

        archiver.addDirectory(mockOutputDirectory,
                              DEFAULT_INCLUDES,
                              DEFAULT_EXCLUDES);

        archiver.createArchive();

        projectHelper.attachArtifact(project, "jar", "mock", jarFile);
      }
    }
    catch (Exception e)
    {
      throw new MojoExecutionException("Error assembling mock JAR", e);
    }
  }
}
