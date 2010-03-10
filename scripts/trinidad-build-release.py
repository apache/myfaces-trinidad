#!/usr/bin/python
# -----------------------------------------------------------------------------
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# -----------------------------------------------------------------------------
import os, sys, posixpath
from xml.dom.minidom import parse

### Run the script as
### ./trinidad-build-release.py verysecret /home/matzew/work/source/dude
###

#global variables, customize them...
home = '/home/matzew'
stagingHome = home
remoteHome  = home
staging = stagingHome + '/staging'
remote  = remoteHome + '/public_html/trinidad'
remoteServer = 'people.apache.org'
stagingRepository = 'matzewStaging::default::scp://minotaur.apache.org/home/matzew/public_html/staging_repo'

#script begins
def directoryExists(directory):
  return os.system('[ -d ' + directory + ' ]') == 0  

def fileExists(filename):
  return os.system('[ -f ' + filename + ' ]') == 0  

def runCommand(command, directory='', tries=1):
  cmdline = 'set -e; '
  if directory != '':
    cmdline += 'cd "'+directory+'"; '
  cmdline += command
  myrange = xrange(tries)
  for i in myrange:
    if(os.system(cmdline) == 0):
      return
  print 'Command Failed:', cmdline
  sys.exit(1)

def runRemoteCommand(server, command, tries=3):
  cmdline = 'ssh '+server+' "'+command+'"'
  runCommand(cmdline, '', tries)

def signFiles(directory):
  runCommand('for path in `ls -b`;do echo '+passphrase+'|gpg --clearsign --passphrase-fd "$path" -o "$path.asc";done', directory)

def generateChecksums(directory):
  runCommand('for path in `ls -b *.zip *.tar.gz *.asc`;do md5sum $path > $path.md5;sha1sum $path > $path.sha1;done',directory)

def copyFiles(fromDir, toDir):
  runCommand('cp * "'+toDir+'"', fromDir)
  
def buildAssembly(srcDir, destination):
  print 'Assembling: ', srcDir
  runCommand('mvn -e clean install package assembly:assembly -Dgpg.passphrase='+passphrase, srcDir, 3)
  srcDir += '/target/assembly/out'
  signFiles(srcDir)
  generateChecksums(srcDir)
  copyFiles(srcDir, destination)

def getPomVersion(pomFile):
  document = parse(pomFile)
  for node in document.documentElement.childNodes:
    if node.nodeType == node.ELEMENT_NODE and node.nodeName == 'version':
        #actually get version
        for txtNode in node.childNodes:
          if txtNode.nodeType == node.TEXT_NODE or txtNode.nodeType == node.CDATA_SECTION_NODE:
            nodeVal = txtNode.nodeValue.strip()
          if nodeVal != '':
            return nodeVal
  print 'Unable to get pom version:', pomFile
  sys.exit(1)

passphrase = sys.argv[1]

numSrc = xrange(2, len(sys.argv))
for srcindx in numSrc:
  srcDir = sys.argv[srcindx]
  version = getPomVersion(srcDir + '/pom.xml')
  stagingDir = staging + '/' + version
  print 'Staging Directory:', stagingDir
  if directoryExists(stagingDir):
    print 'Deleting old staging directory'+stagingDir
    runCommand('rm -Rf '+stagingDir)
  archive = version + '.tar.gz'
  stagingArc = staging + '/' + archive
  print 'Staging Archive:', stagingArc
  if fileExists(stagingArc):
    print 'Deleting old staging archive',stagingArc
    runCommand('rm -R '+stagingArc)
  runCommand('mkdir '+stagingDir)

  ## release plugin interaction (requires 2.0 of the release plugin)
  runCommand('mvn -e release:stage -DstagingRepository='+ stagingRepository +' -DperformRelease=true -Dgpg.passphrase='+passphrase, srcDir, 2)

  buildAssembly(srcDir+'/trinidad-assembly', stagingDir)
  runCommand('mvn -e clean source:jar install', srcDir + '/trinidad-examples',2)
  buildAssembly(srcDir+'/trinidad-examples/trinidad-example-assembly', stagingDir)

  #do some cleanup
  #runCommand('rm -Rf trinidad-blank trinidad-demo trinidad-example', stagingDir+"/org/apache/myfaces/trinidad")
  #package everything up

  print 'Creating archive for transfer:',stagingArc
  runCommand('tar czf '+stagingArc+' '+version, staging)
  remoteDir = remote + version
  print 'Remote Directory:', remoteServer + ':' + remoteDir
  print 'Removing old archive from server'
  remoteArc  = remoteHome + '/' + archive
  runRemoteCommand(remoteServer, 'if [ -f '+remoteArc+' ]; then rm -f '+remoteArc+';fi')
  print 'Transfering archive to server'
  runCommand('scp '+stagingArc+' '+remoteServer+':'+remoteHome, '', 3)
  print 'Replacing the old staging directories on apache.  (See http://people.apache.org/~matzew/trinidad/'+version+')'
  runRemoteCommand(remoteServer, 'if [ -d '+remoteDir+' ]; then rm -Rf '+remoteDir+';fi;cd '+remote+';tar -xzf '+remoteArc)
