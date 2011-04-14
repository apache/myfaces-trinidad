#!/bin/sh
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

# settings:
VERSION=2.0.0-beta-3
REPOSITORY=https://repository.apache.org/content/repositories/releases

# download the bits (user-agent to "" is important)
wget --user-agent="" $REPOSITORY/org/apache/myfaces/trinidad/trinidad-assembly/$VERSION/trinidad-assembly-$VERSION-dist.tar.gz
wget --user-agent="" $REPOSITORY/org/apache/myfaces/trinidad/trinidad-assembly/$VERSION/trinidad-assembly-$VERSION-dist.zip
wget --user-agent="" $REPOSITORY/org/apache/myfaces/trinidad/trinidad-example-assembly/$VERSION/trinidad-example-assembly-$VERSION-example.tar.gz
wget --user-agent="" $REPOSITORY/org/apache/myfaces/trinidad/trinidad-example-assembly/$VERSION/trinidad-example-assembly-$VERSION-example.zip
wget --user-agent="" $REPOSITORY/org/apache/myfaces/trinidad/trinidad/$VERSION/trinidad-$VERSION-source-release.zip

# rename the dist to the name which is used on the site
mv trinidad-assembly-$VERSION-dist.zip trinidad-$VERSION-dist.zip
mv trinidad-assembly-$VERSION-dist.tar.gz trinidad-$VERSION-dist.tar.gz

# rename the example-dist to the name which is used on the site
mv trinidad-example-assembly-$VERSION-example.zip trinidad-$VERSION-example.zip
mv trinidad-example-assembly-$VERSION-example.tar.gz trinidad-$VERSION-example.tar.gz

# rename the source to the name which is used on the site
mv trinidad-$VERSION-source-release.zip trinidad-$VERSION-src-all.zip 

# call the GPG script 
/home/matzew/gpg-sign-all

# uploading....
## For alpha/beta (all non 'final' releases), we have to use these URLS:
# minotaur.apache.org:/www/myfaces.apache.org/dist/binaries
# minotaur.apache.org:/www/myfaces.apache.org/dist/source
scp *-dist.* minotaur.apache.org:/www/www.apache.org/dist/myfaces/binaries
scp *-example.* minotaur.apache.org:/www/www.apache.org/dist/myfaces/binaries
scp *-src-all.* minotaur.apache.org:/www/www.apache.org/dist/myfaces/source
