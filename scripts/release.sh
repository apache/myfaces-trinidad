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

set -e

echo "Copying the download artifacts from the repository to the site."

if [ $# -ne 1 ]; then
    echo "Usage: $0 <version>"
    exit 1
fi

VERSION=$1

MAVEN_REPOSITORY=https://repository.apache.org/content/repositories/releases
DIST_REPOSITORY=https://dist.apache.org/repos/dist/release/myfaces

# download file and hashes/signatures
download() {
  DIR="$1"
  FILE_ON_REPO="$2"
  FILE="$3"

  curl --fail "${MAVEN_REPOSITORY}/${DIR}/${VERSION}/${FILE_ON_REPO}"      -o ${FILE}
  curl --fail "${MAVEN_REPOSITORY}/${DIR}/${VERSION}/${FILE_ON_REPO}.asc"  -o ${FILE}.asc
  curl --fail "${MAVEN_REPOSITORY}/${DIR}/${VERSION}/${FILE_ON_REPO}.md5"  -o ${FILE}.md5
  curl --fail "${MAVEN_REPOSITORY}/${DIR}/${VERSION}/${FILE_ON_REPO}.sha1" -o ${FILE}.sha1
}

# this performs check of the hashes (if this fails, something might went wrong absolutely)
check() {
  FILE="$1"

  echo "Checking file ${FILE}: "
  if [ -f /usr/bin/md5sum ]; then
     md5sum -b "${FILE}" | cut "-d " -f1 > "${FILE}.md5.temp"
  else
     md5 -q "${FILE}" > "${FILE}.md5.temp"
  fi

  if ! diff --ignore-all-space "${FILE}.md5" "${FILE}.md5.temp" ; then
    echo "Error: MD5 check failed!"
    exit -1
  fi
  echo "  MD5 hash okay"

  shasum -a 1 "${FILE}" | cut "-d " -f1 > "${FILE}.sha1.temp"
  if ! diff --ignore-all-space "${FILE}.sha1" "${FILE}.sha1.temp" ; then
    echo "Error: SHA1 check failed!"
    exit -1
  fi
  echo "  SHA1 hash okay"

  if ! gpg --verify "${FILE}.asc" ; then
    echo "Error: GPG check failed!"
    exit -1
  fi
  echo "  GPG signature okay"

  # todo: change, if maven creates the sha256
  shasum -a 256 "${FILE}" > "${FILE}.sha256"
  echo "  SHA-256 created"

}

# this uploads the files into the svn dist repo
upload() {
  PATTERN="$1"
  FOLDER="$2"

  for file in $(find . -type file -name "${PATTERN}" -exec basename \{\} \; ) ; do
    # echo $file;
    svn import -m "Uploading Trinidad release artifact ${VERSION} to dist server" $file ${DIST_REPOSITORY}/${FOLDER}/$file;
  done
}

# commands

download "org/apache/myfaces/trinidad/trinidad-assembly"         "trinidad-assembly-${VERSION}-dist.tar.gz"            "trinidad-assembly-${VERSION}-dist.tar.gz"
download "org/apache/myfaces/trinidad/trinidad-assembly"         "trinidad-assembly-${VERSION}-dist.zip"               "trinidad-assembly-${VERSION}-dist.zip"
download "org/apache/myfaces/trinidad/trinidad-example-assembly" "trinidad-example-assembly-${VERSION}-example.tar.gz" "trinidad-example-assembly-${VERSION}-example.tar.gz"
download "org/apache/myfaces/trinidad/trinidad-example-assembly" "trinidad-example-assembly-${VERSION}-example.zip"    "trinidad-example-assembly-${VERSION}-example.zip"
download "org/apache/myfaces/trinidad/trinidad"                  "trinidad-${VERSION}-source-release.zip"              "trinidad-${VERSION}-source-release.zip"

check "trinidad-assembly-${VERSION}-dist.tar.gz"
check "trinidad-assembly-${VERSION}-dist.zip"
check "trinidad-example-assembly-${VERSION}-example.tar.gz"
check "trinidad-example-assembly-${VERSION}-example.zip"
check "trinidad-${VERSION}-source-release.zip"

rm -f *.temp

upload "trinidad-assembly-${VERSION}-dist.*"           "binaries"
upload "trinidad-example-assembly-${VERSION}-example.*"        "binaries"
upload "trinidad-${VERSION}-source-release.*" "source"
