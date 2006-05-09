/*
 * Copyright 2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.filters.output;

import org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.filters.ObfuscatorFilter;
import org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.javascript15parser.AnnotatedToken;
import org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.javascript15parser.Token;
import java.io.BufferedOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;


public class OutputGenerator implements ObfuscatorFilter
{
  private PrintStream _outStream;

  public OutputGenerator()
  {
  }

  public OutputGenerator(OutputStream outStream)
  {
    _outStream = new PrintStream(new BufferedOutputStream(outStream));
  }

  public AnnotatedToken filter(AnnotatedToken startToken)
  {
    AnnotatedToken token = startToken;

    while (token != null)
    {
      printToken(token);
      token = token.getNext();
    }

    _outStream.flush();

    return startToken;
  }

  public void setOutputStream(OutputStream outStream)
  {
    _outStream = new PrintStream(new BufferedOutputStream(outStream));
  }

  public PrintStream getOutputStream()
  {
    return _outStream;
  }

  private void printToken(Token token)
  {
    // print special tokens first
    if (token.specialToken != null)
    {
      printToken(token.specialToken);
    }

    _outStream.print(token.image);
  }
}
