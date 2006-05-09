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
package org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.filters.obfuscation.state;

import org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.javascript15parser.NameGen;
import org.apache.myfaces.adfbuild.plugin.javascript.obfuscator.javascript15parser.ProgramContext;


public class FunctionContext extends ProgramContext {
    private boolean _obfuscateLocalVars;

    public FunctionContext(String name, boolean obfuscateLocalVars) {
        super(name);
        _obfuscateLocalVars = obfuscateLocalVars;
    }

    public FunctionContext(String name, NameGen nameGen, boolean usesEval) {
        super(name, nameGen);
        _obfuscateLocalVars = usesEval;
    }

    public void setObfuscateLocalVars(boolean obfuscateLocalVars) {
        _obfuscateLocalVars = obfuscateLocalVars;
    }

    public boolean canObfuscateLocalVars() {
        return _obfuscateLocalVars;
    }
}
