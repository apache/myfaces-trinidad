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
package org.apache.myfaces.adfbuild.plugin.mock.maker;

import com.mockobjects.ExpectationCounter;
import com.mockobjects.ExpectationList;
import com.mockobjects.ReturnValues;

public class MockMockMaker extends MockMaker{
   private ExpectationCounter myMainCalls = new ExpectationCounter("mockmaker.MockMaker MainCalls");
   private ExpectationList myMainParameter0Values = new ExpectationList("mockmaker.MockMaker MainParameter0Values");
   private ExpectationCounter myGetClassLoaderCalls = new ExpectationCounter("mockmaker.MockMaker GetClassLoaderCalls");
   private ReturnValues myActualGetClassLoaderReturnValues = new ReturnValues(false);
   private ExpectationCounter myMakeCalls = new ExpectationCounter("mockmaker.MockMaker MakeCalls");
   private ExpectationCounter myGetValidLoaderCalls = new ExpectationCounter("mockmaker.MockMaker GetValidLoaderCalls");
   private ReturnValues myActualGetValidLoaderReturnValues = new ReturnValues(false);
   private ExpectationCounter mySetClassLoaderCalls = new ExpectationCounter("mockmaker.MockMaker SetClassLoaderCalls");
   private ExpectationList mySetClassLoaderParameter0Values = new ExpectationList("mockmaker.MockMaker SetClassLoaderParameter0Values");

   public MockMockMaker() {
    super("", null, null);
   }


   public void setExpectedMainCalls(int calls){
      myMainCalls.setExpected(calls);
   }
   public void addExpectedMainValues(String[] arg0){
      myMainParameter0Values.addExpected(arg0);
   }

   public void setExpectedGetClassLoaderCalls(int calls){
      myGetClassLoaderCalls.setExpected(calls);
   }
   public ClassLoader getClassLoader(){
      myGetClassLoaderCalls.inc();
      Object nextReturnValue = myActualGetClassLoaderReturnValues.getNext();
      return (ClassLoader) nextReturnValue;
   }
   public void setupGetClassLoader(ClassLoader arg){
      myActualGetClassLoaderReturnValues.add(arg);
   }
   public void setExpectedMakeCalls(int calls){
      myMakeCalls.setExpected(calls);
   }
   public void make() throws ClassNotFoundException{
      myMakeCalls.inc();
   }
   public void setExpectedGetValidLoaderCalls(int calls){
      myGetValidLoaderCalls.setExpected(calls);
   }
   public ClassLoader getValidLoader(){
      myGetValidLoaderCalls.inc();
      Object nextReturnValue = myActualGetValidLoaderReturnValues.getNext();
      return (ClassLoader) nextReturnValue;
   }
   public void setupGetValidLoader(ClassLoader arg){
      myActualGetValidLoaderReturnValues.add(arg);
   }
   public void setExpectedSetClassLoaderCalls(int calls){
      mySetClassLoaderCalls.setExpected(calls);
   }
   public void addExpectedSetClassLoaderValues(ClassLoader arg0){
      mySetClassLoaderParameter0Values.addExpected(arg0);
   }
   public void setClassLoader(ClassLoader arg0){
      mySetClassLoaderCalls.inc();
      mySetClassLoaderParameter0Values.addActual(arg0);
   }
   public void verify(){
      myMainCalls.verify();
      myMainParameter0Values.verify();
      myGetClassLoaderCalls.verify();
      myMakeCalls.verify();
      myGetValidLoaderCalls.verify();
      mySetClassLoaderCalls.verify();
      mySetClassLoaderParameter0Values.verify();
   }
}
