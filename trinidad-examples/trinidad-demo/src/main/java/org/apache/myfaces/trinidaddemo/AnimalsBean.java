/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidaddemo;

import java.util.ArrayList;
import java.util.List;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.event.RangeChangeEvent;

public class AnimalsBean implements java.io.Serializable
{
  public AnimalsBean()
  {
    _names = new ArrayList<String>();
    _names.add("Aardvark");
    _names.add("Albatross ");
    _names.add("Alligator");
    _names.add("Anaconda");
    _names.add("Antelope");
    _names.add("Armadillo");
    _names.add("Baboon");
    _names.add("Badger");
    _names.add("Bald eagle");
    _names.add("Bear");
    _names.add("Beaver");
    _names.add("Bee");
    _names.add("Bison"); 
    _names.add("Bobcat");
    _names.add("Butterfly");
    _names.add("Camel");
    _names.add("Capybara");
    _names.add("Cardinal");
    _names.add("Caterpillar");
    _names.add("Catfish");
    _names.add("Chameleon");
    _names.add("Cheetah");
    _names.add("Cooper's hawk");
    _names.add("Cougar");
    _names.add("Coyote");
    _names.add("Crab"); 
    _names.add("Cricket");    
    _names.add("Crane");  
    _names.add("Daddy longlegs");
    _names.add("Deer mouse");
    _names.add("Dog");
    _names.add("Dolphin");
    _names.add("Donkey");
    _names.add("Dragonfly"); 
    _names.add("Duck");
    _names.add("Eagle");
    _names.add("Eel");
    _names.add("Egret");
    _names.add("Elephant");
    _names.add("Falcon");
    _names.add("Fangtooth");
    _names.add("Fantail");
    _names.add("Finch");
    _names.add("Flycatcher");     
    _names.add("Fox");
    _names.add("Frog");     
    _names.add("Gecko");    
    _names.add("Gorilla");
    _names.add("Grasshopper"); 
    _names.add("Grouse");      
    _names.add("Gorilla"); 
    
    _names.add("Hedgehog");    
    _names.add("Heron");
    _names.add("Horned lizard"); 
    _names.add("Hyena");  
    
    _names.add("Iguana");    
    _names.add("Ivory-billed woodpecker");
    
    _names.add("Jackal"); 
    _names.add("Jaguar");      
    _names.add("Jellyfish"); 
 
    _names.add("Kingfisher"); 
    _names.add("Kite");      
    _names.add("Koala"); 
 
    _names.add("Ladybug"); 
    _names.add("Lark");      
    _names.add("Lion"); 
    
 
    _names.add("Macaw"); 
    _names.add("Mandrill");      
    _names.add("Margay"); 
    _names.add("Merlin"); 
    _names.add("Mockingbird"); 
    _names.add("Monkey"); 
    _names.add("Mouse");
 
    _names.add("Turkey"); 
    _names.add("Vampire Bat");      
    _names.add("Wolf"); 
    _names.add("Woodpecker"); 
    _names.add("Yellow-bellied marmot"); 
    _names.add("Zebra"); 
    _names.add("Zorilla");    
    _end = _BLOCK_SIZE;
    _namesInRange = _names.subList(_start, _end);
  }

  public List<String> getNames()
  {
    return _names;
  }
  
  public List<String> getNamesInRange()
  {

    return _namesInRange;
  }
  

  public int getStart()
  {
    return _start;
  }


  public int getEnd()
  {
    return _end;
  }


  
  public void rangeChange(RangeChangeEvent rce)
  {
    int oldStart = rce.getOldStart();
    int oldEnd   = rce.getOldEnd();    
    int newStart = rce.getNewStart();
    int newEnd   = rce.getNewEnd();   
    
    FacesContext context = FacesContext.getCurrentInstance();
    
    String message =  "Range changed from " 
                      + oldStart +"-"+oldEnd+" to "
                      + newStart +"-"+newEnd;
                      
    context.addMessage(rce.getComponent().getClientId(context),
                       new FacesMessage(message));
                       
    _setStart(newStart);
    _setEnd(newEnd);
    _setNamesInRange(new ArrayList<String>(getNames().subList(_start, getEnd())));
  } 

  private void _setStart(int start)
  {
    _start = start;

  }
  
  private void _setEnd(int end)
  {
    _end = end;   
  } 
  
  private void _setNamesInRange(List<String> namesInRange)
  {
    _namesInRange = namesInRange;

  } 
  // start is inclusive of the range, end is exclusive. If you are showing
  // the first 5 items, _start = 0, _end = 5.
  private int _start = 0;  
  private int _end;
  
  private List<String> _names;
  
  private List<String> _namesInRange;
  
  private static final int _BLOCK_SIZE = 5;

}
