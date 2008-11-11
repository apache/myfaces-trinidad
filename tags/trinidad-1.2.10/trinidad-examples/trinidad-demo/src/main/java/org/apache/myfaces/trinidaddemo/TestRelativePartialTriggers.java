package org.apache.myfaces.trinidaddemo;


import java.awt.Color;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.faces.event.ValueChangeEvent;

import org.apache.myfaces.trinidad.component.core.data.CoreTable;
import org.apache.myfaces.trinidad.component.core.input.CoreInputText;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandButton;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;
import org.apache.myfaces.trinidad.event.SelectionEvent;

public class TestRelativePartialTriggers {


  public String commandButton1_action() 
  {
      // Add event code here...
    if (commandButton2 != null)
      commandButton2.setText("Found!");
    if (inputText1 != null)
      inputText1.setValue("inputText1 found commandButton1");
    if (inputText2 != null)
      inputText2.setValue("inputText2 found commandButton1");
    if (inputText3 != null)
      inputText3.setContentStyle("background-color:green");
    if (inputText4 != null)
      inputText4.setValue("inputText4 found commandButton1");
    if (table1 != null)
      table1.setInlineStyle("background-color: red");
    if (table2 != null)
      table2.setInlineStyle("background-color: aqua");    
    
    return null;
  } 
  
  public String commandButton3_action() 
  {

    if (inputText1 != null)
      inputText1.setValue("inputText1 found commandButton3");
    if (inputText2 != null)
      inputText2.setValue("inputText2 found commandButton3");
    if (inputText3 != null)
      inputText3.setDisabled(true);
    // Add event code here...
    if (table1 != null)
      table1.setInlineStyle("background-color: pink");
    
    return null;
  }  
  
  
  public void setCommandButton2(CoreCommandButton cb2) 
  {
    this.commandButton2 = cb2;
  }

  public CoreCommandButton getCommandButton2() 
  {
    return commandButton2;
  } 
  
  public void setInputText1(CoreInputText inputText1) 
  {
    this.inputText1 = inputText1;
  }

  public CoreInputText getInputText1() 
  {
    return inputText1;
  }
  
  public void setInputText2(CoreInputText inputText2) 
  {
    this.inputText2 = inputText2;
  }

  public CoreInputText getInputText2() 
  {
    return inputText2;
  }   
  
  public void setInputText3(CoreInputText inputText3) 
  {
    this.inputText3 = inputText3;
  }

  public CoreInputText getInputText3() 
  {
    return inputText3;
  }
  
  public void setInputText4(CoreInputText inputText4) 
  {
    this.inputText4 = inputText4;
  }

  public CoreInputText getInputText4() 
  {
    return inputText4;
  } 
  
  public void setTable1(CoreTable table1) 
  {
    this.table1 = table1;
  }

  public CoreTable getTable1() 
  {
    return table1;
  }  
  
  public void setTable2(CoreTable table2) 
  {
    this.table2 = table2;
  }

  public CoreTable getTable2() 
  {
    return table2;
  }   
  
  public List<Person> getTableData() 
  {
    if (tableData == null)
    {
      tableData = new ArrayList<Person>();
      tableData.add(new Person("Bob"));
      tableData.add(new Person("Kat"));
    }
    return tableData;
  }

  public String commandButton4_action()
  {
    // Add event code here...
    // refresh the table
    if (table1 != null)
      table1.setInlineStyle("background-color: gold");
    return null;
  }
  
  public String getInputText5Value()
  {
    return inputText5Value;
  }

  public void setInputText5Value(String value)
  {
    inputText5Value = value;
  }
  
  /* table demo (between columns), in/out of table, etc */
  public void setOutputText1x(CoreOutputText outputText1x) {
    this.outputText1x = outputText1x;
  }

  public CoreOutputText getOutputText1x() {
    return outputText1x;
  } 
  
  public void setCommandButton1x(CoreCommandButton cb1x) 
  {
    this.commandButton1x = cb1x;
  }

  public CoreCommandButton getCommandButton1x() 
  {
    return commandButton1x;
  } 
  
  public void input1xChanged(ValueChangeEvent vce)
  {
    if (outputText1x != null)
      outputText1x.setInlineStyle("color: red");   
    if (commandButton1x != null)
      commandButton1x.setInlineStyle("color: red");
 
  }
  
  public String commandButton1x_action() 
  {
    if (outputText1x != null)
      outputText1x.setInlineStyle("color: pink");  
    return null;
  }
  
  public void tableRowSelected(SelectionEvent se)
  {
    if (outputText1x != null)
      outputText1x.setInlineStyle("color: aqua"); 
    if (commandButton1x != null)
      commandButton1x.setInlineStyle("color: aqua"); 
  }
  

  public static class Person 
  {
    public Person(String name)
    {
      _name = name;
    }
    private String _name;
    
    public String getName()
    {
      return _name;
    }
    
    public void setName(String name)
    {
      _name = name;
    }
  }
  private CoreCommandButton commandButton2;
  private CoreInputText inputText1;
  private CoreInputText inputText2;   
  private CoreInputText inputText3;
  private CoreInputText inputText4;
  private String         inputText5Value;

  private CoreTable table1;
  private CoreTable table2;  
  private List<Person> tableData;
  
  // table specific demos (between columns, etc)
  private CoreOutputText outputText1x;
  private CoreCommandButton commandButton1x;

  

}
