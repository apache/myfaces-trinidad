package org.apache.myfaces.trinidad.validator;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.model.DateListProvider;

public class TestDateListProvider implements DateListProvider
{
  
  List<Date> germanHolidays = null;
  
  public TestDateListProvider()
  {
    germanHolidays = new ArrayList<Date>();
    germanHolidays.add(newDate("01.01.2007"));
    germanHolidays.add(newDate("01.05.2007"));
    germanHolidays.add(newDate("15.05.2007"));
  }
  

  private Date newDate(String string)
  {
    SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
    Date ret = null;
    try
    {
      ret = sdf.parse(string);
    } catch (ParseException e)
    {
      e.printStackTrace();
    }
    return ret;
  }


  public List<Date> getDateList(FacesContext context, Calendar base,
      Date rangeStart, Date rangeEnd)
  {
    
    List<Date> returnDates = new ArrayList<Date>();
    
    for (Date it : germanHolidays)
    {
      if(!it.before(rangeStart) && !it.after(rangeEnd)){
        base.setTime(it);
        returnDates.add(base.getTime());
      }
    }
    
    return returnDates;
  }

}
