package org.apache.myfaces.trinidad.change;

import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.component.ContextCallback;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.model.RowKeySet;

public class RowKeySetAttributeChange extends AttributeComponentChange
{
  public RowKeySetAttributeChange(String clientId,  String propertyName, Object value)
  {
    super(propertyName, value);
    
    if ((clientId == null) || (clientId.length() == 0))
      throw new IllegalArgumentException("No clientId specified");

    _clientId = clientId;
  }

  @Override
  @SuppressWarnings("deprecation")
  public void changeComponent(UIComponent uiComponent)
  {
    Map<String, Object> attributeMap = uiComponent.getAttributes();

    Object attributeValue = getAttributeValue();
    String attributeName  = getAttributeName();
    
    // if the attributevalue is a ValueExpression or ValueBinding, use the
    // appropriate setValueExpression/setValueBinding call and remove the
    // current attribute value, if any, so that the ValueExpression/ValueBinding
    // can take precedence
    if (attributeValue instanceof ValueExpression)
    {
      uiComponent.setValueExpression(attributeName, (ValueExpression)attributeValue);
      attributeMap.remove(attributeName);
    }
    else if (attributeValue instanceof ValueBinding)
    {
      uiComponent.setValueBinding(attributeName, (ValueBinding)attributeValue);
      attributeMap.remove(attributeName);
    }
    else
    {
      boolean putValue = true;
      
      // Specially handle RowKeySet case by replacing the contents of the RowKeySet in-place
      // rather than replacing the entire object.  This keeps the mutable object instance from
      // changing
      if (attributeValue instanceof RowKeySet)
      {
        ValueExpression expression = uiComponent.getValueExpression(attributeName);

        Object oldValue;
        
        if (expression != null)
        {
          final FacesContext context = FacesContext.getCurrentInstance();
          
          RowKeySet[] outHolder = new RowKeySet[1];
          
          context.getViewRoot().invokeOnComponent(context,
                                                 _clientId,
                                                 new ExpressionEvaluator(expression, outHolder));
          
          oldValue = outHolder[0];
        }
        else
        {
          oldValue = attributeMap.get(attributeName);
        }
                
        if (oldValue instanceof RowKeySet)
        {
          RowKeySet oldKeySet = (RowKeySet)oldValue;
          
          // check for equality because otherwise we would clear ourselves and end up empty
          if (oldKeySet != attributeValue)
          {
            oldKeySet.clear();
            oldKeySet.addAll((RowKeySet)attributeValue);
          }
          
          // don't replace the RowKeySet
          putValue = false;
        }
      }
      
      
      if (putValue)
      {
        attributeMap.put(attributeName, attributeValue);
      }
    }
  }
  
  private static final class ExpressionEvaluator implements ContextCallback
  {
    public ExpressionEvaluator(ValueExpression expression, RowKeySet[] out)
    {
      _out        = out;
      _expression = expression;
    }
    public void invokeContextCallback(FacesContext context,
                                      UIComponent target)
    {
      _out[0] = (RowKeySet)_expression.getValue(context.getELContext());
    }
    
    private final RowKeySet[] _out;
    private final ValueExpression _expression;
  }

  private static final long serialVersionUID = 1L;
  
  private final String _clientId;
}
