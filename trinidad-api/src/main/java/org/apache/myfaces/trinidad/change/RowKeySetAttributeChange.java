package org.apache.myfaces.trinidad.change;

import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.component.ContextCallback;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.model.RowKeySet;

/**
 * Handles RowKeySetAttribute changes, which need to be handled specially because they are mutable
 * and programmers assume that the instances don't change
 */
public final class RowKeySetAttributeChange extends AttributeComponentChange
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
      // Specially handle RowKeySet case by replacing the contents of the RowKeySet in-place
      // rather than replacing the entire object.  This keeps the mutable object instance from
      // changing
      if (attributeValue instanceof RowKeySet)
      {
        ValueExpression expression = uiComponent.getValueExpression(attributeName);
        final FacesContext context = FacesContext.getCurrentInstance();
        
        if (expression != null)
        {
          //use EL to get the oldValue and then determine whether we need to update in place
          context.getViewRoot().invokeOnComponent(
            context,
            _clientId,
            new GetOldValueAndUpdate(expression, (RowKeySet)attributeValue));
        }
        else
        {
          context.getViewRoot().invokeOnComponent(context, _clientId,
                 new GetOldValueAndUpdate(attributeName, (RowKeySet)attributeValue));
        
        }       
      }
      
      attributeMap.put(attributeName, attributeValue);
    }
  }
  
  private static void _updateKeySet(String clientId, RowKeySet oldKeySet, RowKeySet newKeySet)
  {
    // check for equality because otherwise we would clear ourselves and end up empty
    if (oldKeySet != newKeySet)
    {
      // no client id, so we're in context
      if (clientId == null)
      {
        oldKeySet.clear();
        oldKeySet.addAll(newKeySet);        
      }
      else
      {
        final FacesContext context = FacesContext.getCurrentInstance();
        
        context.getViewRoot().invokeOnComponent(
           context,
           clientId,
           new RowKeySetUpdater(oldKeySet, newKeySet));
      }
    }    
  }
  
  /**
   * Get the oldValue in context and update it in context
   */
  private static final class GetOldValueAndUpdate implements ContextCallback
  {
    public GetOldValueAndUpdate(ValueExpression expression, RowKeySet newKeySet)
    {
      _attributeName = null;
      _expression = expression;
      _newKeySet  = newKeySet;
    }
    public GetOldValueAndUpdate(String attributeName, RowKeySet newKeySet)
    {
      _expression = null;
      _attributeName = attributeName;
      _newKeySet  = newKeySet;
    }

    public void invokeContextCallback(FacesContext context,
                                      UIComponent target)
    {
      if (_expression != null)
      {
        // update the KeySet with the old and new values
        RowKeySetAttributeChange._updateKeySet(null,
                                             (RowKeySet)_expression.getValue(context.getELContext()),
                                             _newKeySet);
      }
      else 
      {
        Map<String, Object> attributeMap = target.getAttributes();
        RowKeySet oldKeySet = (RowKeySet) attributeMap.get(_attributeName);

        // update the KeySet with the old and new values
        RowKeySetAttributeChange._updateKeySet(null,
                                             oldKeySet,
                                             _newKeySet);
      }
    }
    
    private final ValueExpression _expression;
    private final String _attributeName;
    private final RowKeySet _newKeySet;
  }

  /**
   * Makes sure that we clear and add the RowKeySet in context
   */
  private static final class RowKeySetUpdater implements ContextCallback
  {
    public RowKeySetUpdater(RowKeySet oldKeySet, RowKeySet newKeySet)
    {
      _oldKeySet = oldKeySet;
      _newKeySet = newKeySet;
    }

    public void invokeContextCallback(FacesContext context,
                                      UIComponent target)
    {
      _oldKeySet.clear();
      _oldKeySet.addAll(_newKeySet);
    }
    
    private final RowKeySet _oldKeySet;
    private final RowKeySet _newKeySet;
  }

  private static final long serialVersionUID = 1L;
  
  private final String _clientId;
}
