package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.io.InputStream;
import java.io.IOException;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.share.io.InputStreamProvider;
import org.apache.myfaces.trinidad.skin.Icon;

class IconInputStreamProvider implements InputStreamProvider
{
  IconInputStreamProvider(Icon icon)
  {
    _icon = icon;
  }

  /**
   * Return an InputStream for the target.  This function
   * should never return null - if a stream cannot be opened,
   * throw an IOException.
   */
  public InputStream  openInputStream() throws IOException
  {
    return _icon.openStream(FacesContext.getCurrentInstance(),
                            RenderingContext.getCurrentInstance());
  }

  /**
   * Returns the name of the target location, suitable
   * for user display.
   */
  public String  getDisplayName()
  {
    return _icon.toString();
  }

  /**
   * Returns an identifier object that uniquely
   * identifies the target location. If two providers
   * return equal identifiers, that is, given:
   * <pre>
   *   Object identifierA = providerA.getIdentifier();
   *   Object identifierB = providerB.getIdentifier();
   * </pre>
   * ... then:
   * <pre>
   *   if (identifierA.equals(identifierB)) ...
   * </pre>
   * then the two providers must point to the same location.
   */
  public Object getIdentifier()
  {
    return _icon;
  }

  /**
   * Returns true if the underlying target has changed
   * since the last call to openInputStream()
   */
  public boolean           hasSourceChanged()
  {
    return false;
  }

  /**
   * Returns the cached result from reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public Object            getCachedResult()
  {
    return _cachedResult;
  }

  /**
   * Stores the cached result of reading and parsing this
   * provider.
   * @see CachingNameResolver
   */
  public void              setCachedResult(Object value)
  {
    _cachedResult = value;
  }

  private Icon   _icon;
  private Object _cachedResult;
}
