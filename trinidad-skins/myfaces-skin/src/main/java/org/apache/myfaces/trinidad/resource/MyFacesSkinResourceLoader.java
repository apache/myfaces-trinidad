/**
 * 
 */
package org.apache.myfaces.trinidad.resource;

/**
 *
 * @author Andrew Robinson
 */
public class MyFacesSkinResourceLoader
  extends RegexResourceLoader
{
  public MyFacesSkinResourceLoader()
  {
    register("(/.*\\.(jgp|gif|png|jpeg|cur))", new MyFacesSkinImageResourceLoader());
  }
}
