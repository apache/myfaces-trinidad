/**
 * 
 */
package org.apache.myfaces.trinidaddemo.support.jsf;

import javax.faces.context.FacesContext;

/**
 *
 */
public class JsfHelper {

	/**
	 * @param name
	 * @return
	 */
	public static Object getBean(String name) {
		FacesContext facesContext = FacesContext.getCurrentInstance();
		return facesContext.getApplication().getELResolver().getValue(facesContext.getELContext(), null, name);
	}
}
