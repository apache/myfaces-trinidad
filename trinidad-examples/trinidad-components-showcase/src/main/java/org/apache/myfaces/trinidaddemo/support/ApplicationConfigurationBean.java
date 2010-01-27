/**
 * 
 */
package org.apache.myfaces.trinidaddemo.support;

import java.io.Serializable;

/**
 * 
 */
public class ApplicationConfigurationBean implements Serializable {

	private static final long serialVersionUID = -911848815332920525L;

	private boolean jsBasedCodeFormatting;

	/**
	 * @return the jsBasedCodeFormatting
	 */
	public boolean isJsBasedCodeFormatting() {
		return jsBasedCodeFormatting;
	}

	/**
	 * @param jsBasedCodeFormatting the jsBasedCodeFormatting to set
	 */
	public void setJsBasedCodeFormatting(boolean jsBasedCodeFormatting) {
		this.jsBasedCodeFormatting = jsBasedCodeFormatting;
	}
}
