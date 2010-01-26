package org.apache.myfaces.trinidadinternal.renderkit.core.skin;

import org.apache.myfaces.trinidadinternal.skin.SkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidad.skin.Skin;

/**  Class for the Casablanca portlet skin
 *
 * @author Marius Petoi
 */
public class CasablancaPortletSkin extends SkinExtension {

  /**
   * Constructs a CasablancaPortletSkin instance
   */
  public CasablancaPortletSkin(Skin baseSkin)
  {
    // Create a SkinExtension for Casablanca
    super(baseSkin,
          _CASABLANCA_PDA_ID,
          TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
          TrinidadRenderingConstants.APACHE_TRINIDAD_PORTLET);

    // Register our style sheet
    setStyleSheetName(_CASABLANCA_STYLE_SHEET_NAME);
  }

  // Casablanca skin id
  private static final String _CASABLANCA_PDA_ID = "casablanca.portlet";

  // Casablanca skin style sheet name
  private static final String _CASABLANCA_STYLE_SHEET_NAME =
    "META-INF/adf/styles/casablancaSkin.css";
}
