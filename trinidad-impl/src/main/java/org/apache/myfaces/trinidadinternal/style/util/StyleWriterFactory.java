package org.apache.myfaces.trinidadinternal.style.util;

import java.io.PrintWriter;

/**
 * Factory that creates {@link PrintWriter} instances for the Skin engine to write content
 * onto. Each call to the create writer function must return a new instance of a print writer.
 * <p>This method is used to write multiple CSS files for a skin to be able to span CSS selectors
 * over multiple files</p>
 */
public interface StyleWriterFactory
{
  /**
   * Create a print writer to write to a new file. The previous writer, if it exists, should
   * be closed by this method call
   * @return a new instance of a print writer
   */
  PrintWriter createWriter();
}
