package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import junit.framework.TestCase;

import java.util.Arrays;
import java.util.List;

public class OutputUtilsTest extends TestCase
{
  public OutputUtilsTest(String name)
  {
    super(name);
  }

  public void testStyleClassListParsing() throws Exception
  {
    assertEquals(OutputUtils.parseStyleClassList("a"), null);
    assertEquals(OutputUtils.parseStyleClassList(" a "),
                 Arrays.asList("a"));
    assertEquals(OutputUtils.parseStyleClassList("a b c"),
                 Arrays.asList("a", "b", "c"));
    assertEquals(OutputUtils.parseStyleClassList("a   b   c"),
                 Arrays.asList("a", "b", "c"));
    assertEquals(OutputUtils.parseStyleClassList("ab c"),
                 Arrays.asList("ab", "c"));
  }
}

