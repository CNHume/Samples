using System;
using System.Diagnostics;

using static Fermat.Parsers.Parser;

namespace Fermat.Parsers {
  class ParserTest {
    #region Methods
    public static void TestFormatAndParse(decimal id) {
      var formattedId = FormatNavigatorId(id);
      var telephoneId = ParseTelephoneID(formattedId);
      Debug.Assert(telephoneId == id, $"telephoneId = {telephoneId} != id = {id}");
      Console.WriteLine($"{telephoneId} = {formattedId}");
      #endregion
    }
  }
}
