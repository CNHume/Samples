using System.Text;

namespace Fermat.Extensions {
  public static class Extension {
    #region Constants
    private const string space = " ";
    private const char zero = '0';
    #endregion

    #region Methods
    public static StringBuilder AppendId(
      this StringBuilder sb, decimal id, int width = 10, int grouping = 3) {
      var padded = id.ToString().PadLeft(width, zero);
      var index = 0;
      for (var n = 0; n < 2; n++, index += grouping)
        sb.Append(padded.AsSpan(index, grouping)).Append(space);
      return sb.Append(padded.AsSpan(index));
    }
    #endregion
  }
}
