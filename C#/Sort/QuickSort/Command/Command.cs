//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-10-30 CNHume  Added Command class
//
namespace Sort {
  using SortTest;
  using SortTest.Exceptions;
  using SortTest.Extensions;

  class Command {
    #region Properties
    public Int32? Length { get; set; }
    public UInt32? InsertionLimit { get; set; }
    public Boolean Print { get; set; }
    public SortCase SortCase { get; set; }
    public UInt32? Trials { get; set; }
    #endregion

    #region Methods
    public void Parse(String[] args) {
      Length = default;
      InsertionLimit = default;
      Print = false;
      SortCase = SortData.SORTCASE_DEFAULT;
      Trials = default;

      var usage = false;
      var count = args.Length;
      var n = 0;
      for (; n < count && !usage; n++) {
        var token = args[n];            // parse any switches
        var len = token.Length;
        if (len < 1 || token[0] != '-')
          break;                        // end of switches
        else if (len < 2)
          usage = true;
        else {                          // parse single character switch
          switch (token[1]) {
          case 'i':                     // the insertion-limit switch
            if (len > 2)                // whitespace optional
              InsertionLimit = token.Substring(2, len - 2).TryParseUInt32();
            else if (n < count)         // whitespace allowed
              InsertionLimit = args[++n].TryParseUInt32();

            usage = !InsertionLimit.HasValue;
            break;

          case 'p':                     // the print switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              Print = true;
            break;

          case 's':                     // the sort-case switch
            if (len > 2)                // whitespace optional
              SortCase = token.Substring(2, len - 2).ParseEnumFromName<SortCase>(true);
            else if (n < count)         // whitespace allowed
              SortCase = args[++n].ParseEnumFromName<SortCase>(true);
            break;

          case 't':                     // the trials switch
            if (len > 2)                // whitespace optional
              Trials = token.Substring(2, len - 2).TryParseUInt32();
            else if (n < count)         // whitespace allowed
              Trials = args[++n].TryParseUInt32();

            if (!Trials.HasValue)
              usage = true;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      // Length is required
      if (n < count)
        Length = args[n++].TryParseInt32();

      usage |= n < count;               // superfluous argument specified

      if (usage || !Length.HasValue)    // throw usage line if parse failed
        throw new CommandException("Usage: quicksort [-i <insertion-limit>] [-p] [-s (ascending | descending | random)] [-t trials] length");

      if (Length.Value >= SortData.LENGTH_MAX)
        throw new CommandException($"LENGTH_MAX = {SortData.LENGTH_MAX} <= length = {Length}");
    }
    #endregion
  }
}
