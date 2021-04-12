//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
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

  using System;

  public class Command {
    #region Properties
    public Int32? Length { get; set; }
    public Int32? Merges { get; set; }
    public UInt32? InsertionLimit { get; set; }
    public Boolean Print { get; set; }
    public SortCase SortCase { get; set; }
    public UInt32? Trials { get; set; }
    #endregion

    #region Methods
    public void Parse(String[] args) {
      Length = default;
      InsertionLimit = default;
      Merges = default;
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

          case 'm':                     // the merges switch
            if (len > 2)                // whitespace optional
              Merges = token.Substring(2, len - 2).TryParseInt32();
            else if (n < count)         // whitespace allowed
              Merges = args[++n].TryParseInt32();

            usage = !Merges.HasValue;
            break;

          case 'p':                     // the print switch
            if (len > 2)                // superfluous value specified
              usage = true;
            else
              Print = true;
            break;

          case 's':                     // the sort-case switch
            if (len > 2)                // whitespace optional
              SortCase = token.Substring(2, len - 2).ParseEnumFromName<SortCase>();
            else if (n < count)         // whitespace allowed
              SortCase = args[++n].ParseEnumFromName<SortCase>();
            break;

          case 't':                     // the trials switch
            if (len > 2)                // whitespace optional
              Trials = token.Substring(2, len - 2).TryParseUInt32();
            else if (n < count)         // whitespace allowed
              Trials = args[++n].TryParseUInt32();

            usage = !Trials.HasValue;
            break;

          default:                      // switch unknown
            usage = true;
            break;
          }
        }
      }

      // length is required
      if (n < count)
        Length = args[n++].TryParseInt32();

      usage |= !Length.HasValue;

      usage |= n < count;               // superfluous argument specified

      if (usage)                        // throw usage line if parse failed
        throw new CommandException("Usage: MergeSort [-i <insertion-limit>] [-m <merges>] [-p] [-s (ascending | descending | random)] [-t trials] length");

      if (Length.Value >= SortData.LENGTH_MAX)
        throw new CommandException($"LENGTH_MAX = {SortData.LENGTH_MAX} <= length = {Length}");
    }
    #endregion
  }
}
