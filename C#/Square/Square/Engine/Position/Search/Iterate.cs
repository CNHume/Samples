//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-07-04 CNHume]Created File
//
// Conditionals:
//
#define RefreshPV
#define WritePV
#define DisplayDepth
#define DisplayRate
//#define DisplayPrediction
//#define TestRegression
#define UseMoveSort
#define ThrowFinal

namespace Engine {
  using System;
  using System.Diagnostics;
  using System.Text;

  using Command;

  using Exceptions;

  using static System.Math;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Depth = UInt16;
  using Eval = Int16;
  using PlyDepth = Byte;

  partial class Position : Board {
    #region Constants
    protected static Eval[] AspirationDelta =
      //{ 16, 40, 108, 256 };
      //{ 16, 42, 110, 288 };
      //{ 200 };
      //{ 64, 96, 160, 256 };
      //{ 75, 150, 225 };
      //{ 96, 192 };
      //{ mPawnWeight + mHalfWeight, mRookWeight + mQuarterWeight };
      { mBishopWeight + mHalfWeight };
    #endregion

    #region Search Methods
    [Conditional("ThrowFinal")]
    private void throwFinalPosition() {
      //[Assume]TurnFlags have been set by search()
      if (IsFinal() || IsInsufficient()) {
        String sMessage;
        if (IsInsufficient())
          sMessage = sTextInsufficient;
        else if (InCheck())
          sMessage = sTextCheckmate;
        else
          sMessage = sTextStalemate;
        throw new FinalPositionException(sMessage);
      }
    }

    private Eval beginIteration(PlyDepth vDepth, Eval mValue) {
      State!.StartDepth = vDepth;
      var sb = new StringBuilder();
      var bWTM = WTM();
      var nLen = AspirationDelta.Length;

      var nLo = State!.IsAspiration ? 0 : nLen;
      var nHi = State!.IsAspiration ? 0 : nLen;

      var mAlpha = EvalUndefined;
      var mBeta = EvalUndefined;

      while (true) {
        if (nLo < nLen &&
            EvalUndefined < mAlpha &&
            mValue <= mAlpha)
          nLo++;                        // Failed Lo

        if (nLen <= nLo || mValue - AspirationDelta[nLo] <= -MateMin)
          mAlpha = MinusInfinity;
        else if (mValue < MateMin)
          mAlpha = (Eval)(mValue - AspirationDelta[nLo]);
        else if (mAlpha == EvalUndefined)
          mAlpha = MinusInfinity;

        if (nHi < nLen &&
            EvalUndefined < mBeta &&
            mBeta <= mValue)
          nHi++;                        // Failed Hi

        if (nLen <= nHi || MateMin <= mValue + AspirationDelta[nHi])
          mBeta = PlusInfinity;
        else if (-MateMin < mValue)
          mBeta = (Eval)(mValue + AspirationDelta[nHi]);
        else if (mBeta == EvalUndefined)
          mBeta = PlusInfinity;

        if (State!.IsAspiration && UCI.IsDebug) {
          var mEvalAlpha = ReflectValue(bWTM, mAlpha);
          var mEvalBeta = ReflectValue(bWTM, mBeta);

          sb.Append("searchRoot(")      // Display Aspiration Window
            .Append("Alpha =").AppendEvalTerm(mEvalAlpha)
            .Append(", Beta =").AppendEvalTerm(mEvalBeta)
            .Append(") Lo = ").Append(nLo)
            .Append(", Hi = ").Append(nHi);

          LogInfo(Level.note, sb.ToString());
          sb.Clear();
        }

        //
        // Any Variation[] previously reported will now be overwritten.
        // Reset # of PV found in the current iteration:
        //
        State!.VariationCount = 0;

        //
        // Enter recursive search:
        //
        var wDraft = draft(vDepth);
        mValue = search(wDraft, mAlpha, mBeta);
        throwFinalPosition();

        if (mAlpha < mValue && mValue < mBeta)
          break;                        // Aspiration Window was adequate;
        else if (UCI.IsDebug) {         // else loop with a larger Window
          var mEval = ReflectValue(bWTM, mValue);
          var sFailed = mValue < mBeta ? "Lo" : "Hi";
          sb.Append("Eval").AppendEvalTerm(mEval)
            .AppendFormat($" failed {sFailed} at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");

          LogInfo(Level.note, sb.ToString());
          sb.Clear();
        }
      }

      return mValue;
    }

    private void endIteration(PlyDepth vDepth) {
      //
      // Annotate PV abbreviations and refresh XP
      //
      refreshPV((Depth)vDepth);         //[Conditional]

      //
      // Display the Principal Variation(s)
      //
      writeMultiPV();                   //[Conditional]
    }

    public Eval IteratePlies(SearchBound bound) {
      var mValue = EvalUndefined;       // Return Value
      var vDepthLimit = bound.Plies;
      var wMovesToMate = bound.MovesToMate;
#if DisplayDepth
      var sw = State!.IterationTimer;
      if (sw == null)
        throw new PositionException("Null IterationTimer Stopwatch");
      else
        sw.Start();

      var qTotal1 = (UInt64)State!.NodeTotal;
#if DisplayPrediction
      var qPredicted1 = 0UL;
#endif
#endif
      var vStartDepth = vStartDepthDefault;
      if (vDepthLimit.HasValue && vDepthLimit < vStartDepth)
        vStartDepth = vDepthLimit.Value;

      for (var vDepth = vStartDepth;
           !vDepthLimit.HasValue || vDepth <= vDepthLimit;
           vDepth++) {
#if DisplayDepth
        if (UCI.IsDebug) {
          LogInfoNewLine(Level.note);
          LogInfo(Level.note, $"Depth = {vDepth} at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
        }
#endif
        mValue = beginIteration(vDepth, mValue);
        endIteration(vDepth);
#if DisplayDepth
        if (UCI.IsDebug) {
          sw.Stop();
          var dElapsedMS = (Double)sw.ElapsedMilliseconds;
          var qTotal2 = (UInt64)State!.NodeTotal;
          var qNodeDelta = qTotal2 - qTotal1;

          GameState.displayRate(dElapsedMS, qNodeDelta);
#if DisplayPrediction
          var qPredicted2 =
#if !TestRegression                     // Elide final prediction
            vDepth == vDepthLimit ? 0UL :
#endif
          State!.Predict(vStartDepth, vDepth, qNodeDelta);

          GameState.DisplayPrediction(dElapsedMS, qNodeDelta, qPredicted1, qPredicted2);
          qPredicted1 = qPredicted2;
#endif                                  // DisplayPrediction
          qTotal1 = qTotal2;
          sw.Restart();
        }
#endif                                  // DisplayDepth
        if (wMovesToMate.HasValue) {
          var mAbs = Abs(mValue);
          if (MateMin <= mAbs && MateMax <= mAbs + wMovesToMate)
            break;                      // End search early if a MovesToMate bound has been satisfied
        }
      }

      return mValue;
    }

    public void IterateCases() {
#if DisplayDepth
      var sw = State!.IterationTimer;
      if (sw == null)
        throw new PositionException("Null IterationTimer Stopwatch");
      else
        sw.Start();

      var qTotal1 = State!.NodeTotal;
#if DisplayPrediction
      var qPredicted1 = 0UL;
#endif
#endif
      var testCases = getTestCases();

      var pc = State!.Case;
      foreach (var tc in testCases) {
        var vDepth = tc.Plies;
#if DisplayDepth
        if (UCI.IsDebug) {
          LogInfoNewLine(Level.note);
          LogInfo(Level.note, $"Depth = {vDepth} at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
        }
#endif
        //[Init]Reset PerfCase counts prior to the recursive search for each test case
        pc.Clear();
        perft(vDepth);
#if DisplayDepth
        if (UCI.IsDebug) {
          sw.Stop();
          var dElapsedMS = (Double)sw.ElapsedMilliseconds;
          var qNodeDelta = pc.TotalNodes.HasValue ? pc.TotalNodes.Value : 0;
          GameState.displayRate(dElapsedMS, qNodeDelta);
#if DisplayPrediction
          var qPredicted2 = State!.Predict(0, vDepth, qNodeDelta);
          GameState.DisplayPrediction(dElapsedMS, qNodeDelta, qPredicted1, qPredicted2);
          qPredicted1 = qPredicted2;
#endif                                  // DisplayPrediction
          qTotal1 = State!.NodeTotal;
          sw.Restart();
        }
#endif                                  // DisplayDepth
        if (!tc.Passed(pc))
          LogInfo(Level.error, $"{Name} failed at Depth = {tc.Plies}");
      }
    }
    #endregion
  }
}
