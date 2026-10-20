// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2026-10-11 CNHume  Created Duration class
// 2026-10-15 CNHume  Completed Duration as an RAII performance timer
// 2026-10-17 CNHume  Added auto-scaling of time and rate units
//
#include "Duration.h"

#include <cstdint>                      // for int64_t, uint8_t
#include <ctime>                        // for localtime_r, localtime_s, tm
#include <format>                       // for format
#include <iostream>                     // for cout
#include <limits>                       // for numeric_limits
#include <locale>                       // for locale
#include <string>                       // for string
#include <utility>                      // for move

namespace {
  //
  // The Stopwatch measures elapsed time in microseconds, the reference unit
  // here.  These constants give the interval of each display unit in
  // microseconds.
  //
  constexpr int64_t
    USEC = 1,                           // reference unit: one microsecond
    USEC_PER_MSEC = 1000,
    USEC_PER_SEC = 1000 * USEC_PER_MSEC,
    USEC_PER_MINUTE = 60 * USEC_PER_SEC,
    USEC_PER_HOUR = 60 * USEC_PER_MINUTE,
    USEC_PER_DAY = 24 * USEC_PER_HOUR,
    USEC_PER_WEEK = 7 * USEC_PER_DAY;

  enum class TimeUnits : uint8_t {
    us, ms, sec, min, hours, days, weeks
  };

  enum class RateUnits : uint8_t {
    MHz, KHz, Hz, perMinute, perHour, perDay, perWeek
  };

  //
  // Greatest lower bound: the coarsest time unit whose interval <= us, so the
  // reported value is >= 1.
  //
  TimeUnits getTimeUnits(int64_t us) {
    if (us >= USEC_PER_WEEK)
      return TimeUnits::weeks;
    if (us >= USEC_PER_DAY)
      return TimeUnits::days;
    if (us >= USEC_PER_HOUR)
      return TimeUnits::hours;
    if (us >= USEC_PER_MINUTE)
      return TimeUnits::min;
    if (us >= USEC_PER_SEC)
      return TimeUnits::sec;
    if (us >= USEC_PER_MSEC)
      return TimeUnits::ms;
    return TimeUnits::us;
  }

  //
  // Least upper bound: the finest rate unit whose interval >= usPerOp, so the
  // reported rate is >= 1.
  //
  RateUnits getRateUnits(double usPerOp) {
    if (usPerOp <= (double)USEC)
      return RateUnits::MHz;
    if (usPerOp <= (double)USEC_PER_MSEC)
      return RateUnits::KHz;
    if (usPerOp <= (double)USEC_PER_SEC)
      return RateUnits::Hz;
    if (usPerOp <= (double)USEC_PER_MINUTE)
      return RateUnits::perMinute;
    if (usPerOp <= (double)USEC_PER_HOUR)
      return RateUnits::perHour;
    if (usPerOp <= (double)USEC_PER_DAY)
      return RateUnits::perDay;
    return RateUnits::perWeek;
  }

  int64_t timeInterval(TimeUnits unit) {
    switch (unit) {
    case TimeUnits::weeks: return USEC_PER_WEEK;
    case TimeUnits::days: return USEC_PER_DAY;
    case TimeUnits::hours: return USEC_PER_HOUR;
    case TimeUnits::min: return USEC_PER_MINUTE;
    case TimeUnits::sec: return USEC_PER_SEC;
    case TimeUnits::ms: return USEC_PER_MSEC;
    default: return USEC;
    }
  }

  int64_t rateInterval(RateUnits unit) {
    switch (unit) {
    case RateUnits::MHz: return USEC;
    case RateUnits::KHz: return USEC_PER_MSEC;
    case RateUnits::Hz: return USEC_PER_SEC;
    case RateUnits::perMinute: return USEC_PER_MINUTE;
    case RateUnits::perHour: return USEC_PER_HOUR;
    case RateUnits::perDay: return USEC_PER_DAY;
    default: return USEC_PER_WEEK;
    }
  }

  const char* timeUnitsName(TimeUnits unit) {
    switch (unit) {
    case TimeUnits::us: return "µs";
    case TimeUnits::ms: return "ms";
    case TimeUnits::sec: return "sec";
    case TimeUnits::min: return "min";
    case TimeUnits::hours: return "hours";
    case TimeUnits::days: return "days";
    default: return "weeks";
    }
  }

  const char* rateUnitsName(RateUnits unit) {
    switch (unit) {
    case RateUnits::MHz: return "MHz";
    case RateUnits::KHz: return "KHz";
    case RateUnits::Hz: return "Hz";
    case RateUnits::perMinute: return "per minute";
    case RateUnits::perHour: return "per hour";
    case RateUnits::perDay: return "per day";
    default: return "per week";
    }
  }

  //
  // Format a scaled value with at most three fractional digits, trimming
  // trailing zeros, analogous to the C# "0.0##" custom format.
  //
  std::string formatScaled(double value) {
    auto text = std::format("{:.3f}", value);
    if (text.find('.') != std::string::npos) {
      while (!text.empty() && text.back() == '0')
        text.pop_back();
      if (!text.empty() && text.back() == '.')
        text.pop_back();
    }
    return text;
  }
}

Duration::Duration() {
  sw_.start();
}

Duration::~Duration() {
  sw_.stop();
}

void Duration::Stop() {
  sw_.stop();
}

int64_t Duration::ElapsedMicroseconds() const {
  return sw_.elapsed_microseconds();
}

std::string Duration::ToString(const char* name, long count, int64_t microseconds) {
  auto us = microseconds;

  auto timeUnit = getTimeUnits(us);
  auto timeValue = (double)us / (double)timeInterval(timeUnit);

  auto hasrate = us > 0 && count > 0;
  auto rateValue = std::numeric_limits<double>::quiet_NaN();
  auto rateUnit = RateUnits::MHz;
  if (hasrate) {
    auto usPerOp = (double)us / (double)count;
    rateUnit = getRateUnits(usPerOp);
    rateValue = (double)count * (double)rateInterval(rateUnit) / (double)us;
  }

  return std::format(
    std::locale(),
    "{}: {:L} operations in {} {} @{} {}\n",
    name, count, formatScaled(timeValue), timeUnitsName(timeUnit),
    formatScaled(rateValue), rateUnitsName(rateUnit));
}

std::string Duration::WallClock() {
  using namespace std::chrono;
  auto now = system_clock::now();
  auto tt = system_clock::to_time_t(now);
  std::tm local{};
#if defined(_WIN32)
  localtime_s(&local, &tt);
#else
  localtime_r(&tt, &local);
#endif
  auto centis = duration_cast<milliseconds>(
    now.time_since_epoch() % seconds{ 1 }).count() / 10;
  return std::format("{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:02}",
    local.tm_year + 1900, local.tm_mon + 1, local.tm_mday,
    local.tm_hour, local.tm_min, local.tm_sec, centis);
}
