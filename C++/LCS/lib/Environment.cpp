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
// 2026-10-19 CNHume  Created Environment class
//
#include "Environment.h"

#include <cstdint>                      // for uint32_t, uint64_t
#include <format>                       // for format
#include <locale>                       // for locale
#include <string>

#if defined(_WIN32)
  #define NOMINMAX
  #include <windows.h>                  // before using-directives: windows.h defines ::byte
#elif defined(__linux__)
  #include <fstream>                    // for ifstream
  #include <unistd.h>                   // for sysconf
#endif

using namespace std;
using namespace lcs;

namespace {
  //
  // Total physical memory in bytes, or 0 when unknown.
  //
  uint64_t totalBytes() {
#if defined(_WIN32)
    MEMORYSTATUSEX mem{};
    mem.dwLength = sizeof(mem);
    return GlobalMemoryStatusEx(&mem) ? mem.ullTotalPhys : 0;
#elif defined(__linux__)
    auto pages = sysconf(_SC_PHYS_PAGES);
    auto pageSize = sysconf(_SC_PAGESIZE);
    if (pages > 0 && pageSize > 0)
      return (uint64_t)pages * (uint64_t)pageSize;
    return 0;
#else
    return 0;
#endif
  }

#if defined(__linux__)
  //
  // Value of the first /proc/cpuinfo field whose name matches, or empty.
  //
  std::string cpuInfoField(const char* field) {
    std::ifstream in("/proc/cpuinfo");
    std::string line;
    while (std::getline(in, line)) {
      auto colon = line.find(':');
      if (colon == std::string::npos)
        continue;
      auto nameEnd = line.find_last_not_of(" \t", colon - 1);
      if (
        nameEnd == std::string::npos ||
        line.compare(0, nameEnd + 1, field) != 0)
        continue;
      auto first = line.find_first_not_of(" \t", colon + 1);
      return first == std::string::npos ?
        std::string{} : line.substr(first);
    }
    return {};
  }
#endif

  //
  // Processor brand string, or empty when unknown.
  //
  std::string processorType() {
#if defined(_WIN32)
    char buffer[256] = {};
    DWORD size = sizeof(buffer);
    auto status = RegGetValueA(
      HKEY_LOCAL_MACHINE,
      "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0",
      "ProcessorNameString", RRF_RT_REG_SZ, nullptr, buffer, &size);
    if (status == ERROR_SUCCESS)
      return buffer;
    return {};
#elif defined(__linux__)
    return cpuInfoField("model name");
#else
    return {};
#endif
  }

  //
  // Processor frequency in MHz, or 0 when unknown.
  //
  uint32_t processorMHz() {
#if defined(_WIN32)
    DWORD mhz = 0;
    DWORD size = sizeof(mhz);
    auto status = RegGetValueA(
      HKEY_LOCAL_MACHINE,
      "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0",
      "~MHz", RRF_RT_REG_DWORD, nullptr, &mhz, &size);
    if (status == ERROR_SUCCESS)
      return mhz;
    return 0;
#elif defined(__linux__)
    auto text = cpuInfoField("cpu MHz");
    if (!text.empty()) {
      try { return (uint32_t)std::stod(text); }
      catch (...) { return 0; }
    }
    return 0;
#else
    return 0;
#endif
  }
}

//
// Available physical memory — the working-set ceiling before the OS starts
// killing (Linux OOM) or paging (Windows) — or 0 when unknown.
//
uint64_t Environment::AvailableBytes() {
#if defined(_WIN32)
  MEMORYSTATUSEX mem{};
  mem.dwLength = sizeof(mem);
  return GlobalMemoryStatusEx(&mem) ? mem.ullAvailPhys : 0;
#elif defined(_SC_AVPHYS_PAGES)
  auto pages = sysconf(_SC_AVPHYS_PAGES);
  auto pageSize = sysconf(_SC_PAGESIZE);
  if (pages > 0 && pageSize > 0)
    return (uint64_t)pages * (uint64_t)pageSize;
  return 0;
#else
  return 0;
#endif
}

std::string Environment::ToString() {
  std::string text;
  text += std::format(
    "locale: {}\n",
    std::locale().name());
  auto type = processorType();
  if (!type.empty())
    text += std::format(
      "processor: {}\n", type);
  auto mhz = processorMHz();
  if (mhz > 0)
    text += std::format(
      "processor speed: {:.2f} GHz\n",
      (double)mhz / 1000.0);
  auto total = totalBytes();
  if (total > 0)
    text += std::format(
      "total memory: {:.3f} GB\n",
      (double)total / (1ull << 30));
  auto available = AvailableBytes();
  if (available > 0)
    text += std::format(
      "available memory: {:.3f} GB\n",
      (double)available / (1ull << 30));
  return text;
}
