# Makefile for compare — GNU/Linux out-of-source build.
#
#   make                                                  release build into build/
#   make BUILD=build-debug CXXFLAGS="-std=c++20 -O0 -g -Wall -Wextra"
#                                                         debug build into build-debug/
#   make clean                                            remove the default build/
#   make BUILD=build-debug clean                          remove a named build directory
#
# The executable lands at $(BUILD)/compare and the object files at
# $(BUILD)/*.o, so no artifacts are written beside the sources.
#
# CXX, CPPFLAGS, and CXXFLAGS honor environment overrides and are also
# overridable on the make command line (GNU convention).

CXX      ?= g++
CPPFLAGS ?= -Iinclude
CXXFLAGS ?= -std=c++20 -O2 -Wall -Wextra

BUILD   ?= build
SOURCES := $(wildcard *.cpp)
OBJECTS := $(patsubst %.cpp,$(BUILD)/%.o,$(SOURCES))
TARGET  := $(BUILD)/compare

.PHONY: all clean
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

$(BUILD)/%.o: %.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

$(BUILD):
	mkdir -p $(BUILD)

clean:
	rm -rf $(BUILD)
