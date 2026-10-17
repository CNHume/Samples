# Makefile for delta — GNU/Linux out-of-source build.
#
#   make                                                  release build into build/
#   make BUILD=build-debug CXXFLAGS="-std=c++20 -O0 -g -Wall -Wextra"
#                                                         debug build into build-debug/
#   make clean                                            remove the default build/
#   make BUILD=build-debug clean                          remove a named build directory
#
# The executable lands at $(BUILD)/delta and the object files at
# $(BUILD)/*.o, so no artifacts are written beside the sources.  Each
# compile also emits a build/*.d header-dependency file (DEPFLAGS), which
# the -include below pulls in so a header edit rebuilds its dependents.
#
# CXX, CPPFLAGS, and CXXFLAGS honor environment overrides and are also
# overridable on the make command line (GNU convention); DEPFLAGS is
# fixed (:=) and not meant to be overridden.

CXX      ?= g++
CPPFLAGS ?= -Iinclude
CXXFLAGS ?= -std=c++20 -O2 -Wall -Wextra
DEPFLAGS := -MMD -MP

BUILD   ?= build
SOURCES := $(wildcard *.cpp)
OBJECTS := $(patsubst %.cpp,$(BUILD)/%.o,$(SOURCES))
DEPS    := $(OBJECTS:.o=.d)
TARGET  := $(BUILD)/delta

.PHONY: all clean
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

$(BUILD)/%.o: %.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(BUILD):
	mkdir -p $(BUILD)

# Pull in the generated .d files so header edits rebuild their dependents.
-include $(DEPS)

clean:
	rm -rf $(BUILD)
