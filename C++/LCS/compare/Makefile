# Makefile for compare — GNU/Linux out-of-source build.
#
#   make                                                  release build into build/
#   make BUILD=build-debug CXXFLAGS="-std=c++20 -O0 -g -Wall -Wextra"
#                                                         debug build into build-debug/
#   make clean                                            remove the default build/
#   make BUILD=build-debug clean                          remove a named build directory
#
# The executable lands at $(BUILD)/compare and the object files at
# $(BUILD)/*.o, so no artifacts are written beside the sources.  Each
# compile also emits a build/*.d header-dependency file (DEPFLAGS), which
# the -include below pulls in so a header edit rebuilds its dependents.
#
# The tool sources in this directory are linked with the shared library
# sources in ../lib/ (the LCS library extracted from compare and delta).
#
# CXX, CPPFLAGS, and CXXFLAGS honor environment overrides and are also
# overridable on the make command line (GNU convention); DEPFLAGS is
# fixed (:=) and not meant to be overridden.

CXX      ?= g++
CPPFLAGS ?= -I../lib
CXXFLAGS ?= -std=c++20 -O2 -Wall -Wextra
DEPFLAGS := -MMD -MP

# Define ENABLE_MULTI_THREADING=1 on the make command line to compile the
# RickSpace two-thread Hirschberg root split (Option 1); -pthread is added
# automatically.  The default remains single-threaded.
ENABLE_MULTI_THREADING ?= 0
ifeq ($(ENABLE_MULTI_THREADING),1)
CPPFLAGS += -DENABLE_MULTI_THREADING
CXXFLAGS += -pthread
endif

BUILD   ?= build
SOURCES := $(wildcard *.cpp)
OBJECTS := $(patsubst %.cpp,$(BUILD)/%.o,$(SOURCES))
LIB_SOURCES := $(wildcard ../lib/*.cpp)
LIB_OBJECTS := $(patsubst ../lib/%.cpp,$(BUILD)/%.o,$(LIB_SOURCES))
DEPS    := $(OBJECTS:.o=.d) $(LIB_OBJECTS:.o=.d)
TARGET  := $(BUILD)/compare

.PHONY: all clean
all: $(TARGET)

$(TARGET): $(OBJECTS) $(LIB_OBJECTS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

$(BUILD)/%.o: %.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(BUILD)/%.o: ../lib/%.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(BUILD):
	mkdir -p $(BUILD)

# Pull in the generated .d files so header edits rebuild their dependents.
-include $(DEPS)

clean:
	rm -rf $(BUILD)
