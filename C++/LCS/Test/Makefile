# Makefile for the LCS test application — out-of-source build into build/.
#
#   make          build build/test
#   make test     build and run the tests
#   make clean    remove build/
#
# The test binary links the compare library sources (all but compare/main.cpp)
# together with the doctest test cases in this directory, so the tests exercise
# the same code the compare tool ships.

CXX      ?= g++
CPPFLAGS ?= -I../compare -I. -DTEST_DATA_DIR=\"$(abspath ../data)\"
CXXFLAGS ?= -std=c++20 -O2 -Wall -Wextra
DEPFLAGS := -MMD -MP

BUILD   ?= build

# Library objects: one per compare/*.cpp, excluding compare/main.cpp.
LIB_SOURCES := $(filter-out %/main.cpp, $(wildcard ../compare/*.cpp))
LIB_OBJECTS := $(patsubst ../compare/%.cpp, $(BUILD)/%.o, $(LIB_SOURCES))

# Test objects: one per *.cpp in this directory.
TEST_SOURCES := $(wildcard *.cpp)
TEST_OBJECTS := $(patsubst %.cpp, $(BUILD)/%.o, $(TEST_SOURCES))

TARGET := $(BUILD)/test
DEPS   := $(LIB_OBJECTS:.o=.d) $(TEST_OBJECTS:.o=.d)

.PHONY: all test clean
all: $(TARGET)

test: $(TARGET)
	./$(TARGET)

$(TARGET): $(LIB_OBJECTS) $(TEST_OBJECTS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

# Compile a compare library source.
$(BUILD)/%.o: ../compare/%.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

# Compile a local test source.
$(BUILD)/%.o: %.cpp | $(BUILD)
	$(CXX) $(CPPFLAGS) $(DEPFLAGS) $(CXXFLAGS) -c $< -o $@

$(BUILD):
	mkdir -p $(BUILD)

-include $(DEPS)

clean:
	rm -rf $(BUILD)
