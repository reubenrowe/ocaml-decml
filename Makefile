JBUILDER := jbuilder
BUILD := build
CLEAN := clean

# Examples

LINEAR_REGRESSION_EX := examples/linear_regression.exe

ALL_EXAMPLES := \
	$(LINEAR_REGRESSION_EX)

# Tests

MODEL_BUILDING_TEST := test/model_building.exe

ALL_TESTS := \
	$(MODEL_BUILDING_TEST)

# Targets

.PHONY: all lib tests examples clean

lib:
	$(JBUILDER) $(BUILD)

tests: $(ALL_TESTS)

examples: $(ALL_EXAMPLES)

all: lib tests examples

%.exe:
	$(JBUILDER) $(BUILD) $@

clean:
	$(JBUILDER) $(CLEAN)