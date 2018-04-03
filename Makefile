JBUILDER := jbuilder
BUILD := build
CLEAN := clean

# Examples

LINEAR_REGRESSION := examples/linear_regression.exe
CONFIDENCE_INTERVAL := examples/confidence_interval.exe
REGRESSION_MIXTURE := examples/regression_mixture.exe

ALL_EXAMPLES := \
	$(LINEAR_REGRESSION) \
	$(CONFIDENCE_INTERVAL) \
	$(REGRESSION_MIXTURE)

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