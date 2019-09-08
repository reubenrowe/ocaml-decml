DUNE := dune
BUILD := build
INSTALL := install
TEST := runtest
CLEAN := clean

# Examples

LINEAR_REGRESSION := examples/linear_regression.exe
CONFIDENCE_INTERVAL := examples/confidence_interval.exe
REGRESSION_MIXTURE := examples/regression_mixture.exe

ALL_EXAMPLES := \
	$(LINEAR_REGRESSION) \
	$(CONFIDENCE_INTERVAL) \
	$(REGRESSION_MIXTURE)

# Targets

.PHONY: all lib tests examples install clean

lib:
	$(DUNE) $(BUILD)

tests:
	$(DUNE) $(TEST)

examples: $(ALL_EXAMPLES)

all: lib tests examples

install:
	$(DUNE) $(INSTALL)

%.exe:
	$(DUNE) $(BUILD) $@

clean:
	$(DUNE) $(CLEAN)
