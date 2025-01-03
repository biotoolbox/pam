.PHONY: test

test:
	rm -rf src/tests/testthat/results
	Rscript -e "setwd('src/'); library(devtools); test()"

test-%:
	rm -rf src/tests/testthat/results
	@echo "running test file: test-$*"
	$(eval ARGS := $(if $(suffix $(*)),$(*),$(*).R))
	$(eval path := tests/testthat/test-$(ARGS))
	Rscript -e "setwd('src/'); library(devtools); devtools::load_all(); library(testthat); test_file('$(path)')"
