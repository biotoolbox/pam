.PHONY: test

test:
	Rscript -e "library(devtools); test()"

test-%:
	@echo "running test file: test-$*"
	$(eval ARGS := $(if $(suffix $(*)),$(*),$(*).R))
	$(eval path := tests/testthat/test-$(ARGS))
	Rscript -e "library(devtools); devtools::load_all(); library(testthat); test_file('$(path)')"
	