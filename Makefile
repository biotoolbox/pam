ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))

.PHONY: test

test:
ifndef ARGS
	Rscript -e "library(devtools); test()"
else
	$(eval ARGS := $(if $(suffix $(ARGS)),$(ARGS),$(ARGS).R))
	$(eval path := tests/testthat/$(ARGS))
	@echo "Testing: $(ARGS)"
	Rscript -e "library(devtools); devtools::load_all(); library(testthat); test_file('$(path)')"
endif