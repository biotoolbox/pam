.PHONY: test

install:
	Rscript -e 'packages <- readLines("packages.txt"); install.packages(packages)'

test:
	rm -rf src/tests/testthat/results
	Rscript -e "setwd('src/'); library(devtools); test()"

test-%:
	rm -rf src/tests/testthat/results
	@echo "running test file: test-$*"
	$(eval ARGS := $(if $(suffix $(*)),$(*),$(*).R))
	$(eval path := tests/testthat/test-$(ARGS))
	Rscript -e "setwd('src/'); library(devtools); devtools::load_all(); library(testthat); test_file('$(path)')"

build:
	Rscript -e "setwd('src/'); library(devtools); devtools::load_all(); devtools::document(); devtools::build()"

buildtest:
	rm -rf src/tests/testthat/results
	make build
<<<<<<< HEAD
	R CMD check pam_2.0.3.tar.gz
=======
	R CMD check pam_2.1.0.tar.gz
>>>>>>> a691a6c (renamed etrmax_with_without_ratio to etrmax_without_with_ratio)
