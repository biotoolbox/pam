.PHONY: test

test:
	Rscript -e "library(devtools); test()"

test-%:
	@echo "running test file: test-$*"
	$(eval ARGS := $(if $(suffix $(*)),$(*),$(*).R))
	$(eval path := tests/testthat/test-$(ARGS))
	Rscript -e "library(devtools); devtools::load_all(); library(testthat); test_file('$(path)')"
	

original
|same								|Eilers and Peeters	|Platt				|Walsby										|Vollenweider				|
|a									|a					|ps					|etr_max									|pmax						|
|b									|b					|alpha				|alpha										|a							|
|c									|c					|beta				|beta										|alpha						|
|d									|NA					|NA					|NA											|n							|
|alpha								|s					|alpha				|alpha										|NA							|
|beta								|NA					|beta				|beta										|NA							|
|etrmax_with_photoinhibition		|pm					|pm					|NA											|popt						|
|etrmax_without_photoinhibition		|NA					|ps					|etr_max									|pmax						|
|ik_with_photoinhibition			|ik					|ik					|NA											|iik						|
|ik_without_photoinhibition			|NA					|is					|NA											|ik							|
|im_with_photoinhibition			|im					|im					|NA											|NA							|
|w									|w					|NA					|NA											|NA							|
|ib									|NA					|ib					|NA											|NA							|
|etrmax_with_without_ratio			|NA					|NA					|NA											|pmax_popt_and_ik_iik_ratio	|
|sdiff								|sdiff				|sdiff				|sdiff										|sdiff						|


modified
|same								|Eilers and Peeters	|Platt				|Walsby										|Vollenweider				|
|a									|a					|ps					|etr_max									|pmax						|
|b									|b					|alpha				|alpha										|a							|
|c									|c					|beta				|beta										|alpha						|
|d									|NA					|NA					|NA											|n							|
|alpha								|s					|alpha				|alpha										|real_alpha					|
|beta								|NA					|beta				|beta										|NA							|
|etrmax_with_photoinhibition		|pm					|pm					|etrmax_with_photoinhibition				|popt						|
|etrmax_without_photoinhibition		|NA					|ps					|etr_max									|pmax						|
|ik_with_photoinhibition			|ik					|ik					|ik_with_photoinhibition					|iik						|
|ik_without_photoinhibition			|NA					|is					|ik_without_photoinhibition					|ik							|
|im_with_photoinhibition			|im					|im					|im_with_photoinhibition					|im_with_photoinhibition	|
|w									|w					|NA					|NA											|NA							|
|ib									|NA					|ib					|NA											|NA							|
|etrmax_with_without_ratio			|NA					|ps_pm_ratio		|etr_max_etrmax_with_photoinhibition_ratio	|pmax_popt_and_ik_iik_ratio	|
|sdiff								|sdiff				|sdiff				|sdiff										|sdiff						|