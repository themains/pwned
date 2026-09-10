.PHONY: restore analysis exhibits paper test lint check

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript scripts/run_all.R

exhibits:
	Rscript -e 'source("scripts/run_all.R"); source("scripts/exhibits.R")'
	Rscript -e 'knitr::knit("README.Rmd", quiet = TRUE)'

paper: exhibits
	cd ms && latexmk -pdf -interaction=nonstopmode -halt-on-error pwned.tex pwned_si.tex
	cd present && latexmk -pdf -interaction=nonstopmode -halt-on-error pwned.tex

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

lint:
	Rscript -e 'lints <- unlist(lapply(c("scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(lints); stopifnot(length(lints) == 0L)'

check: analysis test lint
