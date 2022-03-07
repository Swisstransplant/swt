clean:
	rm -rf ./swt/NAMESPACE swt*.tar.gz swt.Rcheck

build:
	R CMD build --no-manual swt

check:
	R CMD check --as-cran --no-manual --no-vignettes swt_0.1.tar.gz
	# R CMD check --as-cran --no-manual --no-vignettes $(file)
