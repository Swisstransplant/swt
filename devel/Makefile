clean:
	rm -rf ./swt/NAMESPACE swt*.tar.gz swt.Rcheck

build:
	R CMD build --no-manual swt

check:
	R CMD check --as-cran --no-manual --no-vignettes swt_0.1.tar.gz
	# R CMD check --as-cran --no-manual --no-vignettes $(file)

doc:
	rm -rf swt.pdf
	R CMD Rd2pdf swt
	rm -rf ./swt/.Rd2pdf*
	mv swt.pdf ./swt/manual/swt.pdf
