clean:
	rm -rf ./swt/NAMESPACE swt*.tar.gz swt.Rcheck

doc:
	cd swt; R -e 'devtools::document()'; cd ..
	rm -rf swt.pdf
	R CMD Rd2pdf swt
	rm -rf ./swt/.Rd2pdf*

build:
	mv swt.pdf ./swt/manual/swt.pdf
	R CMD build --no-manual swt

check:
	R CMD check --as-cran --no-manual --no-vignettes $(file)
