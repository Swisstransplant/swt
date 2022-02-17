# swt: Swisstransplant R package

The aim of this package is to support statictial analyses and visualizations
at [Swisstransplant](https://www.swisstransplant.org/) - the Swiss National
Foundation for organ donation and transplantation.

## User guide
### Installation from github
    install.packages("devtools")
    library(devtools)
    install_github("schw4b/swt")


### Loading package swt
    library(swt)

Please find further instruction on how to use the package in the
[SWT cookbook](https://schw4b.github.io/rcookbook/).


### Package building
   export LC_CTYPE='C'

   rm swt/NAMESPACE
   cd swt; R -e 'devtools::document()'; cd ..
   rm swt*.tar.gz
   rm -rf swt.Rcheck

   R CMD build swt
   R CMD check --as-cran swt_0.1.tar.gz
