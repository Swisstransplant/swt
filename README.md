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

## Developer guide
### Package building
    export LC_CTYPE='C'

    make clean
    cd swt; R -e 'devtools::document()'; cd ..
    make build
    make file=swt_0.1.tar.gz check

### Install package locally
    detach("package:swt", unload=TRUE)

    R CMD REMOVE swt
    R CMD INSTALL swt_0.1.tar.gz
