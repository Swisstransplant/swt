# swt: Swisstransplant R package <img src="man/figures/swt_hex.png" align="right" width=120 height=139 alt="swt hex sticker" />

The aim of this package is to support statistical analyses and visualizations
at [Swisstransplant](https://www.swisstransplant.org/) - the Swiss National
Foundation for organ donation and transplantation.

## User guide

### Installation from github

    remotes::install_github("Swisstransplant/swt")

### Loading package swt

    library(swt)

## Developer guide

### Package building

    make clean
    cd swt; R -e 'devtools::document()'; cd ..
    make build
    make file=swt_0.21.tar.gz check

### Install package locally

    detach("package:swt", unload=TRUE)

    R CMD REMOVE swt
    R CMD INSTALL swt_0.2.tar.gz
