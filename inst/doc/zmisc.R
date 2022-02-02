## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# We need rprojroot to locate other files needed for the Vignette
library(rprojroot)

# Source a few helper/utility functions
source(find_package_root_file("vignettes","utils_knitr.R"))
source(find_package_root_file("vignettes","utils_roxygen.R"))

# Get roxygen blocks, use force to trigger any warnings immediately
blocks <- roxy_get_blocks(find_package_root_file())
blocks <- force(blocks)


## ----load_package-------------------------------------------------------------
library(zmisc)

## ---- echo=FALSE--------------------------------------------------------------
roxy_get_section(blocks, "lookup", "examples") |> kat_code()

## ---- echo=FALSE--------------------------------------------------------------
roxy_get_section(blocks, "lookuper", "examples") |> kat_code()

## ---- echo=FALSE--------------------------------------------------------------
roxy_get_section(blocks, "zample", "examples") |> kat_code()

## ---- echo=FALSE--------------------------------------------------------------
roxy_get_section(blocks, "zeq", "examples") |> kat_code()

## ---- echo=FALSE--------------------------------------------------------------
roxy_get_section(blocks, "zingle", "examples") |> kat_code()

