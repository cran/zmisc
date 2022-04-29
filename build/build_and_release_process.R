
# This script manages the development build process for zmisc.
# The starting point is a clean checkout, typically from Github:
# > git clone https://github.com/torfason/zmisc


## Build package and basic documentation (before commits)
{
  devtools::build()
  devtools::document()
  devtools::build_readme()
  devtools::test()
  message("Build OK")
}

## Build vignettes and site
{
  devtools::build_vignettes()
  devtools::build_manual()
  devtools::build_site()
}

## Final checks (before release)
{
  system("R CMD INSTALL --preclean --no-multiarch --with-keep.source .")
  devtools::spell_check()
  devtools::check()
  devtools::release_checks()
  devtools:::git_checks()
}


## Remote checks (commented out, copy to terminal and run manually)
# devtools::check_rhub()
# devtools::check_win_devel()

## Finally submit to cran (commented out, copy to terminal and run manually)
# devtools::release()

## There should be no need to run the following cleanup tasks
# devtools::clean_vignettes()
# pkgdown::clean_site()
