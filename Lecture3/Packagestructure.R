Package structure


# DESCRIPTION
# NAMESPACE
# README.md

  
R/ contains R code
  can have /windows and /unix subdirectories
  
man/ contains help files 
  can have /windows and /unix subdirectories
  can have macros subdirectory
  
src/ contains C/fortran stuff etc.
exec/ contains shell/perl scripts etc.
  
data/
  
demo/ untested R scripts that demonstrate use of the package

inst/ contents are moved up a level after the package is installed. CITATION and 
      NEWS.rd recommended as a minimum
      
tests/ unit tests
  can have examples subdirectory
  can have .Rout.save file
  
vignettes/ comprehensive documentation
  
  
also /po and /tools
  

## Useful packages
  
devtools
# create_package() function (alternatively use R studio)
# install.github() function

roxygen
# semi-automate generation of help files

testthat
# help with creation of unit tests

bioccheck
# automatically check the coding style of packages conforms to bioconductor guidelines


# and many others