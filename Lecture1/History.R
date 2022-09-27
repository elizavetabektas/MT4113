# Reclaiming work

## If everything closed nicely, and you just forgot to save your work
## then look in history

history()

## or look in the history panel in Rstudio

# Note also

timestamp()

## and

savehistory(file="MondaySept19.Rhistory")

## There are ways of automating running 
## functions that we might look at later 
## in the module

# If things didn't go well, or if it is a source file that was lost:

## If the system crashed, and you are using Rstudio, look for the hidden folder.

# .Rproj.user

## within this folder there should be a subfolder with an 8-digit
## name made up of numbers and letters

list.dirs(path = ".Rproj.user",recursive = F)

## within that there is usually a "sources" or an "sdb" folder


## within that there is usually a "s-XXXXXXXX" folder

## The files within that folder often contain lost work 
## (it takes a bit of effort to sort through them, but...)
