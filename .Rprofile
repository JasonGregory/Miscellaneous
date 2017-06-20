# A fun welcome message
message("Howdy Jason!")

#For fun. Show's a quote when opening.
if(interactive()) 
  try(fortunes::fortune(), silent = TRUE)

options(prompt = "R> ", scipen = 999,  max.print = 999999, continue = "  ")
# prompt modifies the >
# scipen eliminates scientific notation
# max.print doesn't limit what is printed
# (use " " for a blank prompt)

# `local` creates a new, empty environment
# This avoids polluting .GlobalEnv with the object r
local({
  r = getOption("repos")             
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
})

#hidden environment
.env = new.env ()

#useful functions
# ht == headtail
ht = function(d, n=6) rbind(head(d, n), tail(d, n))

# Show the first 5 rows & first 5 columns of a data frame
hh = function(d) d[1:5, 1:5]

#Set nice plotting window
nice_par = function(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -0.01, 
                    cex.axis = 0.9, las = 1, mfrow = c(1, 1), ...) {
  par(mar = mar, mgp = mgp, tck = tck, cex.axis = cex.axis, las = las, 
      mfrow = mfrow, ...)
}

#allows the environment to be used
attach(.env)

#set working directory to directory of project
#setwd("R") 

#Installs fortunes package when exiting
.Last = function() {
  cond = suppressWarnings(!require(fortunes, quietly = TRUE))
  if(cond) 
    try(install.packages("fortunes"), silent = TRUE)
  message("Goodbye at ", date(), "\n")
}

