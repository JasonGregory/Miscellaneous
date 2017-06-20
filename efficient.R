#Notes taken from https://csgillespie.github.io/efficientR/

# Profiling & Benchmarking -----

#system.time()
system.time(
  for(i in 1:50000){
    df[3, 2]
  }
)

##Benchmarking
library("microbenchmark")
  #milliseconds (ms) 1,000/second
  #microsecond (us) 1 million/second
  #nanosecond (ns) 1 billion/second
df = data.frame(v = 1:4, name = letters[1:4])
microbenchmark(df[3, 2], df[3, "name"], df$name[3])

x = 1:100 # initiate vector to cumulatively sum

cs_for = function(x){
  for(i in x){
    if(i == 1){
      xc = x[i]
    } else {
      xc = c(xc, sum(x[1:i]))
    }
  }
  xc
}

cs_apply = function(x){
  sapply(x, function(x) sum(1:x))
}  

microbenchmark(cs_for(x), cs_apply(x), cumsum(x))
indentical() #can be used to test of 2 different code has the same result

##Profiling 
library("profvis")
profvis(expr = {
  exploreIt <- function(input_data, output_file = "exploreIt.html", output_format = "html", output_dir = getwd(), ...) {
    ## Get argument list
    args <- as.list(match.call())
    ## Get directory of report markdown template
    if (output_format == "pdf") {
      report_dir <- system.file("rmd_template/exploreItpdf.rmd", package = "dataFun")
      output_file <- "exploreIt.pdf"
    } else {
      report_dir <- system.file("rmd_template/exploreIt.rmd", package = "dataFun")
    }
    
    ## Render report into html
    render(
      input = report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(data = input_data, fun_options = list()),
      ...
    )
    ## Open report
    report_path <- file.path(output_dir, output_file)
    browseURL(report_path)
    ## Print report directory
    if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]])) message(paste0("\n\nReport is generated at \"", report_path, "\"."))
  }
  
  exploreIt(dat)
}, interval = 0.01, prof_output = "ice-prof")


#####Efficient set-up
library("benchmarkme")
Sys.info()

#Install latests version of R (only on Windows?)
installr::updateR() 
#Helps load packages for windows with system level dependencies
installr::install.rtools() 
#Install & load packages
pkgs = c("raster", "leaflet", "rgeos") # package names
install.packages(pkgs)
inst = lapply(pkgs, library, character.only = TRUE) # load them
update.packages() # update installed CRAN packages

##Startup
#https://support.rstudio.com/hc/en-us/articles/200549016-Customizing-RStudio
#Snippets (type shift+tab after typing) https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets?version=1.0.143&mode=desktop
  #lib (library)
  #fun (function)
  #if 
  #el (else)
  #ei (else if)
  #for 
  #while 
  #switch
  #apply, lapply, sapply, mapply, tapply, vapply, rapply
  #ts (time stamp)
#.Rprofile & directory locations
  R.home() #location for directory where R is installed
  Sys.getenv("HOME") #Users home directory
  getwd() #Current working directory
  file.exists("~/.Rprofile")
  file.edit("~/.Rprofile") # edit .Rprofile in HOME
  file.edit(".Rprofile") # edit project specific .Rprofile 
  #check for site wide Rprofile
  site_path = R.home(component = "home")
  fname = file.path(site_path, "etc", "Rprofile.site")
  file.exists(fname)  
#Specify windows filepath that will work for both mac & windows
  normalizePath("")
#.Rprofile specific items
  #See file.edit(".Rprofile") for this project's Rprofile
  options(prompt = "> ") # return to default
# .Renviron can be used to modify where packages are saved and other related options.
# For rStudio uncheck Restore .RData to make it quicker when re-opening.

# Efficient Programing -----
  #Don't grow vectors in loops. They should be the same size before and after.
  n = 10
  method1 = function(n) {
    vec = NULL # Or vec = c()
    for(i in seq_len(n))
      vec = c(vec, i)
    vec
  }
  method2 = function(n) {
    vec = numeric(n)
    for(i in seq_len(n))
      vec[i] = i
    vec
  }
  method3 = function(n) seq_len(n)

  microbenchmark(times = 100, unit = "s", 
                 method1(n), method2(n), method3(n))   
  #Use vectorized code if possible
    #seq_along(x); seq_len(length(x)); log_sum = sum(log(x)); sum of vector rather than for loop
##Misc code
  try(); tryCatch() #Error handling
  stop(); warning(); suppressWarnings() #Stop code & Warnings
  message(); suppressMessages(); cat(); print(); show() #Messages 
  invisible() #Allows a plot to not be plotted to be passed as a variable
##Factors
  #Use factors for ordering as well as when there is a fixed set of categories
  factor(type, levels = c("Small", "Medium", "Large")) #Order variable "type" example
  #For read.csv() use the stringsAsFactors = FALSE argument to not convert automatically to factors
  #Use apply family instead of loops 
    apply(array, margin, ...) #apply function to each row or column of matrix or automic vector (margin=1 is rows, 2 is columns)
    lapply() #similar to apply except that input type is a vector or list and output is a list
    sapply() #apply function to each element of list but get automic vector back
##Caching variables & functions
  #Store calculated values rather than re-calculating them multiple times
  m_plot_mpg = memoise(plot_mpg) #cache function if it's going to be called multiple times
  #Example of memoise with multiple box plots
    multboxplot <- function(data) {
    
      for (i in 1:length(p.numeric(data))) {
        y <- data[,p.numeric(data)[i]] 
        for (t in 1:length(p.factor(data))) {
          boxplot(y ~ data[,p.factor(data)[t]], col=rainbow(t), varwidth=TRUE)
          abline(h=mean(y), col="navy")
          titl(paste(p.numeric(data)[i], "vs.", p.factor(data)[t], sep=" "))
        }
      }
    
    }
    m_multboxplot = memoise(multboxplot)
    c_multboxplot = cmpfun(multboxplot)
    m_c_multboxplot = memoise(c_multboxplot)
    microbenchmark(times = 10, unit = "ms", multboxplot(dat), c_multboxplot(dat), m_multboxplot(dat), m_c_multboxplot(dat))
    rm(m_multboxplot) #also remove() clears the cache 
  #Use function closures to help write concise code
##Compile functions and packages
  library("compiler") # byte compile functions and packages
  cmpfun() #compiles functions
  #Add "ByteCompile: true" to the DESCRIPTION file for personal packages
  install.packages("ggplot2", type = "source", INSTALL_opts = "--byte-compile") #example of how to install a package pyte compiled

#####Efficient Workflow
##How to find packages
  #Find packages with google or rdocumentation.org or RSiteSearch("word")
##How to choose packages
  #Determine amount of downloads http://cranlogs.r-pkg.org/badges/last-month/ 
  #Mature and activly developed

# Efficient input/output -----
  readRDS(); saveRDS() #Input and output using R's native file format
  library("rio"); import() #efficiently import data from a wide range of file formats
  library("readr"); library("data.table"); library("feather") #reading in larger data files efficiently
  library("WDI") #access online data
  file.size(); object.size() #determine the size of files and if they are too big
  ##Comparison of file formats
    #Example 1
      fname = system.file("extdata", "titanic.csv", package = "dataFun")
      microbenchmark(times = 10, unit = "ms", read.csv(fname), rio::import(fname), readr::read_csv(fname), data.table::fread(fname))
        #rio & data table are the quickest. readr is more efficient than read.csv and is easier to work with than rio and data.table
      #Example 2
      fpath = "~/Documents/Kaggle/Russian Housing/Kaggle Sberbank/train.csv"
      microbenchmark::microbenchmark(times = 10, unit = "ms", read.csv(fpath), rio::import(fpath), readr::read_csv(fpath), data.table::fread(fpath))
##rio package
  import("filename") #import a variety of data
  export(data, "filename") #export as a variety of data
##Optimized file types
  #Rds
  saveRDS(dat, "dataTest.Rds") #seems to be a little slower than the rio::export method
  readRDS("extdata/co2.Rds") #seems to be faster than rio::import for Rds Files
  #Feather (similar read time, possible quicker write time, maybe slightly larger file size)
  library("feather")
  write_feather(df_co2, "extdata/co2.feather")
  df_co2_feather = read_feather("extdata/co2.feather")
##Downloading files from the internet
  url = "https://www.monetdb.org/sites/default/files/voc_tsvs.zip"
  download.file(url, "voc_tsvs.zip") # download file
  unzip("voc_tsvs.zip", exdir = "data") # unzip files
  file.remove("voc_tsvs.zip") # tidy up by removing the zip file
  library("httr"); library("RCurl") #packages 
##Data from packages
  data(package = "dplyr") #data available in a package (converted to R's raw data format)
  list.files(system.file("extdata", package = "dataFun")) #How to view raw data
  system.file(package = "dplyr") #locate a file's location

# Efficient data carpentry -----
  ##Tibble
  library("tibble")
  tibble(x = 1:3, y = c("A", "B", "C")) #A more efficient data frame. A part of the tidyverse.
  ##Tidying data
    vignette("tidy-data") #See further info on tidy data
    library("stringi"); library("stringr") #Messy character strings. "stringr" depends on "stringi"
    library("assertive"); library("assertr") #Diagnostic checks for data integrity
    library("lubridate") #non-standard text string into date formats
    #"Wide" tables "long" (better for vector computations)
      gather(data = pew, key = Income, value = Count, -religion) #Makes wide tables long
      spread() #does the inverse
    #Split joint variables
      separate(agesex_df, col = agesex, into = c("age", "sex")) #Seperate age & gender into seperate columns
    library("broom"); tidy() #standard output for model results
    ##Regular expresions
      #stringr (rather than regex [uses grepl; regmatches; etc.]) is a relativly new way to do regular expressions
        str_detect(); str_match_all(); str_split(); str_replace_all() #Various string manipulation fuctions
      library("stringr")
      x = c("Hi I'm Robin.", "DoB 1985")
      str_detect(string = x, pattern = "9")
  ##Data processing
    library("dplyr"); vignette("dplyr") #good package for processing data
      #Is fast works well w/ tidy data and w/ databases. Only supports data frames.
      filter(); slice() #subset rows by attribute (filter) or position (slice)
        #filter example
          filter(wb_ineq, code == "AUS" & Year == 1974)
      arrange() #order data by variables (use .by_group to sort by groups)
      select(); pull() #subset columns to create a smaller tibble (select) or vector (pull)
      rename() #rename columns
        #example of renaming 
          wb_ineq = rename(wb_ineq,
                           top10 = "Income share held by highest 10% [SI.DST.10TH.10]",
                           bot10 = "Income share held by lowest 10% [SI.DST.FRST.10]")
      distinct() #return unique rows
      mutate() #create new variables (trasmute or drop existing variables)
        #example of converting to numeric
          wb_ineq = mutate_each(wb_ineq, "as.numeric", cols_to_change)
      summarise() #collapse data into a single row
      sample_n() #return a sample of the data
      top_n() #returns the top n of a variable
      group_by()
      bind_rows(); combine()
        #all single tab verbs include scoped variants (_if(); _at(); _all())
    #Examples
      #creating a new variable
      wb_ineq %>% 
        select(Year, gini) %>% 
        mutate(decade = floor(Year / 10) * 10) %>% 
        group_by(decade) %>% 
        summarise(mean(gini, na.rm = TRUE))
      #group by & summarize example
      wb_ineq %>% 
        filter(grepl("g", Country)) %>%
        group_by(Year) %>%
        summarise(gini = mean(gini, na.rm  = TRUE)) %>%
        arrange(desc(gini)) %>%
        top_n(n = 5)
    #Data Aggregation
      #example 1
        data(wb_ineq, package="efficient")
        countries = group_by(wb_ineq, Country)
        summarise(countries, gini = mean(gini, na.rm  = TRUE))
      #example 2
        summarise(countries,
                  # number of rows per country
                  obs = n(), 
                  med_t10 = median(top10, na.rm  = TRUE),
                  # standard deviation
                  sdev = sd(gini, na.rm  = TRUE), 
                  # number with gini > 30
                  n30 = sum(gini > 30, na.rm  = TRUE), 
                  sdn30 = sd(gini[ gini > 30 ], na.rm  = TRUE),
                  # range
                  dif = max(gini, na.rm  = TRUE) - min(gini, na.rm  = TRUE)
          )
    #Data Table
      library("data.table"); data.table() #faster for some operations; Supports rolling joins
      #set keys for even faster performance (good when subsetting data)
        setkey(wb_ineq_dt, Country)
        aus3b = wb_ineq_dt[.("Australia")]
      #other recommended packages & vignette for data tables
        library("datatable-intro"); library("datatable-reshape"); vignette("datatable-reference-semantics")

# Efficient Optimization -----
  ##profvis 
    provis() #use functions as shown earlier to profile code.
    #to profile functions the package may need to be installed from source
    #example
      devtools::install_github("csgillespie/efficient",
                               args = "--with-keep.source") 
      library("efficient")
      profvis(simulate_monopoly(10000))
  ##Base R computations
    ##ifelse
      #don't use ifelse(). Alternatives:
        #Quickest but not written the neatest
        mark = 25
        if(mark >= 40) {
          "pass" 
        } else {
          "fail"
        }
        #Written better but slower
        dplyr::if_else(marks >= 40, "pass", "fail")
    ##sorting, ordering, max, and min
      sort() #use the sorting algorithm "radix" 
      sort(x, partial = 1:10) #quicker if you only want the top 10 results
      sort(x, decreasing = TRUE); rev(sort(x)) #rev() is about 10% slower than the first option
      which.min(); which.max() #quicker than using which(); which(x == min(x))
    ##converting factors to numerics
      as.numeric(levels(f))[f] #quickest way to do it
    ##Matrices are generally quicker than data frames
      data.matrix() #to convert to matrix
    ##Misc
      #use "&&" and "||" for non-vector comparisons since they are more efficient than "&" and "|"
      library("matrixStats") #includes multiple optimized row/column functions
        rowSums(); colSums(); rowMeans(); colMeans()
      is.na() # anyNA() is more efficient than any(is.na())
      #sparse matrices are more efficient in storing data
        library("Matrix"); sparseMatrix()
  ##Parallel computing
    library("parallel")
    no_of_cores = detectCores()
    parLapply(); parApply(); parSapply() #same as lapply(); sapply(); and apply() but parallel
    #example
      library("parallel")
      cl = makeCluster(4)
      parSapply(cl, 1:N, snakes_ladders)
      stopCluster(cl)
    #function example
      simulate = function(cores) {
        cl = makeCluster(cores)
        on.exit(stopCluster(cl)) #on.exit can also be used with the par() function to return to normal graphics
        # Do something  
      }
  #C++
    library("Rcpp") #useful if you want to optimize code using C++

# Hardware & Style Guide ------
  benchmarkme::get_ram() #get ram for computer
  # http://adv-r.had.co.nz/Style.html style guide
  # RStudio: indent code with (Ctrl+I); Reformat code (Ctrl+Shift+A)
  
# Efficient Learning -----
  library(dplyr)
  browseVignettes(package = "dplyr") #search for any vignettes
  v = vignette("introduction", package = "dplyr")
  edit(v)
  apply #hit F2 to view the source code to the function
  library("swirl")
  swirl()