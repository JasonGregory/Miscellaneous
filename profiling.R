# for basic comparisons use system.time
  # example 
    system.time(
      f()
    )
    
# for small code use microbenchmark
  # example (default is to run it 100 times)
    microbenchmark::microbenchmark(..., times = 100L)

# for larger code use the base profile function
  # example
    Rprof(tmp <- tempfile())
    prep_describe(i_data) # insert function that you want to test
    Rprof()
    summaryRprof(tmp) # this outputs the results