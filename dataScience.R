#Notes are taken from R for Data Science (http://r4ds.had.co.nz)
# Intro -----
  # Packages ---
    install.packages("tidyverse", type = "source", INSTALL_opts = "--byte-compile") #tidyverse packages
    library(tidyverse) #includes ggplot2, tibble, tidyr, readr, purrr, dplyr
    install.packages(c("nycflights13", "gapminder", "Lahman"))    

# Explore -----
  # Data visualization ---
    # ggplot() creates the coordinate system. 
    # geom functions add layers to the plots.
      # each geom function takes a mapping argument (aes(); x; y)
      # template
        # ggplot(data = <DATA>) + 
          # <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
    
      # basic ggplot example
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy))

    # Aesthetic (one way to diplay variables) --
      size  # modify size asethic
      alpha # modify transparency asethic
      shape # modify shape asethic
      fill 
      #border it doesn't look like this is a known aesthetic      
          
      # Asethetic example
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))    
          # scaling is where a unique level of the aesthetic is assigned to a unique value of the variable

    # Facets (another way to display variables) --
      # facets displays subsets of your data in different plots. 
        facet_wrap(~ class, nrow = 2) # is used for one variable
        facet_grid(drv ~ cyl) # is used for two variables. put variable with more unique levels in columns
      
      # example 1 (1 variable)
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy)) + 
          facet_wrap(~ class, nrow = 2)

      # example 2 (2 variables)
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy, border = class)) +
          facet_grid(cyl ~ drv)          
        
      # example 3 (use "." to plot on either x or y axis. Maybe better to use facets_wrap...)
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy, color = class), show.legend = FALSE) +
          facet_grid(. ~ year)

    # Geometric objects --
      # geoms are different objects used to represent objects
        # bar charts:bar geoms; line charts:line geoms; boxplots: boxplot geoms; scatterplots: point geoms
      # all geom's take a mapping argument (shape, x, etc.) but every not all aesthetics work with every geom
      # more than 30 different geoms with more in extention packages 
      # comprehensive overview: http://rstudio.com/cheatsheets
      # geom extention packages: https://www.ggplot2-exts.org

        geom_point(mapping)
        geom_jitter(); # Same as geom_point(position = "jitter")
        geom_smooth(mapping, se); # se = FALSE turns off standard error   
        geom_bar() # Good for discrete variable
        geom_freqpoly() # Similar to bar charge except that it's for conineous variables. Good for showing frequency of numbers.
        geom_boxplot()
          
      # example 1 (point geom)
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy))
        
      # example 2 (smooth geom)
        ggplot(data = mpg) + 
          geom_smooth(mapping = aes(x = displ, y = hwy))
        
      # use the group aesthetic for geoms that use a single geometric object to display multiple rows
        # of data. the group aesethic can be set to a categorical variable to draw
        # multiple objects. It doesn't add a distinguising feature though.

        # example (with group aesthetic)        
        ggplot(data = mpg) +
          geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))  
        
      # to display multiple geoms to a plot add a geom functions  

        # example
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy)) +
          geom_smooth(mapping = aes(x = displ, y = hwy), se = FALSE)
        
      # control the overall data and aesthetics in the ggplot function
        # the data and aesthetics can be overridden within each geom for that specific layer
        
        # example
        ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
          geom_point(mapping = aes(color = class)) + 
          geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

      # misc info
        # wtihin a geom show.legend = FALSE removes the legend
        
        ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
          geom_point(mapping = aes(color = drv)) +
          geom_smooth(mapping = aes(linetype = drv),se = FALSE)

    # Statistical transformations -- 
      # Some graphs calculate new statistics to plot
      # Bar charts, histograms, frequency polygons bin your data and plot bin counts
      # Stat is the statistical transformation used for the plot
      # There are over 20 different stats to use
      # Geoms and stats can be used interchangeably 
        # Every geom has a default stat and every stat has a default geom
        # Example (geom vs stat) # geom_bar is using a stat argument of "count"
          ggplot(data = diamonds) + 
            geom_bar(mapping = aes(x = cut))
          
          ggplot(data = diamonds) + 
            stat_count(mapping = aes(x = cut))
        # Example of changing the geom_bar default stat to "identity"
          demo <- tribble(
            ~cut,         ~freq,
            "Fair",       1610,
            "Good",       4906,
            "Very Good",  12082,
            "Premium",    13791,
            "Ideal",      21551
          )
          
          ggplot(data = demo) +
            geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
       
        # Example (change the default mapping from the default stat to aesthetic)
          ggplot(data = diamonds) + 
            geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))          
          
      # Sometime more attention may be wanted for the statistical stransformations
          ggplot(data = diamonds) + 
            stat_summary(
              mapping = aes(x = cut, y = depth),
              fun.ymin = min,
              fun.ymax = max,
              fun.y = median
            )
          
    # Position adjustment --
      # Position adjustment for bar charts is helpful 
        # Example 1 (Position = "identity". Hard to tell length of bars.)
          ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) + 
            geom_bar(fill = NA, position = "identity")
        # Example 2 (Position = "fill". Compare proportion across groups.) 
          ggplot(data = diamonds) + 
            geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
        # Example 3 (Position = "dodge". Compare individual values.) 
          ggplot(data = diamonds) + 
            geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")          
      # Position adjustment for scatter plots (position = "jitter")
        # This is the same as geom_jitter()
          ggplot(data = mpg) + 
            geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")          

      # Search following terms for help
        position_dodge
        position_fill
        position_identity
        position_jitter
        position_stack
        
    # Coordinate system --
      # default coordinate system is the Cartisian coordinate system
        coord_flip # switches the x and y axes
          ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
            geom_boxplot()
          ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
            geom_boxplot() +
            coord_flip()
        coord_quickmap # sets aspect ratio correctly for maps
          nz <- map_data("nz")
          ggplot(nz, aes(long, lat, group = group)) +
            geom_polygon(fill = "white", colour = "black")
          ggplot(nz, aes(long, lat, group = group)) +
            geom_polygon(fill = "white", colour = "black") +
            coord_quickmap()
        coord_polar # uses polar coordinates
          bar <- ggplot(data = diamonds) + 
            geom_bar(
              mapping = aes(x = cut, fill = cut), 
              show.legend = FALSE,
              width = 1
            ) + 
            theme(aspect.ratio = 1) +
            labs(x = NULL, y = NULL)
          
          bar + coord_flip()
          bar + coord_polar()
          
    # Final template    
      #  ggplot(data = <DATA>) + 
      #    <GEOM_FUNCTION>(
      #      mapping = aes(<MAPPINGS>),
      #      stat = <STAT>, 
      #      position = <POSITION>
      #    ) +
      #    <COORDINATE_FUNCTION> +
      #    <FACET_FUNCTION> +
      #    <SCALE_FUNCTION> + 
      #    <THEME_FUNCTION>
      
                
# Workflow -----
  # "alt -" is a shortcut for " <- "
  # "snake_case" is the recommended convention for objects

# Data transformation (dplyr) -----   
  library(nycflights13)
  library(tidyverse)
          
  flights # tibble dataset
  # <int>   # integer
  # <dbl>   # double
  # <chr>   # character
  # <dttm>  # date-times
  # <lgl>   # logical (TRUE/FALSE)
  # <fctr>  # categorical variables w/ fixed possible values
  # <date>  # dates 
          
  # 6 main dplyr functions
    # First specify data. Following arguments specify what to do with data. Result is new data frame.
    filter()    # Pick observations by their values
    arrange()   # Reorder the rows
    select()    # Pick variables by their names
    mutate()    # Create new variables (at the end) with functions of existing variables
    summarise() # Collapse many values down to a single summary
    group_by()  # Change from entire data set to operating group-by-group
    # If you want to both assign a variable & print results wrap in parentheses.
      # Example
        (dec25 <- filter(flights, month == 12, day == 25))

  filter(.data, ... )
    filter(flights, month == 1, day == 1)
    
    # between (use between along with filter)
      filter(flights, between(dep_time,0,600))
    # NA
      # To preserve NA in a filter specify it
        df <- tibble(x = c(1, NA, 3))
        filter(df, is.na(x) | x > 1)
      # NA operations
        # NA * 0 # NA
        # NA ^ 0 # 0
        # NA | TRUE # TRUE
        # FALSE & NA # NA
        # NA > 5 # NA
        # 10 == NA # NA
        # NA + 10 # NA
        # NA / 2 # NA
        
    # Comparisons
      near(); filter(flights, near(month,1)) # use rather than == since numbers are stored with finite precision
    # Logical operators  
      # Example (Two different ways for "or" logic)
      filter(flights, month == 11 | month == 12)
      filter(flights, month %in% c(11, 12))

  arrange(.data, ... )
    # Example
      arrange(flights, year, month, day)
    # Order descending
      arrange(flights, desc(arr_delay))
    # NA
      # always sorts to end
      # to sort NA from top use is.na
        arrange(flights, desc(is.na(arr_delay)))
    
  select(.data, ... )
    # by column name  
      select(flights, year, month, day)
    # columns between
      select(flights, year:day)
    # everything but columns between
      select(flights, -(year:day))
    # select helper functions
      starts_with("abc") # names that begin with "abc"
      ends_with("xyz") # names that end with "xyz"
      contains("ijk") # names that contain "ijk"
        # Example
          select(flights, dplyr::contains("TIME")) # default is to ignore case
      matches("(.)\\1") # names matching a regular expression. This pulls names that have a repeated character.
      num_range("x", 1:3) # select a numbered range "x1, x2, x3"
      one_of(..., vars = current_vars()) # select variables that may not evaluate correctly
      everything() # useful if you want to move a few variables to the front
        select(flights, time_hour, air_time, everything())
  
    # Rename variables
        rename(flights, tail_num = tailnum, blahbalh = month) # use to rename variables

  mutate(.data, ... )      
    # Example (New variables go to end and you can refer to variables created earlier)
      flights_sml <- select(flights, 
        year:day, 
        ends_with("delay"), 
        distance, 
        air_time
      )
      
      mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours
      )    
      
    # transmute() # use to only keep the new variables created
      transmute(flights,
        gain = arr_delay - dep_delay,
        hours = air_time / 60,
        gain_per_hour = gain / hours
      )
      # useful functions to create new variables
        # arithmetic operators
          # +; -; *; /; ^; x / sum(x) # proportion of a total; y - mean(y) # difference from mean
        # modular arithmetic (can break up integers into pieces)
          # %/% # integer division 
          # %%  # remainder
          # Example
              transmute(flights,
                dep_time,
                hour = dep_time %/% 100,
                minute = dep_time %% 100
              )
        # log(); log2(); log10()
        # lead(); lag(); Example x - lag(x), x != lag(x) # more useful to use with group_by(); 
          (x <- 1:10)        
          lag(x)
          lead(x)
        # Cummulative and rolling aggregates
          cumsum()
          cumprod()
          cummin()
          cummax()
          dplyr::cummean()
          library("RcppRoll") # Use for rolling aggregates (i.e. rolling window) 
          # Example
            x; cumsum(x); cummean(x)
        # Ranking
          min_rank()
          row_number()
          dense_rank()
          percent_rank()
          cume_dist()
          ntile()
          # Example 1
            (y <- c(1, 2, 2, NA, 3, 4))
            min_rank(y); min_rank(desc(y))
            row_number(y); dense_rank(y); percent_rank(y); cume_dist(y)
          # Example 2 (Illustrative only. Probably just use the arrange() function.)
            transmute(flights, 
              arr_delay,
              delay_rank = row_number(arr_delay)
            ) %>%
              arrange(desc(delay_rank))

  # Pipe functionality
    # Example without pipe 
      by_dest <- group_by(flights, dest)
      delay <- summarise(by_dest,
       count = n(),
       dist = mean(distance, na.rm = TRUE),
       delay = mean(arr_delay, na.rm = TRUE)
      )
      delay <- filter(delay, count > 20, dest != "HNL")
      ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)
    # Example with pipe
     flights %>% 
      group_by(dest) %>% 
      summarise(
        count = n(),
        dist = mean(distance, na.rm = TRUE),
        delay = mean(arr_delay, na.rm = TRUE)
      ) %>% 
      filter(count > 20, dest != "HNL") %>%
      ggplot(aes(x=dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)
            
  summarise() # collapses a data frame to a single row
    # Example
      summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
    # More useful when used with group_by()
      by_day <- group_by(flights, year, month, day)
      summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
    # Missing values
      # Any missing values in the input will return a missing aggregate value
      # All aggregation functions have a na.rm argument to remove missing values
      # Example 1  
        flights %>% 
          group_by(year, month, day) %>% 
          summarise(mean = mean(dep_delay, na.rm = TRUE))
      # Example 2
        not_cancelled <- flights %>% 
          filter(!is.na(dep_delay), !is.na(arr_delay))
        
        not_cancelled %>% 
          group_by(year, month, day) %>% 
          summarise(mean = mean(dep_delay))

    # Counts
      # Analyze average delay by looking at counts  
      # First graph shows there are some planes with really large average delay
        delays <- not_cancelled %>% 
          group_by(tailnum) %>% 
          summarise(
            delay = mean(arr_delay)
          )
        
        ggplot(data = delays, mapping = aes(x = delay)) + 
          geom_freqpoly(binwidth = 10)
      
      # Analyze average delay when n is small (variation is small with n small)
        delays <- not_cancelled %>% 
          group_by(tailnum) %>% 
          summarise(
            delay = mean(arr_delay, na.rm = TRUE),
            n = n()
          )
        
        ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
          geom_point(alpha = 1/10)
        
      # Filtering out groups with smallest observations
        delays %>% 
          filter(n > 25) %>% 
          ggplot(mapping = aes(x = n, y = delay)) + 
          geom_point(alpha = 1/10)
  
        # If you want to click elsewhere and re-run the code type "Cmd/Ctrl + Shift + P"
    
      # Example 2 - Batters average score increases with increased play time.
        batting <- as_tibble(Lahman::Batting)
        
        batters <- batting %>% 
          group_by(playerID) %>% 
          summarise(
            ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
            ab = sum(AB, na.rm = TRUE)
          )
        
        batters %>% 
          filter(ab > 100) %>% 
          ggplot(mapping = aes(x = ab, y = ba)) +
          geom_point() + 
          geom_smooth(se = FALSE)
           
    # Summary functions
      mean(x)
      median(x)
      # Logical subsetting
        # Example
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            summarise(
              avg_delay1 = mean(arr_delay),
              avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
            )
      # Measures of spread
        sd(x); IQR(x); mad(x) # IQR and mad may be better with outliers
        # Example
          not_cancelled %>% 
            group_by(dest) %>% 
            summarise(distance_sd = sd(distance)) %>% 
            arrange(desc(distance_sd))
      # Measures of rank
        min(x); quantile(x, 0.25); max(x)
        # Example
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            summarise(
              first = min(dep_time),
              last = max(dep_time)
            )
      # Measures of position
        first(x); nth(x, 2); last(x)
        # Example
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            summarise(
              first_dep = first(dep_time), 
              last_dep = last(dep_time)
            )
      
        