#Notes are taken from R for Data Science (http://r4ds.had.co.nz)
# Intro -----
  # Packages ---
    install.packages("tidyverse", type = "source", INSTALL_opts = "--byte-compile") #tidyverse packages
    library(tidyverse) #includes ggplot2, tibble, tidyr, readr, purrr, dplyr
    install.packages(c("nycflights13", "gapminder", "Lahman"))    

# Explore ----------
  # Data visualization -----
    # ggplot() creates the coordinate system. 
    # geom functions add layers to the plots.
      # each geom function takes a mapping argument (aes(); x; y)
      # template
        # ggplot(data = <DATA>) + 
          # <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
    
      # basic ggplot example
        ggplot(data = mpg) + 
          geom_point(mapping = aes(x = displ, y = hwy))

    # Aesthetic (one way to diplay variables)
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

    # Statistical transformations
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
          
    # Position adjustment
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
        
    # Coordinate system
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

  # Data transformation -----   
    library(nycflights13)
    library(tidyverse)

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
        sd(x); IQR(x); mad(x) # IQR() and mad() may be better with outliers
        # Example
          not_cancelled %>% 
            group_by(dest) %>% 
            summarise(distance_sd = sd(distance)) %>% 
            arrange(desc(distance_sd))
      # Measures of rank
        min(x); quantile(x, 0.25); max(x); range(x) # min & max
        # Example 1
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            summarise(
              first = min(dep_time),
              last = max(dep_time)
            )
        # Example 2 (not with summarise but mutate & filter)  
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            mutate(r = min_rank(desc(dep_time))) %>% 
            filter(r %in% range(r))
      # Measures of position
        first(x); nth(x, 2); last(x)
        # Example
          not_cancelled %>% 
            group_by(year, month, day) %>% 
            summarise(
              first_dep = first(dep_time), 
              last_dep = last(dep_time)
            )
      # Counts
        n() # Returns size of current group
        sum(!is.na(x)) # Count of non-missing values
        n_distinct(x) # Count of distinct values
          # Example
            not_cancelled %>% 
              group_by(dest) %>% 
              summarise(carriers = n_distinct(carrier)) %>% 
              arrange(desc(carriers))
        count()  # Helper function to count a variable
          # Example 1
            not_cancelled %>% 
              count(dest)
          # Example 2 (with weight)
            not_cancelled %>% 
              count(tailnum, wt = distance)
        # Logical values
          sum(x > 10) # Gives the number of true
          mean(y == 0) # Gives a proportion
          # Example 1 (Number of flights that left before 5 AM)
            not_cancelled %>% 
              group_by(year, month, day) %>% 
              summarise(n_early = sum(dep_time < 500))
          # Example 2 (Proportion of flights delayed more than 1 hour)
            not_cancelled %>% 
              group_by(year, month, day) %>% 
              summarise(hour_perc = mean(arr_delay > 60))            
      # Example of analyzing counts with summarize
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

    # Group by multiple variables
      # Re-summarizing a group_by rolls it up (OK for sums and counts)
        # Example
          daily <- group_by(flights, year, month, day)
          (per_day   <- summarise(daily, flights = n()))
          (per_month <- summarise(per_day, flights = sum(flights)))
          (per_year  <- summarise(per_month, flights = sum(flights)))
      ungroup() # Ungroups returning to ungrouped data    
      
    # Grouped mutates & filters
      # Worst member of each group example
        flights %>% 
          group_by(year, month, day) %>%
          filter(rank(desc(arr_delay)) <= 2)
      # Groups greater than a threshold
        popular_dests <- flights %>% 
          group_by(dest) %>% 
          filter(n() > 365)
        popular_dests
      # Window-like Example  
        popular_dests %>% 
          filter(arr_delay > 0) %>% 
          mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
          select(year:day, dest, arr_delay, prop_delay)
        
    # For window functions see # vignette("window-functions")
      # Five main families of window functions
        # Ranking and ordering functions
          row_number(); min_rank(); dense_rank(); cume_dist(); percent_rank(); ntile()
        # Offsets
          lead(); lag()
        # Cumulative aggregates
          cumsum(); cummin(); cummax(); cumall(); cumany(); cummean()
        # Rolling aggregates
          library("RcppRoll")
        # Recycled aggregates
          # aggregate is repeated to match length of input (not needed)
        
      # Examples
        library(Lahman)
        batting <- Lahman::Batting %>%
          as_tibble() %>%
          select(playerID, yearID, teamID, G, AB:H) %>%
          arrange(playerID, yearID, teamID) %>%
          semi_join(Lahman::AwardsPlayers, by = "playerID")
        players <- batting %>% group_by(playerID)
        # For each player, find the two years with most hits
        filter(players, min_rank(desc(H)) <= 2 & H > 0)  
        # Within each player, rank each year by the number of games played
        mutate(players, G_rank = min_rank(G))
        # For each player, find all where they played more games than average
        filter(players, G > mean(G))
        # For each, player compute a z score based on number of games played
        mutate(players, G_z = (G - mean(G)) / sd(G))
        # Explore windowing over multiple variables
        mutate(players, G_rank = min_rank(G)) %>%
          group_by(teamID) %>%
          mutate(T_rank = min_rank(G)) %>%
          group_by(playerID, teamID) %>%
          mutate(B_rank = min_rank(G)) %>%
          arrange(playerID, teamID)
      
    # Exercise
      ranking <- flights %>%
        select(carrier, dest, arr_delay) %>%
        group_by(carrier, dest) %>%
        summarise(n = n(),
          non_canceled = sum(!is.na(arr_delay)),
          canceled = sum(is.na(arr_delay)),
          mean_delay = mean(arr_delay, na.rm = TRUE)
        ) %>%
        filter(n > 350) %>%
        group_by(dest) %>%
        mutate(r = min_rank(mean_delay)) %>%
        filter(r %in% range(r)) %>%
        mutate(ranking = if_else(r == max(r), "slowest", "fastest")) %>%
        filter(ranking == "slowest") %>%
        group_by(carrier) %>%
        mutate(n_slow = n()) %>%
        summarise(
          nbr = sum(non_canceled),
          n_slow = unique(n_slow),
          prop = n_slow/nbr
        )
      
        ranking %>%
          ggplot(aes(x = prop, y = carrier)) +
          geom_segment(aes(yend = carrier), xend = 0) +
          geom_point()
        
        ranking %>%
          ggplot(aes(x = carrier, y = prop)) +
          geom_col() +
          coord_flip()  

  # Exploratory Data Analysis -----
    # Ask lots of questions (*****maybe cater the dataFun package to help ask lots of questions*****)
      # What type of variation occurs within my variables?
      # What type of covariation occurs between my variables?
      # Work on answering quetsions shown below
    # Visualising distributions (1 variable)
      # Categorial variables: Use bar chart (geom_bar; dplyr::count())
      # Contineous variables: Use histogram (geom_histogram; count(cut_width(carat, 0.5)))
        # Explore a variety of binwidths # geom_histogram(binwidth = 0.1)
        # geom_freqpoly(binwidth = 0.1): When overlaying multiple histograms
        # Questions to ask with bar chart & histogram
          # Which values are the most common?
          # Which values are rare?
          # Any unusual patterns?
    # Outliers
      # Use coord_cartesian(ylim = c(0, 50)) to view outliers easier with filter() & select()
      # Understand why there are outliers and remove if it makes sense.
    # Missing values
      # Either filter out unusual values
        # Example
          diamonds2 <- diamonds %>% 
            filter(between(y, 3, 20))
      # Or replace unusual values with NA
        # Example
          diamonds2 <- diamonds %>% 
            mutate(y = ifelse(y < 3 | y > 20, NA, y))
        # Use na.rm = TRUE to suppress any NA warnings
          # Example
            ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
              geom_point(na.rm = TRUE)
      # Plot missing values against non-missing values to understand differences better
        # Example
          nycflights13::flights %>% 
            mutate(
              cancelled = is.na(dep_time),
              sched_hour = sched_dep_time %/% 100,
              sched_min = sched_dep_time %% 100,
              sched_dep_time = sched_hour + sched_min / 60
            ) %>% 
            ggplot(mapping = aes(sched_dep_time)) + 
            geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)          
    # Covariation (phenomenon that reduces variation)
      # Categorical & contineous variables
        # geom_freqpoly: density rather than count if total counts vary
          # Example
            ggplot(data = diamonds, mapping = aes(x = price)) + 
              geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
            # vs
            ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
              geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
        # geom_boxplot: 25th percentile to the 75th percentile (IQR)
          # reorder() re-orders the variables & coord_flip() can be used for long variable names
            # Example
              ggplot(data = mpg) +
                geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
                coord_flip()
      # Two categorical variables
        # geom_count: The size of the circles represents how many observations occured at each combination
          # Example
            ggplot(data = diamonds) +
              geom_count(mapping = aes(x = cut, y = color))
        # geom_tile: Heatmap. Use count() in conjunction with this plot
          # Simultaneously reorder rows and columns with the seriation package
          # Larger plots use d3heatmap or heatmaply packages for interactive plots
          # Example
            diamonds %>% 
              count(color, cut) %>%  
              ggplot(mapping = aes(x = color, y = cut)) +
              geom_tile(mapping = aes(fill = n))
      # Two continuous variables
        # geom_point: Use alpha to help with plots as the number of the data points gets larger
          # Example
            ggplot(data = diamonds) +
              geom_point(mapping = aes(x = carat, y = price))
        # geom_bin2d(); geom_hex(); # devide into 2 demensional bins with fill color to display # data points
          # Example
            ggplot(data = smaller) +
              geom_bin2d(mapping = aes(x = carat, y = price))
            
            # install.packages("hexbin")
            ggplot(data = smaller) +
              geom_hex(mapping = aes(x = carat, y = price))
        # Bin contineous variable (acts like categorical)
          # varwidth = TRUE: sets the width to be proportional to # of points
          # cut_number(): display same number of points in each bin
          # Example 1: same width
            ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
              geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
          # Example 2: same number of points 
            ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
              geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
          
    # Patterns and models
      # Questions to ask about patterns
        # Is the pattern due to a coincidence?
        # Can you describe the relationship implied by the pattern?
        # How strong is the relationship implied by the pattern?
        # What other variables might affect the relationship?
        # Does the relationship change if you look at individual subgroups of the data?
        ## If two variables covary can you use the values of one variable to make better predictions about the other?
        ## If the covariation is due to a casual relationship can you use the value of one to control the value of another?
        ## *****Think about doing residual plots with the dataFun package*****
      # Analyzing variable relationships through residuals
        # Example 1 (removing relationship of one variable)
          library(modelr)
          
          mod <- lm(log(price) ~ log(carat), data = diamonds)
          
          diamonds2 <- diamonds %>% 
            add_residuals(mod) %>% 
            mutate(resid = exp(resid))
          
          ggplot(data = diamonds2) + 
            geom_point(mapping = aes(x = carat, y = resid))
        # Example 2 (Looking at relationship of another variable through residuals)  
          ggplot(data = diamonds2) + 
            geom_boxplot(mapping = aes(x = cut, y = resid))
          
    # For more information on ggplot2 see the book https://amzn.com/331924275X, or R Graphics Cookbook, or Graphical Data Analysis with R     
          
  # Workflow: projects -----
    # Recomended to not save workspace but to save R scripts and data files
    # In global options: 1) uncheck "Restore .RData into workspace on startup 2) "Save workspace to .RData on exit: Never"
#### Need to try still          
    # Press Cmd/Ctrl + Shift + F10 to restart RStudio
    # Press Cmd/Ctrl + Shift + S to rerun the current script
    # Example of saving a picture as well as outputing data files    
      library(tidyverse)
      ggplot(diamonds, aes(carat, price)) + 
        geom_hex()
      ggsave("diamonds.pdf")
      write_csv(diamonds, "diamonds.csv")          

          
# Wrangle ----------            
  # Tibble ------
    # Never changes the type of the input (strings to factors).
    # Never changes names of variables.
      # For non-syntactic names in tibbles they need to be surounded with backticks ``
    # Never creates row names. 
    # Recycles inputs of length 1
      # Example
        tibble(
          x = 1:5, 
          y = 1, 
          z = x ^ 2 + y
        )
    # tibble data types
      # <int>   # integer
      # <dbl>   # double
      # <chr>   # character
      # <dttm>  # date-times
      # <lgl>   # logical (TRUE/FALSE)
      # <fctr>  # categorical variables w/ fixed possible values
      # <date>  # dates       
    vignette("tibble") # for more detail
    as_tibble(iris) # coerce data frame into tibble
    tibble() # create a new tibble
    tribble() # transposed tibble. customized for data entry in code.
    class(as.data.frame(tb)) # Turn back to data frame for older code.
    # Printing
      # Use print(); options(); View() to look at more than the default view
      # Example 1 (print)
        nycflights13::flights %>% 
          print(n = 10, width = Inf)
      # Example 2 (options)
        options(tibble.print_max = n, tibble.print_min = m); options(tibble.width = Inf)
      # Example 3 (View)
        nycflights13::flights %>% 
          View()
    # Subsetting
      # To pull out a single variable
        # $ # Only extract by name
        # [[ # Extract by name or position
        # Example
          df <- tibble(
            x = runif(5),
            y = rnorm(5)
          )  
          df$x; df[["x"]]; df[[1]]; df %>% .$x; df %>% .[["x"]]

  # Data import ------          
    # Readr functions
      read_csv() # Comma delimited files
      read_csv2() # Semicolon seperated files
      read_tsv() # Tab deliminated files
      read_delim() # Reads in with any deliminater
      read_fwf() # Fixed width files
        fwf_widths() # Specify by widths
        fwf_positions() # Specify by position
      read_log() # Reads Apache style log files
        # Look at webreadr for mroe detail
      # First argument is the path to read
      # Produce an inline csv file for testing
        read_csv("a,b,c 
          1,2,3
          4,5,6")
      # skip = n skips the first n lines
        read_csv("The first line of metadata
            The second line of metadata
                 x,y,z
                 1,2,3", skip = 2)
      # "#" drops all lines
        read_csv("# A comment I want to skip
           x,y,z
           1,2,3", comment = "#")
      # col_names = FALSE does not treat first row as a heading
      # "\n" is a shortcut for adding a new line
        read_csv("1,2,3\n4,5,6", col_names = FALSE)
      # add a character vector to indicate what the name of each column should be
        read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
      # specify values in dataset that represent NA values
        read_csv("a,b,c\n1,2,.", na = ".")
    # Parsing a vector
      # 8 Parsers used to read in file
        parse_logical(); parse_integer()
        parse_double()
        parse_character()
        parse_datetime(); parse_date(); parse_time()
      # For dates the parse_date() may need to be modified
        parse_date("01/02/15", "%m/%d/%y")
        parse_date("01/02/15", "%d/%m/%y")
        parse_date("01/02/15", "%y/%m/%d")
      # May be good to specify column types when reading in the file
        challenge <- read_csv(
          readr_example("challenge.csv"), 
          col_types = cols(
            x = col_integer(),
            y = col_character()
          )
        )
      # guess_max # increase the max_row to guess what each column is
        challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
      # read all the columns in as character vectors
        challenge2 <- read_csv(readr_example("challenge.csv"), 
           col_types = cols(.default = col_character())
        )
    # Writing to a file
      write_csv()   
      write_tsv() 
      write_excel_csv() # Export csv file to excel
      write_rds() # wrapper for base function saveRDS()
      read_rds() # wrapper for base function readRDS()
      # feather package can be shared across programming languages (doesn't support list-columns)
        library(feather)
        write_feather(challenge, "challenge.feather")
        read_feather("challenge.feather")
    # Other file types
      library("rio") # other file types
      haven # SPSS, Stata, and SAS
      readxl # .xls & .xlsx
      DBI # (with RMySQL, RSQLite, RPostgreSQL) # Run SQL queries against a database and return data frame
      jsonlite # json
      xml2
      
  # Tidy data ------    
    # Underlying theory of tidy data http://www.jstatsoft.org/v59/i10/paper
    # Three rules for tidy data
      # Each variable must have its own column
      # Each observation must have its own row
      # Each value must have its own cell
    # Gathering (useful if some of the column names are not names of variables)
      # Example 1 (cleaning up column names & joining 2 tables together)
        table4a; table4b
        tidy4a <- table4a %>% 
          gather(`1999`, `2000`, key = "year", value = "cases")
        tidy4b <- table4b %>% 
          gather(`1999`, `2000`, key = "year", value = "population")
        left_join(tidy4a, tidy4b)
    # Spreading (useful if an observation is scattered across multiple rows)
      table2
      spread(table2, key = type, value = count)
    # separate() # pulls apart a column into multiple columns
      # extract() does something similar as separate but for groups?
      # by default it seperates where it doesn't see an alpha numeric character
      # by default it leaves the column type as it current is
        # Example
          table3 %>% 
            separate(rate, into = c("cases", "population"))
          # or
          table3 %>% 
            separate(rate, into = c("cases", "population"), sep = "/")        
      # convert = TRUE tries to convert to a better format
        # Example
          table3 %>% 
            separate(rate, into = c("cases", "population"), convert = TRUE)
      # Covert other variables if needed
        # Example
          table3 %>% 
            separate(year, into = c("century", "year"), sep = 2)
    # unite() # inverse of seperate(). Combines columns
      # Default seperator is "_"
      # Example
        table5 %>% 
          unite(new, century, year, sep = "")
    # Missing values
      # Implicit missing values you can create if wanted
        # Example      
          stocks <- tibble(
            year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
            qtr    = c(   1,    2,    3,    4,    2,    3,    4),
            return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
          )
          stocks %>% 
            spread(year, return) %>%
            gather(year, return, `2015`:`2016`)
          # or to ignore implicit missing values
          stocks %>% 
            spread(year, return) %>% 
            gather(year, return, `2015`:`2016`, na.rm = TRUE)
      # complete() makes missing values explicit
        # Example
          stocks %>% 
            complete(year, qtr)
      # fill() # carry forward missing values
        # Example
          treatment <- tribble(
            ~ person,           ~ treatment, ~response,
            "Derrick Whitmore", 1,           7,
            NA,                 2,           10,
            NA,                 3,           9,
            "Katherine Burke",  1,           4
          )
          treatment %>% 
            fill(person)
    # Case Study
      # 1st gather together columns that are not variables & then start manipulating
        who %>%
          gather(code, value, new_sp_m014:newrel_f65, na.rm  = TRUE) %>% 
          mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
          separate(code, c("new", "var", "sexage")) %>% 
          select(-new, -iso2, -iso3) %>% 
          separate(sexage, c("sex", "age"), sep = 1, convert = TRUE) %>%
          count(country) %>%
          arrange(country) %>%
          print(n = Inf)   
      # Verify that country, iso2 and iso3 are duplicates    
        who1 <- who %>%
          gather(code, value, new_sp_m014:newrel_f65, na.rm  = TRUE) %>% 
          mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
          separate(code, c("new", "var", "sexage")) %>% 
          select(-new, -iso2, -iso3) %>% 
          separate(sexage, c("sex", "age"), sep = 1, convert = TRUE) %>%
          #mutate(year = lubridate::make_date(year,1,1)) %>%
          count(country,
            year, 
            sex, wt = value) 
      # Plotting exercise  
        # lollypop like graphs
          who1 %>%
            count(country, wt = n) %>%
            filter(nn > 100000) %>%
            ggplot(aes(x = nn, y = country)) +
            geom_segment(aes(yend = country), xend = 0) +
            geom_point()
        # Line graph
          who1 %>%
            filter(country == "United States of America") %>%
            group_by(year) %>%
            summarise(n = sum(n)) %>%
            ggplot(aes(x = year, y = n)) +
            geom_line()
        # Boxplot graph
          who1 %>%
            group_by(country) %>%
            filter(sum(n) > 500000) %>%
            ungroup() %>%
            #filter(year == max(year)) %>%
            ggplot() +
              geom_boxplot(aes(x = reorder(country, n, FUN = median), y = n)) + 
              geom_point(aes(x = country, y = n), alpha = 1/10) +
              coord_flip()
    # For non-tidy data see http://simplystatistics.org/2016/02/17/non-tidy-data/

  # Relational data ------       
    # Multiple tables of data is relational data
    # Identify unique keys in tables
      planes %>% 
        count(tailnum) %>% 
        filter(n > 1)
    # Create a unique key w/ no keys     
      flights %>%
        mutate(row_num = row_number()) %>%
        select(row_num, everything())
    # Mutating joins
      # Mutating joins allow you to combine variables from two tables
      # Inner join: Keeps rows where equal
      # Outer join: left join; right join; full join
      # Example: Add airline name with left join
        flights2 <- flights %>% 
          select(year:day, hour, origin, dest, tailnum, carrier)

        flights2 %>%
          select(-origin, -dest) %>% 
          left_join(airlines, by = "carrier")
      
      # Methods to join
        by = NULL # Default. Matches by all common variables.
        by = c("a" = "b") # Matches variable a in table x with variable b in table y
        # Exmaple
          flights2 %>% 
            left_join(airports, c("dest" = "faa"))
      # Examples
        inner_join(x, y, by = "z")
        left_join(x, y, by = "z")
        right_join(x, y, by = "z")
        full_join(x, y, by = "z")
    # Filtering joins
      # semi_join(x, y) # keeps all observations in x that have a match in y
      # anti_join(x, y) # drops all observations in x that have a match in y
        # Useful to see if your foreign key matches primary key in another table
      # Example
        top_dest <- flights %>%
          count(dest, sort = TRUE) %>%
          head(10)
        top_dest
        
        flights %>%
          filter(dest %in% top_dest$dest)
        # or
        flights %>% 
          semi_join(top_dest)
    # Set operations
      # These compare the values of everything variable (column)
      # intersect(x, y) # returns observations in both x & y
      # union(x, y) # returns unique observations in x and y
      # setdiff(x, y) # return oberservations in x, but not in y

  # String ------   
    # General info
      # Use double or single quotes. Default should be double. Use single if you want to include quotes inside string
      # Example
        string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
      # Special characters
        # "\" # use to include a literal single or double quote
        writeLines(); #To see the raw contents of a string
          # Example
            # (x <- c("\"", "\\"))
            # writeLines(x)
        # "\n" # newline
        # "\t" # tab
        # ?"'" # For complete list of special characters see     
      # c(); character vector
    # stringr package
      # Functions all start with str_
      # str_length(); # Number of characters in string
      # str_c("x", "y", sep = ", "); # Combine 2 or more strings. Sep controls how they are seperated.
      # str_replace_na(x); #If you want the logic to apply to NA as well.
        # Example
          x <- c("abc", NA)
          stringr::str_c("|-", str_replace_na(x), "-|")
      # collapse; # Collapse a vector of strings into a single string
        # Example
          str_c(c("x", "y", "z"), collapse = ", ")
      # str_sub(); # Extract parts of a string.
        # Example
          x <- c("Apple", "Banana", "Pear")
          str_sub(x, 1, 3)
          # or
          str_sub(x, -3, -1)
          str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
      # str_to_upper(); str_to_lower(); str_title(); #Upper/lower case of variables            
      # str_order(); str_sort(); # determine order or sort of string variables
      # str_wrap(); Helps with wrapping paragraphs
      # str_trim(); str_pad(); # Trims or adds whitespace
      # Matching patterns
        # str_view(); str_view_all(); Show how they match
          # "." # Matches any character
          # "\\." # Searches for "." rather than using it as a wild character
          # "\\\\" # Search for "\"  
          # Example 1
            x <- c("apple", "banana", "pear")
            str_view(x, "an")
          # Example 2  
            str_view(c("abc", "a.c", "bef"), "a\\.c") 
        # Anchors
          str_view("banana","[a]")
          
      ####Need to work on this some more####

          ?dplyr::select
          
             
  # Factors ------
    library(tidyverse)
    library(forcats)
    # General information
      # To create a factor create a list of levels
      # Any values not in the set of levels will be converted to NA
      # Omiting levels will default the levels to be in alphabetical order
      # 
    # factor(); as.factor() create/convert to factors. as.factor doesn't mess with existing factors
    # parse_factor(); gives a warning if a value isn't in the set
    # fct_inorder() # Matches level based on first apperance in data set
    # Example
      x1 <- c("Dec", "Apr", "Jan", "Mar")
      x2 <- c("Dec", "Apr", "Jam", "Mar")
      month_levels <- c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      )
      (y1 <- factor(x1, levels = month_levels))
      (y2 <- parse_factor(x2, levels = month_levels))
      # Re-order based on dataset
        (f1 <- factor(x1, levels = unique(x1)))
        (f2 <- x1 %>% factor() %>% fct_inorder())
    # To see levels
      # levels() # Access levels directly 
      # fct_count() # Get a count by level
      # With count function or bar chart
        gss_cat %>%
          count(race)
        ggplot(gss_cat, aes(race)) +
          geom_bar()
        # or (to include levels with no values)
        ggplot(gss_cat, aes(race)) +
          geom_bar() +
          scale_x_discrete(drop = FALSE)
        # or (another example of bar chart)
        ggplot(gss_cat, aes(rincome)) +
          geom_bar() +
          coord_flip()
    # Modify factor order
      # fct_reorder() # Orders factors based on another factor. Use for factors that are arbitrarily ordered
      # fct_reorder2() # Orders based on the y values associates with x. Good for line plots
      # fct_infreq() # Orders in increasing frequency. Good for bar charts
      # fct_rev() # Reverses order of factors. Good to use in conjunction with fct_infreq
      # Example 1 (scatter plot)
        relig_summary <- gss_cat %>%
          group_by(relig) %>%
          summarise(
            age = mean(age, na.rm = TRUE),
            tvhours = mean(tvhours, na.rm = TRUE),
            n = n()
          )
        # Not re-ordered
          ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
        # Re-ordered
          relig_summary %>%
            mutate(relig = fct_reorder(relig, tvhours)) %>%
            ggplot(aes(tvhours, relig)) +
            geom_point()
      # Example 2 (line chart)
        by_age <- gss_cat %>%
          filter(!is.na(age)) %>%
          group_by(age, marital) %>%
          count() %>%
          group_by(age) %>%
          mutate(prop = n / sum(n))
        
        ggplot(by_age, aes(age, prop, colour = marital)) +
          geom_line(na.rm = TRUE)
        # or (to order by y value)
        ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
          geom_line() +
          labs(colour = "marital")        
      # Example 3 (bar charts)
        gss_cat %>%
          mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
          ggplot(aes(marital)) +
          geom_bar()
            
  # Dates and times ------
          
# Program ----------            
          
  # Pipes ------
  
  # Functions ------
  
  # Vectors ------
      
  # Iteration ------
        
# Model ----------    
  
  # Model basics ------
  
  # Model building ------
  
  # Many models ------

# Communicate ----------
  
  # R Markdown ------
  
  # Graphics for communication ------
  
  # R Markdown formats ------        
        
        
        