# An example of reshaping your data with `pivot_longer` 


## Overview

Sometimes the data structure reshaping functions (e.g., `pivot_longer` and `pivot_wider`) in the `tidyr` package can produce surprising results. In this document, I walk through an example of how to reshape data from long to wide. This is a routine task for data analysts, especially those who work with repeated-measure data (longitudinal, time-series cross-sectional, etc.) and other nested data. However, I created this brief example to illustrate how some of the not-so-well-documented arguments can be used to flexibly pre and post-process your data.

# Getting Started

### Simulating Example Data 
We'll start by simulating some arbitrary panel data with a "long" structure. Specifically, assume that we have data on the `response`s of `n_persons` people to `n_items` survey items asked on `n_waves` measurement occasions, plus additional variables that may vary by `person`, `item`, and/or `wave` (i.e. time). By nature of its long structure, our `data.frame` contains one row for each and every response made by any of the persons to any survey item during any wave of data collection. IN addition to columns identifying the person, wave, and item associate with the item response, the other aforementioned variables are stored in their own respective columns.


``` r
# Packages
library(tidyr)
library(dplyr)

set.seed(123)

####################################################
# Simulate data
####################################################

# Simulation parameters
n_persons <- 2
n_waves <- 3
n_items <- 3 # must be < 27 (only 26 letters in alphabet!)
birth_range <- 1930:2000
response_options <- round(runif(5), 2)
n_rows <- n_persons * n_waves * n_items

# Simulate basic data
df <- tibble(
  item = rep(letters[1:n_items], each = n_persons * n_waves),
  wave = rep(rep(1:n_waves, each = n_persons), n_items),
  person = rep(1:n_persons, times = n_items * n_waves),
  response = sample(response_options, n_rows, replace = TRUE),
  irrelevant_number = response + 12,
  irrelevant_char = sample(letters, n_rows, replace = TRUE)
)

# Add a numeric vector varying across persons but constant across waves and 
# items. We could have done this with more thought in our initial simulation
# but I wanted to show how easy this is to accomplish.
person_table <- df %>% 
                distinct(person) %>% 
                mutate(birth = sample(birth_range, n_persons, replace = TRUE))

df_person <- left_join(df, person_table, by = "person")

# Add a character vector varying across persons and items but not across waves 
personitem_table <- df_person %>%
                    distinct(person, item) %>%
                    mutate(quality = sample(c("low", "med", "high"), n(), replace = TRUE))

df_personitem <- left_join(df_person, personitem_table, by = c("person", "item"))
```

### Losing "unused" variables while reshaping data from long to wide

Suppose we no want to reshape our data from its long structure to a wider structure. We could "widen" our data in different ways depending upon how we wish to group the item responses. For example, we could replace the `person` and `response` columns with `n_persons` columns, where each replacement column contains the item responses of one of the `n_persons` unique people in our sample. Alternatively, we could replace the `person` and `response` columns with a set of `n_items` columns, where each replacement column contains the item responses to one of the  `n_items` unique items asked across people and measurement occasions. In both cases, we would remove rows from our `data.frame` to add columns, essentially shifting information stored in rows into columns.

In our case, let's make our data wider by adding columns for each of the `n_waves` measurement occasions. The desired outcome is a `data.frame` where each row contains _all_ of the responses one person made to one survey item across all of the waves of data collection.

``` r
####################################################
# Reshape our data from long to wide
####################################################

df_personitem %>% pivot_wider(
  id_cols = c(person, item),
  id_expand = TRUE,
  names_from = c(wave),
  names_prefix = "response_wave_",
  values_from = response,
  names_expand = TRUE
) %>%
  head(10)
#> # A tibble: 6 x 5
#>   person item  response_wave_1 response_wave_2 response_wave_3
#>    <int> <chr>           <dbl>           <dbl>           <dbl>
#> 1      1 a                0.79            0.41            0.88
#> 2      1 b                0.79            0.94            0.41
#> 3      1 c                0.88            0.29            0.41
#> 4      2 a                0.79            0.94            0.29
#> 5      2 b                0.41            0.41            0.29
#> 6      2 c                0.29            0.94            0.79
```

While this looks pretty good, we are missing all of the variables that were not provided in the `pivot_wider` call. This is because the default behavior of `pivot_wider` is to drop "unused' variables, defined in the [documentation](https://tidyr.tidyverse.org/reference/pivot_wider.html) as: "columns not identified by id_cols, names_from, or values_from".

### Errors when trying to retain unused variables

Ideally, we wouldn't throw away the unused variables since they may contain useful information. However, retaining the unused variables requires a strategy for dealing with variables that may vary across some combination `wave` or `person`. This is because as we remove rows from our `data.frame` with `pivot_wider`, we effectively coalesce multiple rows rows into a single row. When any of the unused variables varies across the unique row identifiers in the wider data (i.e. `wave` and `person`), this cannot be done without loss of information and we are faced with an error message. In essence, we're asking the function to turn a length $k>1$ vector into a scalar value without specifying how.

To demonstrate this, we can reshape our data using the exact same call as before, but explicitly asking `pivot_wider` to retain the unused variables. I do this
by passing a function to the `unused_fn` argument of `pivot_wider`. This function is supposed to return a single value of each variable for each combination of `person` and `item`, so I design it to return the unique values of that variable for all rows matcing on a  `person`-`item` combination. When the the value of a variable does not vary across `person` or `item`, this will produce vector of length one. When the variable exhibits variation across those grouping variables, it will return a vector of length greater than one and throw an error.

``` r
df_personitem %>% pivot_wider(
  id_cols = c(person, item),
  id_expand = TRUE,
  names_from = c(wave),
  names_prefix = "response_wave_",
  values_from = response,
  unused_fn = ~ unique(.),
  names_expand = TRUE
)
#> Error in `value_summarize()`:
#> ! Applying `unused_fn` to `irrelevant_number` must result in a single summary value per key.
#> x Applying `unused_fn` resulted in a value with length 3.
```

The error tells us that the function we passed to the unused_fn argument did did not produce one value per person*item group for every variable. In some cases, it produced 3 values rather than 1. This tells us that (a) some rows with identical values of `person` and `item` had different values for some unused variables and (b) we need a different strategy.

# A general approach to retaining unused variables

There are different approaches to resolving this issue. Since there is no reasonable way to assign $1$ of the $K > 1$ distinct values for those character variables varying within `person` or `item`, we always set those values to `NA`. For non-character variables, we can either set those values to NA or assign their observed (i.e., complete-case) mean. While `pivot_wider` notes that one can apply different functions to different variables to implement these different strategies, the documentation of how to do this is not particularly good. Instead, we'll employ both outside of the `pivot_wider` call as follows:

 1. Keep variables that make up the pivot key (`item` and `person`), as well as those unused variables that we want to retain in our wide data.
 2. Group the data by the key variables.
 3. For each distinct set of values for `item` and `person`, extract the count ($N_w$) of distinct values for each of the $W$ variables in the grouped data.
 4. Create a new `data.frame` with a row for each `item`-`person` combination and the same columns as the ungrouped `data.frame` in Step 2.
 5. For the variable and key-value, fill in the corresponding cell in the
    new data.frame based on the following rules:
    (i) if N_k = 1 => assign that cell's one distinct value from Step 2
    (ii) if N_k > 1 => assign NA
 6. Repeat Steps 4-5, except assign mean(x,na.rm = TRUE) instead of NA. Note
    that mean(x) is NA for character vectors and the numeric mean for logical
    vectors.
 7. Join the two new data.frames to the original data.frames, ensuring names
    of variables don't conflict.

There are some features of summarise() that make this a bit more involved,
such as requiring nested data, but this is the high-level idea. Now I 
implement this.

``` r
# Per the documentation for dplyr::summarise(), you need to store functions
# in a data.frame/tibble to generate new columns containing the results of
# passing those functions to summarise. We can wrap this in a function to
# that will allow us to essentially pass an arbitrary number of functions to
# summarise in the same call, with the same object, and return all of the
# results. One could use map-reduce/apply/purr syntax to call summarise
# repeatedly, but this would also require creating multiple data.frames as 
# well - not very efficient.

# First, define a function creating a tible containing two vectors per recoded
# variable, with recodings based on N_k.
make_recodes <- function(x) {
  N <- x %>% n_distinct()
  
  tibble(nums2mean = ifelse(N > 1, mean(x, na.rm = TRUE), x),
         nums2NA = ifelse(N > 1, NA, x))
}

# Create the object with recoded values for each variable, using the different 
# types of recoding. Note that this may produce many warnings that an argument
# is not numeric or logical. These can be ignored - this will happen any time
# we try to take the mean of a character vector, which will correctly return
# an NA. 
unique_table_nested <- df_personitem %>%
  select(-c(response, wave)) %>%
  group_by(person, item) %>%
  summarise(across(everything(), ~ make_recodes(.), .names = "{col}"))

unique_table_nested %>% glimpse()
#> Rows: 6
#> Columns: 6
#> Groups: person [2]
#> $ person            <int> 1, 1, 1, 2, 2, 2
#> $ item              <chr> "a", "b", "c", "a", "b", "c"
#> $ irrelevant_number <tibble[,2]> <tbl_df[6 x 2]>
#> $ irrelevant_char   <tibble[,2]> <tbl_df[6 x 2]>
#> $ birth             <tibble[,2]> <tbl_df[6 x 2]>
#> $ quality           <tibble[,2]> <tbl_df[6 x 2]>

# The recoded variables are saved in tibbles nested within a tibble. Unest
# to create a flat data.frame:
unique_table_flat <- unique_table_nested %>%
  tidyr::unnest(cols = everything(), names_sep = "_") %>%
  ungroup()

# pivot_wider() original data while letting it drop unused variables
df_personitem_wide <- df_personitem %>% pivot_wider(
  id_cols = c(person, item),
  id_expand = TRUE,
  names_from = c(wave),
  names_prefix = "response_wave_",
  values_from = response,
  names_expand = TRUE
)

# Join recoded variables to original data and take a look
# Depending upon your data, the two recoded variables may not differ.

joined <- left_join(df_personitem_wide, unique_table_flat, by = c("person", "item"))
joined %>% glimpse()
#> Rows: 6
#> Columns: 13
#> $ person                      <int> 1, 1, 1, 2, 2, 2
#> $ item                        <chr> "a", "b", "c", "a", "b", "c"
#> $ response_wave_1             <dbl> 0.79, 0.79, 0.88, 0.79, 0.41, 0.29
#> $ response_wave_2             <dbl> 0.41, 0.94, 0.29, 0.94, 0.41, 0.94
#> $ response_wave_3             <dbl> 0.88, 0.41, 0.41, 0.29, 0.29, 0.79
#> $ irrelevant_number_nums2mean <dbl> 12.69333, 12.71333, 12.52667, 12.67333, 12~
#> $ irrelevant_number_nums2NA   <lgl> NA, NA, NA, NA, NA, NA
#> $ irrelevant_char_nums2mean   <dbl> NA, NA, NA, NA, NA, NA
#> $ irrelevant_char_nums2NA     <lgl> NA, NA, NA, NA, NA, NA
#> $ birth_nums2mean             <int> 1952, 1952, 1952, 1956, 1956, 1956
#> $ birth_nums2NA               <int> 1952, 1952, 1952, 1956, 1956, 1956
#> $ quality_nums2mean           <chr> "low", "low", "med", "high", "high", "low"
#> $ quality_nums2NA             <chr> "low", "low", "med", "high", "high", "low"
```
