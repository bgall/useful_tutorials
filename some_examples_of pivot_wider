``` r
# Packages
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.1.3
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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

####################################################
# Reshape our data from long to wide
####################################################

# Let's assume that we want to produce the following data.frame:
# - Each row uniquely identifies a person*item combination
# - Contains "item" and "person" columns to identify who made the response and
#   the item to which it corresponds.
# - For each wave of responses collected, contains 1 "wave" column
# - Each wave column contains the response for that wave, person, item combo.
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

# Everything looks good but we are missing variables that were not make up the 
# keys or values for the widening. For example, birth, quality, and both of the
# "irrelevant_" variables were dropped by default because they were not used.

# Ideally, we would have those "unused" variables in our data. Yet keeping the 
# unused variables requires a strategy for dealing with variables that may vary 
# across some combination of keys (i.e. wave, person, item). This is because 
# widening our data removes grows from our dataset to add the information 
# contained in those rows as additional columns. As we remove rows, we effectively
# are collapsing them together and adding additional columns. When we try to do 
# this for the unused variables, we may end up asking pivot_wider() to collapse
# rows that differ on those variables. This will throw an error because there are
# many ways this could be done and the software chooses not to make assumptions.

# To demonstrate the above, we can reshape our data using the exact same call,
# but while specifying that the unused variables should be included. I do this
# by passing a function to the unused_fn argument that is supposed to return
# a single summary value per key (in our case, person*id). I accomodate different
# types of vectors by using a function that effectively groups rows by person*id
# and returns each group's unique values for each unused variable. In order for
# pivot_longer to work, this would need to return 1 value per unused variable 
# per group.
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

# The error tells us that the function we passed to the unused_fn argument did
# did not produce one value per person*item group for every variable. In some
# cases, it produced 3 values rather than 1. This tells us that (a) some rows
# with identical values of person and item had different values for some unused
# variables and (b) we need a different strategy.

# There are different approaches to resolving this issue. Since there is no
# reasonable way to assign 1 of the K > 1 distinct values for those CHARACTER
# variables varying within person*item, we always set those values to NA. For
# non-character variables, we can either set those values to NA or assign their
# observed (i.e., complete case) mean.

# I implement both of these strategies below. The procedure is as follows:
#  1. Keep variables that make up the key (where our key is item*person)
#     and those that we want to retain in our wide data.
#  2. Group the data by the key variables.
#  3. Extract the count (N_k) of distinct values for each of the K variables in
#     the grouped data for each key-value - I simultaneously calculate the
#     number of unique values of e.g. quality and other variables for each
#     person*item value.
#  4. Create a new data.frame with a row for each distinct key-value pair and
#     the same columns as the ungrouped data.frame in Step 2.
#  5. For the variable and key-value, fill in the corresponding cell in the
#     new data.frame based on the following rules:
#     (i) if N_k = 1 => assign that cell's one distinct value from Step 2
#     (ii) if N_k > 1 => assign NA
#  6. Repeat Steps 4-5, except assign mean(x,na.rm = TRUE) instead of NA. Note
#     that mean(x) is NA for character vectors and the numeric mean for logical
#     vectors.
#  7. Join the two new data.frames to the original data.frames, ensuring names
#     of variables don't conflict.

# There are some features of summarise() that make this a bit more involved,
# such as requiring nested data, but this is the high-level idea. Now I 
# implement this.

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
#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA
#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA

#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA

#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA

#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA

#> Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
#> returning NA
#> `summarise()` has grouped output by 'person'. You can override using the
#> `.groups` argument.

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
