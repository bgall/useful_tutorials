# A Brief Introduction to Using Looping and Vectorization to "Automate" Variable Generation

Suppose you have an arbitrary `data.frame` object. You want to add an arbitrary number of new columns to this `data.frame` that are arbitrary functions of existing columns. It is common to use the `dplyr` package's `mutate()` function to accomplish this. In particular, many people will use what I call a "manual approach" where each item is added on its own, independent lines of code. 

## Manual column creation

In the example implementation of the "manual approach" provided below, I first create a `data.frame` with a single character vector (`x`). I then add four new columns (`var_a`, `var_b`, `var_c`, and `var_d`) that are functions of the column's values. Three columns use `grepl()` to search for a sub-string within `x` and return a value of 1 for those elements of the vector where a match is found (i.e. `greply()` is `TRUE` for that element) and a value of 0 otherwise. A fourth column returns a value of 1 for each element where `x` is missing and 0 otherwise.

``` r
library(dplyr)

# Original data
df <- data.frame(x = as.character(1:5))

# Manual approach
df %>% mutate(
  var_a = case_when(grepl("1", x) == T ~ 1, T ~ 0),
  var_b = case_when(grepl("2", x) == T ~ 1, T ~ 0),
  var_c = case_when(grepl("3", x) == T ~ 1, T ~ 0),
  var_d = case_when(is.na(x) == T ~ 1, T ~ 0)
)
#>   x var_a var_b var_c var_d
#> 1 1     1     0     0     0
#> 2 2     0     1     0     0
#> 3 3     0     0     1     0
#> 4 4     0     0     0     0
#> 5 5     0     0     0     0
```

There are at least two issues with this approach:

**It requires a lot of (redundant) code.** 

While it was not particularly burdensome to produce one line of code for each of our four columns, the manual approach causes the tedium and time-demands of coding to grow as the desired number of new columns increases. In fact the code has a constant rate of growth: one additional line is required for each additional column While creating dozens of columns might be tedious, creating tens of thousands of columns may prove practically infeasible.^[Scenarios where researchers may wish to add many columns that are functions of others include the addition of high numbers of temporally lagged values for analysis of time series and longitudinal data, spatially lagged values for analysis of spatial data, contextual/group-level columns calculated from lower-level (e.g. individual) data, etc.]

Writing so much code has downsides beyond its considerable demand on your time. First, it provides more opportunities for bugs. Typos are more likely to occur when more typing is involved and more code provides more opportunities to overlook bugs during review. Second, it often makes your code more difficult to read. While it is debatable whether a 5-line program is easier to understand than a 10 line program, it is rarely easier to understand a 1,000-line program than to understand a program accomplishing the same task in 20 lines.

Fortunately, we can often implement "automated approaches" that substantially reduce this burden and downside risk by reducing the amount of code we write while maintaining readability. The core idea is to write code where additional columns require almost zero additional effort and code. Automated approaches include looping and vectorized (or parallelized) approaches.

**It is slow**

While `mutate()` works wonders, its magic can require patience. This is because it creates new columns by moving *sequentially* through its argument. The is why it enables you to create columns that are functions of columns previously created within the same `mutate()` call. It also explains why the order in which you create columns may change the output of `mutate()` - as demonstrated in the code below.


``` r
library(dplyr)

# Original data
df <- data.frame(x = as.character(1:5))

# Manual approach
df %>% mutate(
  var_a = case_when(grepl("1", x) == T ~ 1, T ~ 0),
  var_b = case_when(grepl("2", x) == T ~ 1, T ~ 0),
  var_c = case_when(grepl("3", x) == T ~ 1, T ~ 0),
  var_d = case_when(is.na(x) == T ~ 1, T ~ 0)
)
#>   x var_a var_b var_c var_d
#> 1 1     1     0     0     0
#> 2 2     0     1     0     0
#> 3 3     0     0     1     0
#> 4 4     0     0     0     0
#> 5 5     0     0     0     0
```

The primary downside to this is that your computer must wait until one column is created before creating another. While many users do not wish to create enough columns nor rely on sufficiently time-intensive functions where this leads to noticeable slowdowns, it *can* do so. Alternative approaches can be faster by either exploiting parallel processing, functions that essentially do the same thing but rely on faster underlying C code, etc.

## Automated column creation

If we want to minimize the amount of code and time that goes into column creation, we can try to automate some parts of the code. Automation may involve more "overhead" - both code and time - than manual creation when the number of columns is small. However, this is an investment to decrease the marginal effort of adding additional columns.

To get started, we will want identify parts of our code that are repeated and parts of our code that vary every time a column is created. Looking at our code, it is clear that we use quite similar code to create the first three columns - which we will collectively refer to as the "grepl columns." In fact, each line only differs by two characters: the suffix attached to the prefix "var_" for each column name and the pattern that `grepl()` searches for when creating the column. The fourth column we created only differs slightly from these as well - instead of using `grepl()`, it uses `is.na()`.

Supposing we wanted to create many more columns, we then might focus on creating lots of grepl columns with only a minimal amount of code. Since only two values are changing, we would want to be able to pass only sets of column name suffixes and the patterns to look for within `grepl()`. Otherwise, the code would put everything together and pass it to `mutate()` to create the variables. While for the purposes here I'll only show approaches to creating many grepl columns, one could further augment the functions I write to incorporate different functions within `case_when()` and or add the other variables manually.

Let's now turn to three approaches we might have alternatively used to add these columns. Note that while each of the following largely uses the same variable construction set-up, I repeat the variable construction every time so that each section has self-contained code.

### for() loop

We'll first create lists of column names and values for those grepl columns we want to create. I construct the columns by concatenating together different sub-strings, which would let me avoid typing out or copying and pasting then editing column names to create many strings.

```r
# Original data
df <- data.frame(x = as.character(1:5))

# Create lists where each element is either a variable name or the value for grepl
new_vars <- paste("var",c("a","b","c"), sep = "_") %>% as.list()
new_vals <- 1:4 %>% as.character() %>% as.list()

# Initialize empty lists to hold the new columns themselves
newcol_n <- length(new_vars)
new_data_list_loop <- vector(mode="list", length = newcol_n)
```

Next we can define a function that takes our original `data.frame`, a list of column names, and a list of values `grepl()` should try to find and produces the new column as a `data.frame`. Note that we place `!!` before `vars`. This ensures each column is assigned the correct name.

``` r
# Define a function to create a new variable!
create_var_loop <- function(data, vars, vals) {
  data %>%
    transmute(!!vars := case_when(grepl(vals, x) == T ~ 1, T ~ 0))
}
```

Finally, we can use a `for()` loop to iterate over the elements of the lists we created, passing the values to our function and storing the output in a different index of the list we created for storing the output. We can then combine the elements of the list into a `data.frame` for our final output.

``` r
# Loop over the function, creating a new variable every time and storing in list
for (i in 1:newcol_n){
  new_data_list_loop[[i]] <- create_var_loop(data = df, vars = new_vars[[i]], vals = new_vals[[i]])
}

# Combine into a single data.frame
new_data_list_loop %>% bind_cols(df, .)
#>   x var_a var_b var_c
#> 1 1     1     0     0
#> 2 2     0     1     0
#> 3 3     0     0     1
#> 4 4     0     0     0
#> 5 5     0     0     0
```

### lapply

Looping can be pretty slow in R. For this reason, users are often encouraged to employ vectorized functions rather than loops. `lapply()`, which essentially implements a `for()` loop in faster C code, allows us to speed things up. Since we are passing multiple arguments to the `apply` functions, we will use the multi-dimensional-input variant `mapply()`.

``` r
# Original Data
df <- data.frame(x = as.character(1:5))

# Create lists containing new column names and values
suffix <- c("a","b","c")
pattern <-  1:3
new_vars <- paste0("var", "_", suffix) %>% as.list()
new_vals <- pattern %>% as.character() %>% as.list()

# Initialize empty lists to hold the new columns themselves
newcol_n <- length(new_vars)
new_data_list_lapply <- new_data_list_loop
#> Error in eval(expr, envir, enclos): object 'new_data_list_loop' not found

# Create a list with one copy of the original data for each new column
df_list <- df %>% list() %>% rep(newcol_n)

# Define `create_var` function
# Creates 1-column data.frame containing new variable
create_var <- function(data, vars, vals) {
  data %>%
    transmute(!!vars := case_when(grepl(vals, x) == T ~ 1, T ~ 0))
}

# Iterate over elements of list with mapply
new_data_list_lapply <- mapply(create_var, data = df_list, vars = new_vars, vals = new_vals)

# Combine into a single data.frame
new_data_list_lapply %>% bind_cols(df, .)
#>   x var_a var_b var_c
#> 1 1     1     0     0
#> 2 2     0     1     0
#> 3 3     0     0     1
#> 4 4     0     0     0
#> 5 5     0     0     0
```
