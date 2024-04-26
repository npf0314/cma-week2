# Demo

### Download this Demoscript via "\</\>Code" (top right)

### Depending on your knowledge of `R`, getting an overview of the data we imported last week might have been quite a challenge.
### Surprisingly enough, importing, cleaning and exploring your data can be the most challenging, time consuming part of a project.
### RStudio and the tidyverse offer many helpful tools to make this part easier (and more fun). You have read chapters on `dplyr`
### and `magrittr` as a preparation for this exercise. Before we start with the exercise however, this demo illustrates a simple
### approach offered by tidyverse which is applicable to sf-objects.

### Assume we want to calculate the timelag between subsequent positions. To achieve this we can use the function `difftime()`
### ombined with `lead()` from `dplyr`. Let's look at these functions one by one.

#`difftime` {#sec-difftime}

`difftime` takes two `POSIXct` values.

```{r}
now <- Sys.time()

later <- now + 10000

later

time_difference <- difftime(later, now)

time_difference
```

You can also specify the unit of the output.

```{r}
time_difference <- difftime(later, now, units = "secs")

time_difference
```

`difftime` returns an object of the class `difftime`. 
```{r}
#| collapse: true
class(time_difference)

str(time_difference)
```

However in our case, numeric values would be more handy than the class `difftime`. So we'll wrap the command in `as.numeric()`:
  
  ```{r}
#| collapse: true

time_difference <- as.numeric(difftime(later, now, units = "secs"))

str(time_difference)
class(time_difference)
``` 

In fact, we will use this exact operation multiple times, so let's create a function for this:

```{r}
difftime_secs <- function(later, now){
    as.numeric(difftime(later, now, units = "secs"))
}
```

### `lead()` / `lag()` {#sec-lead-lag}

`lead()` and `lag()` return a vector of the same length as the input, just offset by a specific number of values (default is 1). Consider the following sequence:

```{r}
numbers <- 1:10

numbers
```

We can now run `lead()` and `lag()` on this sequence to illustrate the output. `n =` specifies the offset, `default =` specifies the default value used to "fill" the emerging "empty spaces" of the vector. This helps us performing operations on subsequent values in a vector (or rows in a table).

```{r}
library("dplyr")

lead(numbers)

lead(numbers, n = 2)

lag(numbers)

lag(numbers, n = 5)

lag(numbers, n = 5, default = 0)
```

### `mutate()`

Using the above functions (`difftime()` and `lead()`), we can calculate the time lag, that is, the time difference between consecutive positions. We will try this on a dummy version of our wild boar dataset.

```{r}
wildschwein <- tibble(
    TierID = c(rep("Hans", 5), rep("Klara", 5)),
    DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

wildschwein
```


To calculate the `timelag` with base-R, we need to mention `wildschwein` three times 

```{r}
wildschwein$timelag <- as.numeric(difftime(lead(wildschwein$DatetimeUTC), wildschwein$DatetimeUTC))
```

Using `mutate()` we can simplify this operation slightly:

```{r}
wildschwein <- mutate(wildschwein, timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC)))

wildschwein
```

### `group_by()`

You might have noticed that `timelag` is calculated across different individuals (`Hans` and `Klara`), which does not make much sense. 
To avoid this, we need to specify that `timelag` should just be calculated between consecutive rows *of the same individual*. We can implement this by using `group_by()`. 

```{r}
wildschwein <- group_by(wildschwein, TierID)
```

After adding this grouping variable, calculating the `timelag` automatically accounts for the individual trajectories.

```{r}
wildschwein <- mutate(wildschwein, timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC)))

wildschwein
```

### `summarise()`

If we want to summarise our data and get metrics *per animal*, we can use the `dplyr` function `summarise()`. In contrast to `mutate()`, which just adds a new column to the dataset, `summarise()` "collapses" the data to one row per individual (specified by `group_by`).

```{r}
summarise(wildschwein, mean = mean(timelag, na.rm = TRUE))
```

Note: You can do `mutate()` and `summarise()` on `sf` objects as well. However, `summarise()` tries to coerce all geometries into one object, which can take along time. To avoid this, use `st_drop_geometry()` before using `summarise()`. 

### Piping 

The code above may be a bit hard to read, since it has so many nested functions which need to be read from the inside out. In order to make code readable in a more human-friendly way, we can use the piping command `|>` from `magrittr`, which is included in `dplyr` and the `tidyverse`. The above code then looks like this:

```{r}
wildschwein |>                                     # Take wildschwein...
    group_by(TierID) |>                            # ...group it by TierID
    summarise(                                     # Summarise the data...
        mean_timelag = mean(timelag, na.rm = TRUE) # ...by calculating the mean timelag
    )
```

<!-- ### Bring it all together...

Here is the same approach with a different dataset:

```{r}
pigs <- tibble(
    TierID = c(8001, 8003, 8004, 8005, 8800, 8820, 3000, 3001, 3002, 3003, 8330, 7222),
    sex = c("M", "M", "M", "F", "M", "M", "F", "F", "M", "F", "M", "F"),
    age = c("A", "A", "J", "A", "J", "J", "J", "A", "J", "J", "A", "A"),
    weight = c(50.755, 43.409, 12.000, 16.787, 20.987, 25.765, 22.0122, 21.343, 12.532, 54.32, 11.027, 88.08)
)

pigs

pigs |>
    summarise(
        mean_weight = mean(weight)
    )

pigs |>
    group_by(sex) |>
    summarise(
        mean_weight = mean(weight)
    )

pigs |>
    group_by(sex, age) |>
    summarise(
        mean_weight = mean(weight)
    )
```

-->