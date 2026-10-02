# Convert case counts to rate

'convert_counts_to_rate' converts case counts for a given population to
an adjusted per population of size X and rounds to the given number of
digits.

## Usage

``` r
convert_counts_to_rate(counts, pop, digits, rate_adj_pop = 1e+05)
```

## Arguments

- counts:

  Integer(s). Case counts to convert.

- pop:

  Integer. Population size where cases were counted.

- digits:

  Integer. Number of decimals to round to.

- rate_adj_pop:

  Integer. Optional target population to use for rate. Defaults to 100k
  for rate per 100k.

## Value

The count(s) as rates per rate_adj_pop.

## Examples

``` r
convert_counts_to_rate(50, 200000, 2)
#> [1] 25
convert_counts_to_rate(c(10, 20), 100000, 1, 10000)
#> [1] 1 2
```
