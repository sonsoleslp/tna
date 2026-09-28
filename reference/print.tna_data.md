# Print a TNA Data Object

Print a TNA Data Object

## Usage

``` r
# S3 method for class 'tna_data'
print(x, data = "sequence", ...)
```

## Arguments

- x:

  A `tna_data` object.

- data:

  A `character` string that defines the data to be printed tibble.
  Accepts either `"sequence"` (default) for wide format sequence data,
  `"meta"`, for the wide format metadata, or `"long"` for the long
  format data.

- ...:

  Arguments passed to the tibble `print` method.

## Value

`x` (invisibly).

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
res <- prepare_data(group_regulation_long, action = "Action", actor = "Actor",
time = "Time")
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 27533 rows, 6 columns
#> ℹ First few time values: 2025-01-01 08:27:07.712698, 2025-01-01
#>   08:35:20.712698, and 2025-01-01 08:42:18.712698
#> ℹ Number of values to parse: 27533
#> ℹ Sample values: 2025-01-01 08:27:07.712698, 2025-01-01 08:35:20.712698, and
#>   2025-01-01 08:42:18.712698
#> ℹ Sample of parsed times: 2025-01-01 08:27:07.712698, 2025-01-01
#>   08:35:20.712698, and 2025-01-01 08:42:18.712698
#> ℹ Time threshold for new session: 900 seconds
#> ℹ Total number of sessions: 2000
#> ℹ Number of unique users: 2000
#> ℹ Total number of actions: 27533
#> ℹ Maximum sequence length: 26 actions
#> ℹ Time range: 2025-01-01 08:01:16.009382 to 2025-01-01 13:03:20.238288
print(res, which = "sequence")
#> # A tibble: 2,000 × 26
#>    Action_T1 Action_T2  Action_T3  Action_T4 Action_T5 Action_T6 Action_T7
#>    <chr>     <chr>      <chr>      <chr>     <chr>     <chr>     <chr>    
#>  1 cohesion  consensus  discuss    synthesis adapt     consensus plan     
#>  2 plan      emotion    consensus  discuss   synthesis adapt     emotion  
#>  3 consensus coregulate monitor    consensus plan      emotion   consensus
#>  4 monitor   emotion    plan       discuss   synthesis consensus discuss  
#>  5 discuss   emotion    cohesion   NA        NA        NA        NA       
#>  6 plan      plan       consensus  plan      plan      plan      plan     
#>  7 plan      discuss    coregulate NA        NA        NA        NA       
#>  8 plan      emotion    consensus  discuss   consensus plan      consensus
#>  9 discuss   consensus  NA         NA        NA        NA        NA       
#> 10 emotion   cohesion   discuss    synthesis NA        NA        NA       
#> # ℹ 1,990 more rows
#> # ℹ 19 more variables: Action_T8 <chr>, Action_T9 <chr>, Action_T10 <chr>,
#> #   Action_T11 <chr>, Action_T12 <chr>, Action_T13 <chr>, Action_T14 <chr>,
#> #   Action_T15 <chr>, Action_T16 <chr>, Action_T17 <chr>, Action_T18 <chr>,
#> #   Action_T19 <chr>, Action_T20 <chr>, Action_T21 <chr>, Action_T22 <chr>,
#> #   Action_T23 <chr>, Action_T24 <chr>, Action_T25 <chr>, Action_T26 <chr>
print(res, which = "meta")
#> # A tibble: 2,000 × 26
#>    Action_T1 Action_T2  Action_T3  Action_T4 Action_T5 Action_T6 Action_T7
#>    <chr>     <chr>      <chr>      <chr>     <chr>     <chr>     <chr>    
#>  1 cohesion  consensus  discuss    synthesis adapt     consensus plan     
#>  2 plan      emotion    consensus  discuss   synthesis adapt     emotion  
#>  3 consensus coregulate monitor    consensus plan      emotion   consensus
#>  4 monitor   emotion    plan       discuss   synthesis consensus discuss  
#>  5 discuss   emotion    cohesion   NA        NA        NA        NA       
#>  6 plan      plan       consensus  plan      plan      plan      plan     
#>  7 plan      discuss    coregulate NA        NA        NA        NA       
#>  8 plan      emotion    consensus  discuss   consensus plan      consensus
#>  9 discuss   consensus  NA         NA        NA        NA        NA       
#> 10 emotion   cohesion   discuss    synthesis NA        NA        NA       
#> # ℹ 1,990 more rows
#> # ℹ 19 more variables: Action_T8 <chr>, Action_T9 <chr>, Action_T10 <chr>,
#> #   Action_T11 <chr>, Action_T12 <chr>, Action_T13 <chr>, Action_T14 <chr>,
#> #   Action_T15 <chr>, Action_T16 <chr>, Action_T17 <chr>, Action_T18 <chr>,
#> #   Action_T19 <chr>, Action_T20 <chr>, Action_T21 <chr>, Action_T22 <chr>,
#> #   Action_T23 <chr>, Action_T24 <chr>, Action_T25 <chr>, Action_T26 <chr>
print(res, which = "long")
#> # A tibble: 2,000 × 26
#>    Action_T1 Action_T2  Action_T3  Action_T4 Action_T5 Action_T6 Action_T7
#>    <chr>     <chr>      <chr>      <chr>     <chr>     <chr>     <chr>    
#>  1 cohesion  consensus  discuss    synthesis adapt     consensus plan     
#>  2 plan      emotion    consensus  discuss   synthesis adapt     emotion  
#>  3 consensus coregulate monitor    consensus plan      emotion   consensus
#>  4 monitor   emotion    plan       discuss   synthesis consensus discuss  
#>  5 discuss   emotion    cohesion   NA        NA        NA        NA       
#>  6 plan      plan       consensus  plan      plan      plan      plan     
#>  7 plan      discuss    coregulate NA        NA        NA        NA       
#>  8 plan      emotion    consensus  discuss   consensus plan      consensus
#>  9 discuss   consensus  NA         NA        NA        NA        NA       
#> 10 emotion   cohesion   discuss    synthesis NA        NA        NA       
#> # ℹ 1,990 more rows
#> # ℹ 19 more variables: Action_T8 <chr>, Action_T9 <chr>, Action_T10 <chr>,
#> #   Action_T11 <chr>, Action_T12 <chr>, Action_T13 <chr>, Action_T14 <chr>,
#> #   Action_T15 <chr>, Action_T16 <chr>, Action_T17 <chr>, Action_T18 <chr>,
#> #   Action_T19 <chr>, Action_T20 <chr>, Action_T21 <chr>, Action_T22 <chr>,
#> #   Action_T23 <chr>, Action_T24 <chr>, Action_T25 <chr>, Action_T26 <chr>
```
