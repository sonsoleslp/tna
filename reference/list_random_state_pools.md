# List Built-in Label Pools for [`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md)

Return the names of the curated state-label pools available to
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md) and
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
with their sizes.

## Usage

``` r
list_random_state_pools()
```

## Value

A named `integer` vector mapping pool names to pool sizes.

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`prepare_data()`](https://sonsoles.me/tna/reference/prepare_data.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
list_random_state_pools()
#>    engagement   engagement5    motivation   motivation4     attention 
#>             3             5             3             4             3 
#>        affect   performance        effort     cognitive metacognitive 
#>             3             3             3            12            12 
#>    behavioral        social 
#>            12            12 
```
