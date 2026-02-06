# Print Found Cliques of a TNA Network

Print Found Cliques of a TNA Network

## Usage

``` r
# S3 method for class 'tna_cliques'
print(x, n = 6, first = 1, digits = getOption("digits"), ...)
```

## Arguments

- x:

  A `tna_cliques` object.

- n:

  An `integer` defining the maximum number of cliques to show. The
  defaults is `6`.

- first:

  An `integer` giving the index of the first clique to show. The default
  index is `1`.

- digits:

  An `integer` giving the minimal number of *significant* digits to
  print.

- ...:

  Ignored.

## Value

`x` (invisibly).

## See also

Clique-related functions
[`cliques()`](http://sonsoles.me/tna/reference/cliques.md),
[`plot.group_tna_cliques()`](http://sonsoles.me/tna/reference/plot.group_tna_cliques.md),
[`plot.tna_cliques()`](http://sonsoles.me/tna/reference/plot.tna_cliques.md),
[`print.group_tna_cliques()`](http://sonsoles.me/tna/reference/print.group_tna_cliques.md)

## Examples

``` r
model <- tna(group_regulation)
cliq <- cliques(model, size = 2)
print(cliq)
#> Number of 2-cliques = 35 (weight threshold = 0)
#> Showing 6 cliques starting from clique number 1
#> 
#> Clique 1
#>            monitor      plan
#> monitor 0.01814375 0.2156315
#> plan    0.07552379 0.3742082
#> 
#> Clique 2
#>            emotion    monitor
#> emotion 0.07684173 0.03630596
#> monitor 0.09071877 0.01814375
#> 
#> Clique 3
#>            emotion       plan
#> emotion 0.07684173 0.09975326
#> plan    0.14682475 0.37420822
#> 
#> Clique 4
#>           discuss    emotion
#> discuss 0.1948874 0.10579600
#> emotion 0.1018682 0.07684173
#> 
#> Clique 5
#>           discuss    monitor
#> discuss 0.1948874 0.02227284
#> monitor 0.3754361 0.01814375
#> 
#> Clique 6
#>            discuss       plan
#> discuss 0.19488737 0.01164262
#> plan    0.06789021 0.37420822
```
