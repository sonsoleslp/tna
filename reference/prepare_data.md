# Compute User Sessions from Event Data

Processes a dataset to create user sessions based on time gaps, ordering
columns, or actor groupings. It supports different ways to understand
order in user behavior and provides flexibility when widening the data.

## Usage

``` r
prepare_data(
  data,
  actor,
  time,
  action,
  order,
  time_threshold = 900,
  custom_format = NULL,
  is_unix_time = FALSE,
  unix_time_unit = "seconds",
  unused_fn = dplyr::first,
  timezone = "UTC",
  session = NULL
)
```

## Arguments

- data:

  A `data.frame` or containing the action/event data.

- actor:

  A `character` vector or an `expression` that represents a tidy
  selection of the names of the columns that represent a user/actor
  identifiers. If not provided and neither `time` nor `order` is
  specified, the entire dataset is treated as a single session. In the
  case of multiple actors, a readable `.actor` column is added while
  grouping uses the original columns to keep distinct combinations
  separate.

- time:

  A `character` string or an `expression` giving the name of the column
  representing timestamps of the action events.

- action:

  A `character` string or an `expression` giving the name of the column
  holding the information about the action taken.

- order:

  A `character` string or an `expression` giving the name of a column
  with sequence numbers or non-unique orderable values that indicate
  order within an `actor` group, if not present it will be ordered with
  all the data if no `actor` is available, used when widening the data.
  If both `actor` and `time` are specified, then the sequence order
  should be specified such that it determines the order of events within
  `actor` and each session.

- time_threshold:

  A positive `numeric` value specifying the time threshold in seconds
  for creating new time-based sessions. Set to `FALSE` to disable
  gap-based session splitting, so each actor-session combination forms
  one session. Defaults to 900 seconds.

- custom_format:

  A `character` string giving the format used to parse the `time`
  column.

- is_unix_time:

  A `logical` value indicating whether the `time` column is in Unix
  time. The default is `FALSE`.

- unix_time_unit:

  A `character` string giving the Unix time unit when `is_unix_time` is
  `TRUE`. The default is `"seconds"`. Valid options are `"seconds"`,
  `"milliseconds"`, or `"microseconds"`.

- unused_fn:

  How to handle extra columns when pivoting to wide format. See
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).
  The default is to keep all columns and to use the first value.

- timezone:

  An Olson time zone used to interpret timestamps that do not contain an
  explicit UTC offset. Explicit offsets such as `Z`, `+00:00`, and
  `-0500` are always honored. The default is `"UTC"` for reproducible
  results across systems. See
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).

- session:

  An optional tidy selection of one or more columns identifying explicit
  sessions within actors, such as a course or semester. When `time` is
  also supplied, each actor-session combination can be further split by
  `time_threshold`.

## Value

A `tna_data` object, which is a `list` with the following elements:

- `long_data`: The processed data in long format.

- `sequence_data`: The processed data on the sequences in wide format,
  with actions/events as different variables structured with sequences.

- `meta_data`: Other variables from the original data in wide format.

- `time_data`: Parsed timestamps in wide format when `time` is supplied,
  or `NULL` otherwise.

- `statistics`: A `list` containing summary statistics: total sessions,
  total actions, unique users, time range (if applicable), and top
  sessions and user by activities.

## Details

Session identity is based on observed combinations of the original actor
and session columns. Separator characters and high-cardinality marginal
levels therefore cannot merge distinct sessions. `.session_id` is the
collision-safe key and `.session_label` is its readable display label.

## See also

Other data:
[`import_data()`](https://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](https://sonsoles.me/tna/reference/import_onehot.md),
[`list_random_state_pools()`](https://sonsoles.me/tna/reference/list_random_state_pools.md),
[`print.tna_data()`](https://sonsoles.me/tna/reference/print.tna_data.md),
[`random_group_tna()`](https://sonsoles.me/tna/reference/random_group_tna.md),
[`random_tna()`](https://sonsoles.me/tna/reference/random_tna.md),
[`random_tna_mmm()`](https://sonsoles.me/tna/reference/random_tna_mmm.md),
[`simulate.group_tna()`](https://sonsoles.me/tna/reference/simulate.group_tna.md),
[`simulate.tna()`](https://sonsoles.me/tna/reference/simulate.tna.md)

## Examples

``` r
results <- prepare_data(
  group_regulation_long, actor = "Actor", time = "Time", action = "Action"
)
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
print(results$sequence_data)
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
print(results$meta_data)
#> # A tibble: 2,000 × 8
#>    .session_id Actor Achiever Group Course Time                .session_nr
#>    <chr>       <int> <chr>    <dbl> <chr>  <dttm>                    <int>
#>  1 1 s1            1 High         1 A      2025-01-01 08:27:07           1
#>  2 2 s1            2 High         1 A      2025-01-01 08:27:33           1
#>  3 3 s1            3 High         1 A      2025-01-01 08:24:45           1
#>  4 4 s1            4 High         1 A      2025-01-01 08:22:07           1
#>  5 5 s1            5 High         1 A      2025-01-01 08:22:50           1
#>  6 6 s1            6 High         1 A      2025-01-01 08:22:25           1
#>  7 7 s1            7 High         1 A      2025-01-01 08:26:04           1
#>  8 8 s1            8 High         1 A      2025-01-01 08:26:46           1
#>  9 9 s1            9 High         1 A      2025-01-01 08:25:56           1
#> 10 10 s1          10 High         1 A      2025-01-01 08:23:45           1
#> # ℹ 1,990 more rows
#> # ℹ 1 more variable: .session_label <chr>
print(results$statistics)
#> $total_sessions
#> [1] 2000
#> 
#> $total_actions
#> [1] 27533
#> 
#> $max_sequence_length
#> [1] 26
#> 
#> $unique_users
#> [1] 2000
#> 
#> $sessions_per_user
#> # A tibble: 2,000 × 2
#>    Actor n_sessions
#>    <int>      <int>
#>  1     1          1
#>  2     2          1
#>  3     3          1
#>  4     4          1
#>  5     5          1
#>  6     6          1
#>  7     7          1
#>  8     8          1
#>  9     9          1
#> 10    10          1
#> # ℹ 1,990 more rows
#> 
#> $actions_per_session
#> # A tibble: 2,000 × 2
#>    .session_id n_actions
#>    <chr>           <int>
#>  1 1010 s1            26
#>  2 1015 s1            26
#>  3 1030 s1            26
#>  4 1092 s1            26
#>  5 1106 s1            26
#>  6 1107 s1            26
#>  7 1153 s1            26
#>  8 1184 s1            26
#>  9 1209 s1            26
#> 10 1267 s1            26
#> # ℹ 1,990 more rows
#> 
#> $time_range
#> [1] "2025-01-01 08:01:16 UTC" "2025-01-01 13:03:20 UTC"
#> 

# Custom order column
data_ordered <- tibble::tibble(
   user = c("A", "A", "A", "B", "B", "C", "C", "C"),
   order = c(1, 2, 3, 1, 2, 1, 2, 3),
   action = c(
     "view", "click", "add_cart", "view",
     "checkout", "view", "click", "share"
   )
)
results_ordered <- prepare_data(
  data_ordered, actor = "user", order = "order", action = "action"
)
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 8 rows, 3 columns
#> ℹ Using provided `order` column to create sequences.
#> ℹ Total number of sessions: 3
#> ℹ Number of unique users: 3
#> ℹ Total number of actions: 8
#> ℹ Maximum sequence length: 3 actions
print(results_ordered$sequence_data)
#> # A tibble: 3 × 3
#>   T1    T2       T3      
#>   <chr> <chr>    <chr>   
#> 1 view  click    add_cart
#> 2 view  checkout NA      
#> 3 view  click    share   
print(results_ordered$meta_data)
#> # A tibble: 3 × 5
#>   .session_id user  order .session_nr .session_label
#>   <chr>       <chr> <dbl>       <int> <chr>         
#> 1 1           A         1           1 A             
#> 2 2           B         1           1 B             
#> 3 3           C         1           1 C             
print(results_ordered$statistics)
#> $total_sessions
#> [1] 3
#> 
#> $total_actions
#> [1] 8
#> 
#> $max_sequence_length
#> [1] 3
#> 
#> $unique_users
#> [1] 3
#> 
#> $sessions_per_user
#> # A tibble: 3 × 2
#>   user  n_sessions
#>   <chr>      <int>
#> 1 A              1
#> 2 B              1
#> 3 C              1
#> 
#> $actions_per_session
#> # A tibble: 3 × 2
#>   .session_id n_actions
#>   <chr>           <int>
#> 1 1                   3
#> 2 3                   3
#> 3 2                   2
#> 

# No actor scenario leading to a single session
data_single_session <- tibble::tibble(
  action = c(
    "view", "click", "add_cart", "view",
    "checkout", "view", "click", "share"
   )
)
results_single <- prepare_data(data_single_session, action = "action")
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 8 rows, 1 columns
#> ℹ No `time` or `order` column provided. Treating the entire dataset as one
#>   session.
#> ℹ Total number of sessions: 1
#> ℹ Total number of actions: 8
#> ℹ Maximum sequence length: 8 actions
print(results_single$sequence_data)
#> # A tibble: 1 × 8
#>   T1    T2    T3       T4    T5       T6    T7    T8   
#>   <chr> <chr> <chr>    <chr> <chr>    <chr> <chr> <chr>
#> 1 view  click add_cart view  checkout view  click share
print(results_single$meta_data)
#> # A tibble: 1 × 3
#>   .session_id .session_nr .session_label
#>   <chr>             <int> <chr>         
#> 1 1                     1 session       
print(results_single$statistics)
#> $total_sessions
#> [1] 1
#> 
#> $total_actions
#> [1] 8
#> 
#> $max_sequence_length
#> [1] 8
#> 
#> $actions_per_session
#> # A tibble: 1 × 2
#>   .session_id n_actions
#>   <chr>           <int>
#> 1 1                   8
#> 

# Multiple actors
data_multi_actor <- tibble::tibble(
  user = c("A", "A", "A", "A", "B", "B", "B", "B"),
  session = c(1, 1, 2, 2, 1, 1, 2, 2),
  action = c(
    "view", "click", "add_cart", "view",
    "checkout", "view", "click", "share"
  )
)
results_multi_actor <- prepare_data(
  data_multi_actor, actor = c("user", "session"), action = "action"
)
#> ── Preparing Data ──────────────────────────────────────────────────────────────
#> ℹ Input data dimensions: 8 rows, 3 columns
#> ℹ No `time` or `order` column provided. Using `actor` as a session identifier.
#> ℹ Total number of sessions: 4
#> ℹ Number of unique users: 4
#> ℹ Total number of actions: 8
#> ℹ Maximum sequence length: 2 actions
print(results_multi_actor$sequence_data)
#> # A tibble: 4 × 2
#>   T1       T2   
#>   <chr>    <chr>
#> 1 view     click
#> 2 checkout view 
#> 3 add_cart view 
#> 4 click    share
print(results_multi_actor$meta_data)
#> # A tibble: 4 × 6
#>   .session_id user  session .actor .session_nr .session_label
#>   <chr>       <chr>   <dbl> <chr>        <int> <chr>         
#> 1 1           A           1 A-1              1 A-1           
#> 2 2           B           1 B-1              1 B-1           
#> 3 3           A           2 A-2              1 A-2           
#> 4 4           B           2 B-2              1 B-2           
print(results_multi_actor$statistics)
#> $total_sessions
#> [1] 4
#> 
#> $total_actions
#> [1] 8
#> 
#> $max_sequence_length
#> [1] 2
#> 
#> $unique_users
#> [1] 4
#> 
#> $sessions_per_user
#> # A tibble: 4 × 2
#>   .actor n_sessions
#>   <chr>       <int>
#> 1 A-1             1
#> 2 B-1             1
#> 3 A-2             1
#> 4 B-2             1
#> 
#> $actions_per_session
#> # A tibble: 4 × 2
#>   .session_id n_actions
#>   <chr>           <int>
#> 1 1                   2
#> 2 2                   2
#> 3 3                   2
#> 4 4                   2
#> 
```
