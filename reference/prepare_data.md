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
  unused_fn = dplyr::first
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
  case of multiple actors, a new `.actor` column is added that
  represents the interaction of the given columns.

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

  An `integer` specifying the time threshold in seconds for creating new
  time-based sessions. Defaults to 900 seconds.

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

## Value

A `tna_data` object, which is a `list` with the following elements:

- `long_data`: The processed data in long format.

- `sequence_data`: The processed data on the sequences in wide format,
  with actions/events as different variables structured with sequences.

- `meta_data`: Other variables from the original data in wide format.

- `statistics`: A `list` containing summary statistics: total sessions,
  total actions, unique users, time range (if applicable), and top
  sessions and user by activities.

## See also

Other data:
[`import_data()`](http://sonsoles.me/tna/reference/import_data.md),
[`import_onehot()`](http://sonsoles.me/tna/reference/import_onehot.md),
[`print.tna_data()`](http://sonsoles.me/tna/reference/print.tna_data.md),
[`simulate.tna()`](http://sonsoles.me/tna/reference/simulate.tna.md)

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
#>    Action_T1 Action_T2 Action_T3 Action_T4 Action_T5  Action_T6  Action_T7
#>    <chr>     <chr>     <chr>     <chr>     <chr>      <chr>      <chr>    
#>  1 cohesion  consensus discuss   synthesis adapt      consensus  plan     
#>  2 emotion   cohesion  discuss   synthesis NA         NA         NA       
#>  3 plan      consensus plan      NA        NA         NA         NA       
#>  4 discuss   discuss   consensus plan      cohesion   consensus  discuss  
#>  5 cohesion  consensus plan      plan      monitor    plan       consensus
#>  6 discuss   adapt     cohesion  consensus discuss    emotion    cohesion 
#>  7 discuss   emotion   cohesion  consensus coregulate coregulate plan     
#>  8 cohesion  plan      consensus plan      consensus  discuss    discuss  
#>  9 emotion   cohesion  emotion   plan      monitor    discuss    emotion  
#> 10 emotion   cohesion  consensus plan      plan       plan       plan     
#> # ℹ 1,990 more rows
#> # ℹ 19 more variables: Action_T8 <chr>, Action_T9 <chr>, Action_T10 <chr>,
#> #   Action_T11 <chr>, Action_T12 <chr>, Action_T13 <chr>, Action_T14 <chr>,
#> #   Action_T15 <chr>, Action_T16 <chr>, Action_T17 <chr>, Action_T18 <chr>,
#> #   Action_T19 <chr>, Action_T20 <chr>, Action_T21 <chr>, Action_T22 <chr>,
#> #   Action_T23 <chr>, Action_T24 <chr>, Action_T25 <chr>, Action_T26 <chr>
print(results$meta_data)
#> # A tibble: 2,000 × 7
#>    .session_id   Actor Achiever Group Course Time                .session_nr
#>    <chr>         <int> <chr>    <dbl> <chr>  <dttm>                    <int>
#>  1 1 session1        1 High         1 A      2025-01-01 08:27:07           1
#>  2 10 session1      10 High         1 A      2025-01-01 08:23:45           1
#>  3 100 session1    100 High        10 A      2025-01-01 10:11:50           1
#>  4 1000 session1  1000 High       100 B      2025-01-01 09:12:00           1
#>  5 1001 session1  1001 Low        101 B      2025-01-01 09:18:40           1
#>  6 1002 session1  1002 Low        101 B      2025-01-01 09:18:53           1
#>  7 1003 session1  1003 Low        101 B      2025-01-01 09:18:05           1
#>  8 1004 session1  1004 Low        101 B      2025-01-01 09:22:26           1
#>  9 1005 session1  1005 Low        101 B      2025-01-01 09:22:31           1
#> 10 1006 session1  1006 Low        101 B      2025-01-01 09:15:23           1
#> # ℹ 1,990 more rows
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
#>    .session_id   n_actions
#>    <chr>             <int>
#>  1 1010 session1        26
#>  2 1015 session1        26
#>  3 1030 session1        26
#>  4 1092 session1        26
#>  5 1106 session1        26
#>  6 1107 session1        26
#>  7 1153 session1        26
#>  8 1184 session1        26
#>  9 1209 session1        26
#> 10 1267 session1        26
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
#> # A tibble: 3 × 3
#>   .session_id user  order
#>   <chr>       <chr> <dbl>
#> 1 A           A         1
#> 2 B           B         1
#> 3 C           C         1
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
#> 1 A                   3
#> 2 C                   3
#> 3 B                   2
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
#> # A tibble: 1 × 1
#>   .session_id
#>   <chr>      
#> 1 session    
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
#> 1 session             8
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
#> # A tibble: 4 × 4
#>   .session_id user  session .actor
#>   <fct>       <chr>   <dbl> <fct> 
#> 1 A-1         A           1 A-1   
#> 2 B-1         B           1 B-1   
#> 3 A-2         A           2 A-2   
#> 4 B-2         B           2 B-2   
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
#>   <fct>       <int>
#> 1 A-1             1
#> 2 B-1             1
#> 3 A-2             1
#> 4 B-2             1
#> 
#> $actions_per_session
#> # A tibble: 4 × 2
#>   .session_id n_actions
#>   <fct>           <int>
#> 1 A-1                 2
#> 2 B-1                 2
#> 3 A-2                 2
#> 4 B-2                 2
#> 
```
