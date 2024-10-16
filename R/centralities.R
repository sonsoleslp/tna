#' Calculate Centralities for a Transition Matrix
#'
#' This function calculates several centrality measures. See 'Details' for
#' information about the measures.
#'
#' The following measures are provided:
#'
#'   * `OutStrength`\cr Outgoing strength centrality, calculated using
#'     [igraph::strength()] with `mode = "out"`. It measures the total weight
#'     of the outgoing edges from each node.
#'   * `InStrength`\cr Incoming strength centrality, calculated using
#'     [igraph::strength()] with `mode = "in"`. It measures the total weight
#'     of the incoming edges to each node.
#'   * `ClosenessIn`\cr Closeness centrality (incoming), calculated using
#'     [igraph::closeness()] with `mode = "in"`. It measures how close a node
#'     is to all other nodes based on the incoming paths.
#'   * `ClosenessOut`\cr Closeness centrality (outgoing), calculated using
#'     [igraph::closeness()] with `mode = "out"`. It measures how close a node
#'     is to all other nodes based on the outgoing paths.
#'   * `Closeness`\cr Closeness centrality (overall), calculated using
#'     [igraph::closeness()] with `mode = "all"`. It measures how close a node
#'     is to all other nodes based on both incoming and outgoing paths.
#'   * `Betweenness`\cr Betweenness centrality based on randomized shortest
#'     paths (Kivimäki et al. 2016). It measures the extent to which a
#'     node lies on the shortest paths between other nodes.
#'   * `Diffusion`\cr Diffusion centrality of Banerjee et.al. (2014).
#'     It measures the influence of a node in spreading information through
#'     the network.
#'   * `Clustering`\cr Signed clustering coefficient of Zhang and Horvath (2005)
#'     based on the symmetric adjacency matrix (sum of the adjacency matrix
#'     and its transpose). It measures the degree to which nodes tend to
#'     cluster together.
#'
#' @export
#' @rdname centralities
#' @param x A square matrix representing transition probabilities or adjacency,
#'   or a `tna` object.
#' @param measures A `character` vector indicating which centrality
#'   measures should be computed. If `NULL`, all available measures are
#'   returned. See 'Details' for available measures. The elements are partially
#'   matched ignoring case.
#' @param loops A `logical` value indicating whether to include loops in the
#'   network when computing the centrality measures (default is `FALSE`).
#' @param normalize  A `logical` value indicating whether the centralities
#'   should be normalized (default is `FALSE`).
#' @param cluster Index of the cluster for which to compute the centralities or
#'   `NULL` if there are no clusters or if centralities should be computed for
#'   all clusters.
#' @param ... Ignored.
#' @return A `centralities` object which is a tibble (`tbl_df`)
#'   containing centrality measures for each state.
#' @references
#' Banerjee, A., A. Chandrasekhar, E. Duflo, and M. Jackson (2014).
#' Gossip: Identifying Central Individuals in a Social Network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., & Saerens, M. (2016).
#' Two betweenness centrality measures based on Randomized Shortest Paths.
#' Scientific Reports, 6, 19668.
#'
#' Zhang, B., & Horvath, S. (2005).
#' A general framework for weighted gene co-expression network analysis.
#' Statistical Applications in Genetics and Molecular Biology, 4(1).
#' @family core
#' @author
#' Mohammed Saqr (\email{mohammed.saqr@uef.fi})
#' @examples
#' tna_model <- build_tna(engagement)
#'
#' # Centrality measures including loops in the network
#' centralities(tna_model)
#'
#' # Centrality measures excluding loops in the network
#' centralities(tna_model, loops = FALSE)
#'
#' # Centrality measures normalized
#' centralities(tna_model, normalize = TRUE)
#'
centralities <- function(x, loops = FALSE, cluster = NULL,
                         normalize = FALSE, measures = NULL, ...) {
  UseMethod("centralities")
}

#' @export
#' @rdname centralities
centralities.tna <- function(x, loops = FALSE, cluster = NULL,
                             normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )
  if (length(x$transits) == 1L) {
    centralities_(
      x$transits[[1]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$transits) > 1L && !is.null(cluster)) {
    centralities_(
      x$transits[[cluster]],
      loops = loops,
      normalize = normalize,
      measures = measures
    )
  } else if (length(x$transits) > 1L) {
    centrality_list <- list()
    clusternames <- names(x$transits)
    for (i in seq_along(x$transits)){
      centrality_list[[i]] <- centralities_(
        x$transits[[i]],
        loops = loops,
        normalize = normalize,
        measures = measures
      )
      centrality_list[[i]]$Cluster <- clusternames[i]
    }
    structure(
      dplyr::bind_rows(centrality_list) |>
        dplyr::mutate(
          Cluster = factor(!!rlang::sym("Cluster"), levels = clusternames)
        ),
      class = c("centralities", "tbl_df", "tbl", "data.frame")
    )
  }
}

#' @export
#' @rdname centralities
centralities.matrix <- function(x, loops = FALSE, cluster = NULL,
                                normalize = FALSE, measures = NULL, ...) {
  stopifnot_(
    is.matrix(x),
    "Argument {.arg x} must be a {.cls matrix}."
  )
  centralities_(x, loops, normalize, measures)
}

#' Internal function to calculate various centrality measures
#'
#' @param x An adjacency matrix of a directed weighted graph
#' @noRd
centralities_ <- function(x, loops, normalize, measures) {
  stopifnot_(
    checkmate::test_flag(x = loops),
    "Argument {.arg loops} must be a single {.cls logical} value."
  )
  stopifnot_(
    checkmate::test_flag(x = normalize),
    "Argument {.arg normalize} must be a single {.cls logical} value."
  )
  default_measures <- c(
    "OutStrength",
    "InStrength",
    "ClosenessIn",
    "ClosenessOut",
    "Closeness",
    "Betweenness",
    "Diffusion",
    "Clustering"
  )
  measures <- ifelse_(is.null(measures), default_measures, measures)
  stopifnot_(
    checkmate::test_character(
      x = measures,
      any.missing = FALSE,
      unique = TRUE,
    ),
    "Argument {.arg measures} must be a {.cls character} vector."
  )
  lower_measures <- tolower(measures)
  lower_defaults <- tolower(default_measures)
  measures_match <- pmatch(lower_measures, lower_defaults)
  no_match <- is.na(measures_match)
  invalid_measures <- measures[no_match]
  valid_measures <- measures_match[!no_match]
  stopifnot_(
    length(invalid_measures) == 0L,
    c(
      "Argument {.arg measures} contains invalid centrality measures:",
      `x` = "Measure{?s} {.val {invalid_measures}} {?is/are} not recognized."
    )
  )
  diag(x) <- ifelse_(loops, diag(x), 0)
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = x,
    mode = "directed",
    weighted = TRUE
  )
  OutStrength <- igraph::strength(g, mode = "out")
  InStrength <- igraph::strength(g, mode = "in")
  ClosenessIn <- igraph::closeness(g, mode = "in")
  ClosenessOut <- igraph::closeness(g, mode = "out")
  Closeness <- igraph::closeness(g, mode = "all")
  Betweenness <- rsp_bet(x)
  Diffusion <- diffusion(x)
  Clustering <- wcc(x + t(x))

  out <- data.frame(
    OutStrength,
    InStrength,
    ClosenessIn,
    ClosenessOut,
    Closeness,
    Betweenness,
    Diffusion,
    Clustering
  )[valid_measures]

  if (normalize) {
    out <- out |>
      dplyr::mutate_at(dplyr::vars(measures), ranger)
  }
  structure(
    tibble::rownames_to_column(out, "State") |>
      dplyr::mutate(
        State = factor(!!rlang::sym("State"), levels = rownames(out))
      ),
    class = c("centralities", "tbl_df", "tbl", "data.frame")
  )
}


#' Convert a dataframe to a centralities object
#'
#' @export
#' @param df A dataframe.
#' @rdname as_centralities
as_centralities <- function(df) {
  structure(df, class = c("centralities", "tbl_df", "tbl", "data.frame"))
}



#' Compute diffusion centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
diffusion <- function(mat) {
  s <- 0
  n <- ncol(mat)
  p <- diag(1, n, n)
  for (i in seq_len(n)) {
    p <- p %*% mat
    s <- s + p
  }
  .rowSums(s, n, n)
}

#' Compute randomized shortest path betweenness centrality measure
#'
#' @param mat A transition probability matrix.
#' @noRd
rsp_bet <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  W <- mat * exp(-beta * mat^-1)
  Z <- solve(diag(1, n, n) - W)
  Zrecip <- Z^-1
  Zrecip_diag <- diag(Zrecip) * diag(1, n, n)
  out <- diag(tcrossprod(Z, Zrecip - n * Zrecip_diag) %*% Z)
  out <- round(out)
  out <- out - min(out) + 1
  out
}

#' Compute signed clustering coefficient
#'
#' @param mat A transition probability matrix.
#' @noRd
wcc <- function(mat) {
  diag(mat) <- 0
  n <- ncol(mat)
  num <- diag(mat %*% mat %*% mat)
  den <- .colSums(mat, n, n)^2 - .colSums(mat^2, n, n)
  num / den
}






#' Estimate Centrality Stability
#'
#' This function estimates the stability of centrality measures in a network using bootstrap resampling. It allows for dropping varying proportions of cases and calculates correlations between the original centralities and bootstrapped subsets. The function supports parallel processing to speed up the computations.
#'
#' @param x A `tna` object representing the temporal network analysis data. The object should be created from a `TraMineR` sequence object.
#' @param cluster An integer specifying the cluster of the `tna` object to analyze. Default is 1.
#' @param measures A character vector of centrality measures to estimate. The default measures are `"InStrength"`, `"OutStrength"`, and `"Betweenness"`.
#' @param normalize A logical value indicating whether to normalize the centrality measures. Default is `FALSE`.
#' @param n_bootstraps An integer specifying the number of bootstrap resamples to perform. Default is 1000.
#' @param drop_proportions A numeric vector specifying the proportions of cases to drop in each bootstrap iteration. Default is a sequence from 0.1 to 0.9 in increments of 0.1.
#' @param threshold A numeric value specifying the correlation threshold for calculating the CS-coefficient. Default is 0.7.
#' @param certainty A numeric value specifying the desired level of certainty for the CS-coefficient. Default is 0.95.
#' @param n_cores An integer specifying the number of cores to use for parallel processing. The default is the number of detected cores minus one.
#' @param return_detailed A logical value specifying whether to return detailed bootstrap results. If `TRUE`, detailed results are included in the output. Default is `FALSE`.
#' @param plot A logical value specifying whether to generate a plot of the stability results. Default is `TRUE`.
#'
#' @details
#' The function works by repeatedly resampling the data, dropping varying proportions of cases, and calculating centrality measures on the subsets. The correlation between the original centralities and the resampled centralities is calculated for each drop proportion. The stability of each centrality measure is then summarized using a centrality stability (CS) coefficient, which represents the proportion of dropped cases at which the correlations drop below a given threshold (default 0.7).
#'
#' The function uses parallel processing to speed up the resampling procedure, with the number of cores specified by the `n_cores` argument.
#'
#' The results can be visualized with an optional plot showing the stability of the centrality measures across different drop proportions, along with confidence intervals. The CS-coefficients are displayed in the plot's subtitle.
#'
#' @return A list containing the following components:
#' - `cs_coefficient`: The centrality stability (CS) coefficient for each measure.
#' - `correlations`: A matrix of correlations between the original centrality and the resampled centralities for each drop proportion.
#' - `detailed_results`: A detailed data frame of the bootstrap correlations, returned if `return_detailed = TRUE`.
#'
#' @examples
#' \dontrun{
#' # Assuming 'x' is a tna object
#' results <- estimate_centrality_stability(x, measures = c("InStrength", "OutStrength"))
#' }
#' @importFrom foreach %dopar%
#' @export
estimate_centrality_stability <- function(x,
                                          cluster = 1,
                                          measures = c("InStrength", "OutStrength", "Betweenness"),
                                          normalize = FALSE,
                                          n_bootstraps = 1000,
                                          drop_proportions = seq(0.1, 0.9, by = 0.1),
                                          threshold = 0.7,
                                          certainty = 0.95,
                                          n_cores = parallel::detectCores() - 1,
                                          return_detailed = FALSE,
                                          plot = TRUE) {



  stopifnot_(
    is_tna(x),
    "Argument {.arg x} must be a {.cls tna} object."
  )

  stopifnot_(
    !is.null(x$seq),
    "Argument {.arg x} must be a {.cls tna} object created from the `TraMineR` sequence object."
  )

  sequence_data <- x$seq[[cluster]]
  # Calculate original model and centralities
  original_centralities <- centralities(x, cluster = cluster, normalize = normalize, measures = measures)

  cl <- NULL
  tryCatch({

    # Set up parallel processing
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)

    parallel::clusterExport(cl, c("original_centralities", "centralities", "build_tna"), envir = environment())

    # Initialize results storage
    stability_results <- list()
    for (measure in measures) {
      stability_results[[measure]] <- matrix(NA, nrow = length(drop_proportions), ncol = n_bootstraps)
    }

    pb <- utils::txtProgressBar(min = 0, max = length(drop_proportions), style = 3)

    # Run computations for each drop proportion
    for (i in seq_along(drop_proportions)) {
      prop <- drop_proportions[i]
      n_cases <- nrow(sequence_data)
      n_drop <- floor(n_cases * prop)

      if (n_drop == 0) {
        cat(sprintf("Warning: No cases dropped for proportion %.2f, skipping...\n", prop))
        next
      }

      bootstrap_results <- foreach::foreach(b = 1:n_bootstraps,
                                            .combine = 'rbind',
                                            .packages = c('seqHMM', 'igraph', 'tna', 'foreach'),
                                            .errorhandling = 'pass') %dopar% {
                                              keep_indices <- sample(1:n_cases, n_cases - n_drop, replace = FALSE)
                                              subset_data <- sequence_data[keep_indices, ]

                                              subset_model <- build_tna(subset_data)

                                              if (is.null(subset_model) || is.null(subset_model$transits)) {
                                                return(rep(NA, length(measures)))
                                              }

                                              sapply(measures, function(measure) {
                                                subset_centrality <- centralities(subset_model$transits[[1]],
                                                                                  normalize = normalize, measures = c(measure)) |>
                                                  dplyr::pull(measure)

                                                cor(original_centralities  |> dplyr::pull(measure), subset_centrality, use = "complete.obs")
                                              })
                                            }


      for (j in seq_along(measures)) {
        measure <- measures[j]
        stability_results[[measure]][i,] <- bootstrap_results[,j]
      }

      utils::setTxtProgressBar(pb, i)
    }

    close(pb)

    # Check if results are empty
    for (measure in measures) {
      if (all(is.na(stability_results[[measure]]))) {
        cat(sprintf("Warning: All results for measure '%s' are NA.\n", measure))
      }
    }

    final_results <- list()

    cat("\n=== Centrality Stability Analysis ===\n\n")

    for (measure in measures) {
      cs_coef <- calculate_cs_coefficient(stability_results[[measure]],
                                          threshold = threshold,
                                          drop_proportions = drop_proportions,
                                          certainty = certainty)

      final_results[[measure]] <- list(
        cs_coefficient = cs_coef,
        correlations = stability_results[[measure]]
      )

      cat(sprintf("%s Centrality: CS-Coefficient = %.2f\n", measure, cs_coef))
    }

    if (return_detailed) {
      detailed_df <- lapply(stability_results, function(res) {
        data.frame(
          drop_proportion = rep(drop_proportions, each = n_bootstraps),
          correlation = as.vector(res)
        )
      })
      final_results$detailed_results <- detailed_df
    }

    if (plot) {
      plot_stability_results(invisible(final_results))
    }

    return(invisible(final_results))

  }, error = function(e) {
    cat("\nError occurred:", conditionMessage(e), "\n")
    stop(e)
  }, finally = {
    if (!is.null(cl)) {
      parallel::stopCluster(cl)
    }
  })
}



calculate_cs_coefficient <- function(correlation_matrix, threshold, certainty, drop_proportions) {
  proportions_above_threshold <- apply(correlation_matrix, 1, function(x) {
    mean(x >= threshold, na.rm = TRUE)
  })

  valid_indices <- which(proportions_above_threshold >= certainty)

  if (length(valid_indices) == 0) {
    return(0.0)
  } else {
    return(drop_proportions[max(valid_indices)])
  }
}

#' Plot Centrality Stability Results
#'
#' This function visualizes the centrality stability results produced by the `estimate_centrality_stability` function. It shows how different centrality measures' correlations change as varying proportions of cases are dropped, along with their confidence intervals (CIs).
#'
#' @param stability_results A list of stability results produced by `estimate_centrality_stability`. Each centrality measure contains:
#' - `correlations`: A matrix of correlations between the original centralities and resampled centralities for different proportions of dropped cases.
#' - `cs_coefficient`: The centrality stability (CS) coefficient, which represents the proportion of dropped cases where the correlation drops below the specified threshold.
#'
#' @details
#' The function aggregates the results for each centrality measure across multiple proportions of dropped cases (e.g., 0.1, 0.2, ..., 0.9) and calculates the mean and standard deviation for each proportion. The confidence intervals (CIs) are computed based on the standard deviations and displayed in the plot.
#'
#' If no valid data is available for a centrality measure (e.g., missing or NA values), the function skips that measure with a warning.
#'
#' The plot includes:
#' - The mean correlation for each centrality measure as a function of the proportion of dropped cases.
#' - Shaded confidence intervals representing the 95% CIs for each centrality measure.
#' - A horizontal dashed line at y = 0.7, indicating the typical threshold used for calculating the CS-coefficient.
#' - A subtitle listing the CS-coefficients for each centrality measure.
#'
#' @return A `ggplot` object displaying the stability analysis plot.
#'
#' @examples
#' \dontrun{
#' # Assuming 'stability_results' is a list from estimate_centrality_stability()
#' plot_stability_results(stability_results)
#' }
#'
#' @export
plot_stability_results <- function(stability_results) {
  plot_data <- data.frame()  # Initialize an empty dataframe to store all results
  cs_subtitle <- ""  # String to accumulate CS-coefficients for the subtitle

  # Iterate over all centrality measures, except detailed results
  for (measure_name in names(stability_results)) {
    if (measure_name != "detailed_results") {
      measure_results <- stability_results[[measure_name]]$correlations

      # Debugging step: check if measure_results has valid dimensions
      if (is.null(dim(measure_results)) || nrow(measure_results) == 0 || ncol(measure_results) == 0) {
        cat(sprintf("Warning: No valid data for measure '%s'. Skipping.\n", measure_name))
        next
      }

      means <- apply(measure_results, 1, mean, na.rm = TRUE)
      sds <- apply(measure_results, 1, sd, na.rm = TRUE)

      # Check for NaN/NA values in the standard deviations
      if (any(is.nan(sds)) || any(is.na(sds))) {
        cat(sprintf("Warning: NA or NaN values detected in the standard deviations for '%s'.\n", measure_name))
      }

      proportions <- seq(0.1, 0.9, by = 0.1)

      if (length(means) == length(proportions)) {
        measure_data <- data.frame(
          proportion = proportions,
          correlation = means,
          lower = means - 2 * sds,  # 95% CI lower bound
          upper = means + 2 * sds,  # 95% CI upper bound
          measure = measure_name
        )

        # Ensure no negative values for CIs
        measure_data$lower <- pmax(measure_data$lower, 0)
        measure_data$upper <- pmin(measure_data$upper, 1)

        plot_data <- rbind(plot_data, measure_data)

        # Collect CS-coefficients for the subtitle
        cs_coef <- stability_results[[measure_name]]$cs_coefficient
        cs_subtitle <- paste0(cs_subtitle, measure_name, ": CS = ", round(cs_coef, 2), "; ")
      } else {
        cat(sprintf("Warning: Length mismatch between means and proportions for '%s'.\n", measure_name))
      }
    }
  }

  if (nrow(plot_data) == 0) {
    stop("Error: No valid data to plot.")
  }

  # Remove the trailing semicolon and space from the subtitle
  cs_subtitle <- gsub("; $", "", cs_subtitle)

  # Create the ggplot with CIs (geom_ribbon) and CS-coefficients in the subtitle
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = proportion, y = correlation, color = measure)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.2) +  # Confidence intervals
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Centrality Stability Analysis",
      subtitle = paste("CS-Coeficients: ", cs_subtitle),  # Include CS-coefficients as a subtitle
      x = "Proportion of Cases Dropped",
      y = "Correlation with Original Centrality",
      color = "Centrality Measure",
      fill = "Centrality Measure"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(0, 1)

  # Print the plot
  print(p)
}
