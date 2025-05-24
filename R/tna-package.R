#' The `tna` Package.
#'
#' @name tna-package
#' @family basic
#' @description Provides tools for performing transition network analysis (TNA),
#' including functions for building TNA models, plotting transition networks,
#' and calculating centrality measures. The package relies on the `qgraph`
#' and `igraph` for network plotting and centrality measure calculations.
#'
#' @author Sonsoles López-Pernas, Santtu Tikka, Mohammed Saqr
#' @references
#' Saqr M., López-Pernas S., Törmänen T., Kaliisa R., Misiejuk K., Tikka S. (2025).
#' Transition Network Analysis: A Novel Framework for Modeling, Visualizing,
#' and Identifying the Temporal Patterns of Learners and Learning Processes.
#' In *Proceedings of the 15th International Learning Analytics and Knowledge Conference (LAK '25)*, 351-361.
#'
#' Banerjee A., Chandrasekhar A., Duflo E., Jackson M. (2014).
#' Gossip: Identifying Central Individuals in a Social Network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., Saerens, M. (2016).
#' Two betweenness centrality measures based on Randomized Shortest Paths.
#' *Scientific Reports*, 6, 19668.
#'
#' Serrano, M. A., Boguna, M., Vespignani, A. (2009).
#' Extracting the multiscale backbone of complex weighted networks.
#' *Proceedings of the National Academy of Sciences*, 106, 6483-6488.
#'
#' Zhang, B., Horvath, S. (2005).
#' A general framework for weighted gene co-expression network analysis.
#' *Statistical Applications in Genetics and Molecular Biology*, 4(1).
#'
"_PACKAGE"

#' Example Data on Student Engagement
#'
#' Students' engagement states (Active / Average / Disengaged)
#' throughout a whole study program. The data was generated synthetically
#' based on the article "The longitudinal association between engagement and
#' achievement varies by time, students' profiles, and achievement state:
#' A full program study"
#'
#' @family datasets
#' @source \doi{10.1016/j.compedu.2023.104787}
#' @format A `stslist` object (sequence data).
"engagement"

#' Example Mixed Markov Model Fitted to the `engagement` Data
#'
#' @family datasets
#' @source The data was generated via `mixed_markov_model.R` in
#' <https://github.com/sonsoleslp/tna/tree/main/data-raw/>
#' @format A `mhmm` object.
"engagement_mmm"

#' Example Wide Data on Group Regulation
#'
#' Students' regulation during collaborative learning. Students' interactions
#' were coded as:  "adapt", "cohesion", "consensus", "coregulate", "discuss",
#' "emotion", "monitor", "plan", "synthesis"
#'
#' @family datasets
#' @source The data was generated synthetically.
#' @format A `data.frame` object.
"group_regulation"

#' Example Long Data on Group Regulation
#'
#' Students' regulation during collaborative learning. This is the same dataset
#' as `group_regulation` but in long format. In addition to students'
#' actions (`Action`), it contains the student identifier (`Actor`),
#' timestamp (`Time`), `Course` name, and collaboration `Group`. It
#' also includes a column (`Achiever`) indicating whether the student
#' is a high or low achiever.
#'
#' @family datasets
#' @source The data was generated synthetically from `group_regulation`
#' @format A `data.frame` object.
"group_regulation_long"
