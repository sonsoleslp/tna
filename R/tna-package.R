#' The `tna` package.
#'
#' @name tna-package
#' @description Provides tools for performing transition network analysis (TNA),
#' including functions for building TNA models, plotting transition networks,
#' and calculating centrality measures. The package relies on the `qgraph`
#' and `igraph` for network plotting and centrality measure calculations.
#'
#' @author Sonsoles López-Pernas, Santtu Tikka, Mohamed Saqr
#' @references
#' Saqr M., López-Pernas S., Törmänen T., Kaliisa R., Misiejuk K., Tikka S. (2024).
#' Transition Network Analysis: A Novel Framework for Modeling, Visualizing,
#' and Identifying the Temporal Patterns of Learners and Learning Processes.
#' <https://arxiv.org/abs/2411.15486>
#'
#' Banerjee A., Chandrasekhar A., Duflo E., Jackson M. (2014).
#' Gossip: Identifying Central Individuals in a Social Network.
#' Working Paper.
#'
#' Kivimaki, I., Lebichot, B., Saramaki, J., Saerens, M. (2016).
#' Two betweenness centrality measures based on Randomized Shortest Paths.
#' *Scientific Reports*, 6, 19668.
#'
#' Serrano, M. A., Boguna, M., & Vespignani, A. (2009).
#' Extracting the multiscale backbone of complex weighted networks.
#' *Proceedings of the National Academy of Sciences*, 106, 6483-6488.
#'
#' Zhang, B., Horvath, S. (2005).
#' A general framework for weighted gene co-expression network analysis.
#' *Statistical Applications in Genetics and Molecular Biology*, 4(1).
#'
#'
"_PACKAGE"

#' Example data on student engagement
#'
#' Students' engagement states (Active / Average / Disengaged)
#' throuhgout a whole study program. The data was generated synthetically
#' based on the article "The longitudinal association between engagement and
#' achievement varies by time, students’ profiles, and achievement state:
#' A full program study" (doi: 10.1016/j.compedu.2023.104787)
"engagement"

#' Example data on group regulation
#'
#' Students' regulation during collaborative learning. Students' interactions
#' were coded as:  "adapt", "cohesion", "consensus", "coregulate", "discuss",
#' "emotion", "monitor", "plan", "synthesis"
#' The data was generated synthetically.
"group_regulation"
