#' Build Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model from a given sequence.
#'
#' @param sequence A sequence of events or states to be used for building the Markov model.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{\code{Matrix}}{A matrix of transition probabilities.}
#'   \item{\code{Matrix0}}{A transition probability matrix without loops (the diagonal is set to zero).}
#'   \item{\code{inits}}{A vector of initial state probabilities.}
#'   \item{\code{Labels}}{The labels of the states.}
#'   \item{\code{colors}}{A color palette for the states.}
#' }
#'
#' @details
#' This function takes a sequence of events or states and builds a Markov model using the \code{build_mm} function from \code{seqHMM}.
#' It extracts the transition probabilities and initial probabilities from the model and stores them in a list
#' along with the state labels. Additionally, it creates a transition matrix with zero diagonal entries (without loops).
#'
#' @examples
#' \dontrun{
#'   library(TraMineR)
#'   data(biofam3c)
#'
#'   # Preparing the sequence data
#'   seq_data <- seqdef(biofam3c$biofam)
#'
#'   # Building a transition matrix from the sequence data
#'   tna_model <- build.tna(seq_data)
#' }
#'
#' @export
build.tna <- function(sequence) {
  modelx <- list()
  mkvmodel <- seqHMM::build_mm(sequence)
  transits <- mkvmodel$transition_probs
  intis <- mkvmodel$initial_probs
  Labels <- colnames(transits)

  modelx$Matrix <- transits
  modelx$inits <- intis
  modelx$Labels <- Labels
  modelx$colors <- cpal(sequence)
  modelx$Matrix -> Diag
  diag(Diag) <- 0
  modelx$Matrix0 <- Diag

  return(modelx)
}
