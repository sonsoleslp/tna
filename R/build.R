#' Build a Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model from a given sequence.
#' It takes a sequence of events or states and builds a Markov model using  [seqHMM::build_mm()].
#' It extracts the transition probabilities and initial probabilities from the model and stores them in a list
#' along with the state labels. Additionally, it creates a transition matrix with zero diagonal entries (without loops).
#'
#' @export
#' @param sequence A sequence of events or states to be used for building the Markov model.
#' @return An object of class `tna` which is a list containing the following elements:
#'
#'   * `Matrix` A matrix of transition probabilities.
#'   * `Matrix0` A transition probability matrix without loops (the diagonal is set to zero).
#'   * `inits` A vector of initial state probabilities.
#'   * `Labels` The labels of the states.
#'   * `colors` A color palette for the states.
#'
#' @examples
#' data(biofam3c, package = "seqHMM")
#'
#' # Preparing the sequence data
#' seq_data <- TraMineR::seqdef(as.data.frame(biofam3c[1:3]))
#'
#' # Building a transition matrix from the sequence data
#' tna_model <- build_tna(seq_data)
#'
build_tna <- function(sequence) {
  mkvmodel <- seqHMM::build_mm(sequence)
  transits <- mkvmodel$transition_probs
  inits <- mkvmodel$initial_probs
  labels <- colnames(transits)
  diag <- transits
  diag(diag) <- 0
  structure(
    list(
      matrix = transits,
      matrix0 = diag,
      inits = inits,
      labels = labels,
      colors = TraMineR::cpal(sequence)
    ),
    class = "tna"
  )
}
