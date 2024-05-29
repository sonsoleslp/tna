#' Build a Transition Network Analysis Model
#'
#' This function constructs a transition network analysis (TNA) model from a
#' given sequence. It takes a sequence of events or states and builds a Markov
#' model using [seqHMM::build_mm()]. It extracts the transition probabilities
#' and initial probabilities from the model and stores them in a `list` along
#' with the state labels. Additionally, it creates a transition matrix with
#' zero diagonal entries (without loops).
#'
#' @export
#' @param sequence A sequence of events or states to be used for building
#'   the Markov model.
#' @return An object of class `tna` which is a `list` containing the
#'   following elements:
#'
#'   * `matrix`\cr A matrix of transition probabilities.
#'   * `matrix0`\cr A transition probability matrix without loops
#'     (the diagonal is set to zero).
#'   * `inits`\cr A vector of initial state probabilities.
#'   * `labels`\cr The labels of the states.
#'   * `colors`\cr A color palette for the states.
#'
#' @examples
#' tna_model <- build_tna(engagement)
#' print(tna_model)
#'
build_tna <- function(sequence) {
  mkvmodel <- seqHMM::build_mm(sequence)
  transits <- mkvmodel$transition_probs
  transits_loopless <- transits
  diag(transits_loopless) <- 0
  structure(
    list(
      matrix = transits,
      matrix0 = transits_loopless,
      inits = mkvmodel$initial_probs,
      labels = colnames(transits)
    ),
    class = "tna"
  )
}
