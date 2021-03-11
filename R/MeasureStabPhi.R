#' @title Phi Stability Measure
#'
#' @description
#' Calculates the stability of the sets of selected features
#' as the mean correlation between all pairs of 0/1 dummy vectors \eqn{z}.
#' These vectors have a 1 in the \eqn{i}-th position if the \eqn{i}-th feature has been selected.
#' With \eqn{m} features and \eqn{p} sets, this can be written as
#' \deqn{\frac{2}{m (m-1)} \sum_{i =1}^{p-1} \sum_{j = i + 1}^{p} \mathrm{Cor}(z_i, z_j).}
#'
#' @templateVar id phi
#' @template measure_stab
#' @template section_dictionary_measure
#'
#' @note
#' More stability measures can be found in the package \CRANpkg{stabm}.
#' For a comparison of stability measures, see `r cite_bib("bommert_2017")` where this measure is
#' called \dQuote{SC}.
#'
#' @section Meta Information:
#' `r rd_info(msr("stab.phi"))`
#'
#' @references
#' `r format_bib("bommert_2017", "nogueira_2016")`
#' @template seealso_measure
#' @export
MeasureStabPhi = R6Class("MeasureStabPhi",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "phi",
        task_type = NA_character_,
        properties = "requires_model",
        predict_type = "response",
        range = c(-1, 1),
        minimize = FALSE,
        man = "mlr3::mlr_measures_stab.phi"
      )
    },

    #' @description
    #' A custom aggregation function.
    #' @param rr [ResampleResult].
    aggregate = function(rr) {
      fn = rr$task$feature_names
      M = cor(vapply(rr$learners, function(x) fn %in% x$selected_features(), logical(length(fn))))
      mean(M[lower.tri(M)])
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      stopf("This measure cannot be applied on single predictions")
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("stab.phi", MeasureStabPhi)
