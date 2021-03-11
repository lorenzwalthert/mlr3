#' @title Jacard Index Stability Measure
#'
#' @description
#' Calculates the stability of the sets of selected features via the Jaccard index.
#' Given two sets of features \eqn{z_1} and \eqn{z_2}, it is defined as
#' \deqn{\frac{| z_1 \cap z_2|}{|z_1 \cup z_2|}.}{length(intersect(z_1, z_2)) / length(union(z_1, z_2)).}
#' For more than 2 sets, the mean of all pairwise Jaccard Indices is returned.
#'
#' @templateVar id jaccard
#' @template measure_stab
#' @template section_dictionary_measure
#'
#' @note
#' More stability measures can be found in the package \CRANpkg{stabm}.
#' For a comparison of stability measures, see `r cite_bib("bommert_2017")` where this measure is
#' called \dQuote{SJ}.
#'
#' @section Meta Information:
#' `r rd_info(msr("stab.jaccard"))`
#'
#' @references
#' `r format_bib("bommert_2017", "jaccard_1901")`
#'
#' @template seealso_measure
#' @export
MeasureStabJaccard = R6Class("MeasureStabJaccard",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "jaccard",
        task_type = NA_character_,
        properties = "requires_model",
        predict_type = "response",
        range = c(0, 1),
        minimize = FALSE,
        man = "mlr3::mlr_measures_stab.jaccard"
      )
    },

    #' @description
    #' A custom aggregation function.
    #' @param rr [ResampleResult].
    aggregate = function(rr) {
      fn = rr$task$feature_names
      sets = map(rr$learners, function(x) x$selected_features())
      if (sum(lengths(sets) == 0L) >= 2L)
        return(NA_real_)

      jaccard = function(x, y) {
        length(intersect(x, y)) / length(union(x, y))
      }

      mean(combn(sets, 2L, splat(jaccard)))
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      stopf("This measure cannot be applied on single predictions")
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("stab.jaccard", MeasureStabJaccard)
