#' @name mlr_measures_stab.<%= id %>
#'
#' @details
#' This measure operates on the [ResampleResult] and requires the model to be
#' stored (see `store_model` in [resample()]/[benchmark()].
#' It is not possible to score a [Prediction] individually or to score resamplings with less
#' than two iterations.
#'
#' @family stability measures
