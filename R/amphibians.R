#' Presence of amphibian species at Polish water reservoirs
#'
#' A multilabel data set describing 189 water reservoirs in Poland. The
#' explanatory variables summarize the site: the size and type of the
#' reservoir, the surrounding land use, and the proximity of roads and
#' buildings. The seven binary labels record whether a given amphibian species
#' is present. Species prevalence differs considerably, from 78\% for the brown
#' frog down to 11\% for the great crested newt, which makes the data set a
#' realistic illustration of multilabel problems with rare labels.
#'
#' The raw data hold 15 site characteristics, 10 of which are categorical.
#' After one-hot encoding those, 56 variables remain. To keep the number of
#' explanatory variables manageable, principal component analysis is applied to
#' the centered and scaled variables, and the smallest number of components
#' explaining at least 80\% of the total variance is retained. This leaves 25
#' components at 82\%. The labels are used as they are, with abbreviated names
#' so that printed output stays readable.
#'
#' The script that produced the stored object from the raw file, together with
#' the raw file itself, is available under \code{data-raw} in the package
#' sources at \url{https://github.com/djwtouw/CCNR}.
#'
#' @format A list with two elements:
#' \describe{
#'   \item{X}{Numeric matrix of principal components (\eqn{189 \times 25}),
#'     with columns \code{PC1} to \code{PC25}.}
#'   \item{Y}{Binary integer matrix of species presence (\eqn{189 \times 7}),
#'     with columns \code{GreenFrog} (green frogs), \code{BrownFrog} (brown
#'     frogs), \code{Toad} (common toad), \code{FireToad} (fire-bellied toad),
#'     \code{TreeFrog} (tree frog), \code{Newt} (common newt), and
#'     \code{CrestNewt} (great crested newt).}
#' }
#'
#' @source UCI Machine Learning Repository, Amphibians data set,
#' \doi{10.24432/C5H90N}. Described in Blachnik, M., Soltysiak, M., and
#' Dabrowska, D. (2019) "Predicting Presence of Amphibian Species Using
#' Features Obtained from GIS and Satellite Images", ISPRS International
#' Journal of Geo-Information, 8(3), 123, \doi{10.3390/ijgi8030123}.
#'
#' @example inst/doc/examples/example-amphibians.R
"amphibians"
