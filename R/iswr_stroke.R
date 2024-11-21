#' Estonian stroke data
#'
#' Taken from the package `{ISwR}`, this example dataset is of all cases of 
#' stroke in Tartu, Estonia, during the period 1991–1993, with follow-up until 
#' January 1, 1996. The main edits here are to decorate this with specified 
#' variable types and define a binary classification of "dead or alive" at 12 
#' months from initial stroke.
#' 
#'
#' @format ## `iswr_stroke`
#' A data frame with 814 rows and 8 columns:
#' \describe{
#'   \item{Gender}{Patient's gender, coded as a factor variable with levels `Male` and `Female`}
#'   \item{Age}{Patients age recorded as an integer}
#'   \item{Diagnosis}{Patient's diagnosis denoting the type of stroke: `ICH`, `ID`, `INF`, and `SAH`}
#'   \item{Coma}{Whether the patient fell into a coma at the time of the original stroke, coded as a factor variable with levels  `Yes` or `No`}
#'   \item{Diabetes}{Patient's history of diabetes, coded as a factor variable with levels `Yes` and `No`}
#'   \item{MI}{Patient's history of myocardial infarction, coded as a factor variable with levels `Yes` and `No`}
#'   \item{Hypertension}{Patient's history of Hypertension, coded as a factor variable with levels `Yes` and `No`}
#'   \item{dead12}{Denoting whether the patient had died by 12 months or not, coded as a numeric variable `1 = yes`, `0 = no`}
#' }
#' @source <https://cran.r-project.org/web/packages/ISwR/index.html>
"iswr_stroke"