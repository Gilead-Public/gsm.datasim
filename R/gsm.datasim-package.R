#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import lubridate
#' @import purrr
#' @importFrom arrow write_parquet
#' @importFrom glue glue
#' @importFrom workr MakeWorkflowList RunWorkflows
#' @importFrom utils modifyList setTxtProgressBar txtProgressBar
#' @importFrom logger log_info
#' @importFrom rlang zap
#' @importFrom stringr str_detect str_replace_all
#' @importFrom tictoc tic toc
#' @importFrom utils read.csv
#' @importFrom R6 R6Class
NULL

# R6 is for refresh.R / refresh-snapshot.R. Remove that when refresh is a 
# separate package.