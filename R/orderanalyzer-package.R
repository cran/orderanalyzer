#' Extracting order position tables from PDF-based order documents
#'
#' This packages provides functions for extracting text and order-position-tables from
#' PDF-based order documents.
#'
#' \tabular{ll}{ Package: \tab orderanalyzer\cr Type: \tab Package\cr Version:
#' \tab 1.0.0\cr Date: \tab 2024-12-11\cr License: \tab GPL-3\cr Depends: \tab
#' R (>= 4.3.0)}
#'
#' @name orderanalyzer-package
#' @aliases orderanalyzer-package orderanalyzer
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Joerg Bauer \email{joerg.bauer@@th-deg.de}
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom data.table := data.table as.data.table setnafill
#' @importFrom matrixcalc shift.left shift.right
#' @importFrom quanteda tokens_ngrams tokens
#' @importFrom rlist list.sort
#' @importFrom stats kmeans na.omit median dist setNames
#' @importFrom stringr str_c str_ends str_count str_detect str_extract str_length str_locate str_replace_all str_split str_split_fixed str_squish str_starts str_to_lower str_to_upper str_trim word str_which str_remove_all str_remove str_sub str_extract_all regex str_flatten str_to_title str_replace
#' @importFrom tibble add_column repair_names
#' @importFrom tidyr separate unite drop_na pivot_wider pivot_longer separate_rows
#' @importFrom utils tail combn
#' @importFrom purrr is_empty partial
#' @importFrom digest digest
#' @importFrom lubridate parse_date_time
NULL
