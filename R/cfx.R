#' @import readr
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
NULL

#' List the CFX csv datasets found in a folder.
#'
#' @param folder a character vector specifying the path(s) to the folder containing the cfx datasets.
#'
#' @return A vector with the basenames (cfx_set) to the different datasets
#'
#' @export
list_cfx <- function(folder) {
  if (!all(dir.exists(folder))) stop("The folder does not exist")

  f <- list.files(folder, pattern = "^.*_Run Information.csv$", full.names = TRUE)

  if (length(f) == 0) {
    stop(sprintf("Could not detect any CFX csv export in this folder (%s)", folder))
  }

  f <- str_match(f, "^(.*)_Run Information.csv")[,2]
  return(f)
}


read_delim_silent <- function(folder, file) {
  # From: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
  # Removing the message telling that a column has no name
  if (file == "_Run Information.csv") cn = c("variable", "value")
  else cn = TRUE
  h <- function(w) if( any( grepl( "Missing column names filled in", w) ) ) invokeRestart( "muffleWarning" )
  .df <- withCallingHandlers(read_delim(paste0(folder, file), ";", na = c("None", "Non Num\xe9rique"), locale = locale("fr", decimal_mark = ","), col_types = cols(), col_names = cn), warning = h) %>%
    select(-matches("X1")) %>%
    setNames(make.names(names(.)))
  return(.df)
}

read_date <- function(cfx_set) {
  read_delim_silent(cfx_set, "_Run Information.csv") %>%
    filter(variable == "Run Ended") -> d
  parse_date(d[[1, 2]], "%m/%d/%Y %H:%M:%S UTC") -> d
  return(d)
}

read_gene2well <- function(cfx_set) {
  gene2well <- read_delim_silent(cfx_set, " -  Quantification Summary_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    select(Well, Target, Sample) %>%
    mutate(Date = read_date(cfx_set)) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) -> df
  return(df)
}


#' Load the Cq values
#'
#' @param cfx_set a string basename combining the folder and the common prefix of exported csv files. Use list_cfx to list the different sets in a folder.
#'
#' @return A dataframe containing the Cq values
#'
#' @export
read_cq <- function(cfx_set) {
  read_delim_silent(cfx_set, " -  Quantification Cq Results_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Date = read_date(cfx_set)) -> df
  return(df)
}

#' Load the RFU amplification values
#'
#' @param cfx_set a string basename combining the folder and the common prefix of exported csv files. Use list_cfx to list the different sets in a folder.
#'
#' @return A dataframe containing the RFU values kinetic
#'
#' @export
read_amplification <- function(cfx_set) {
  read_delim_silent(cfx_set, " -  Quantification Amplification Results_SYBR.csv") %>%
    gather(Well, RFU, -Cycle) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Column = parse_number(Column)) %>%
    inner_join(read_gene2well(cfx_set), by = c("Row", "Column")) -> df

  return(df)
}

#' Load the melt cruve peaks calculated by the CFX Manager software
#'
#' @param cfx_set a string basename combining the folder and the common prefix of exported csv files. Use list_cfx to list the different sets in a folder.
#'
#' @return A dataframe containing the peak informations
#'
#' @export
read_meltpeak <- function(cfx_set) {
  read_delim_silent(cfx_set, " -  Melt Curve Peak Results_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    mutate(Date = read_date(cfx_set)) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) -> df
  return(df)
}


#' Load the derivative of the melt curve RFU values kinetic
#'
#' @param cfx_set a string basename combining the folder and the common prefix of exported csv files. Use list_cfx to list the different sets in a folder.
#'
#' @return A dataframe containing the melt curve RFU derivative values
#'
#' @export
read_meltcurve <- function(cfx_set) {
  read_delim_silent(cfx_set, " -  Melt Curve Derivative Results_SYBR.csv") %>%
    gather(Well, RFU, -Temperature) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Column = parse_number(Column)) %>%
    inner_join(load.gene2well(cfx_set), by = c("Row", "Column")) -> df
  return(df)
}

