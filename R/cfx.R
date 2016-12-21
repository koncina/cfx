#' @import readr
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
NULL

read_delim_silent <- function(folder, file) {
  # From: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
  # Removing the message telling that a column has no name
  if (file == "_Run Information.csv") cn = c("variable", "value")
  else cn = TRUE
  h <- function(w) if( any( grepl( "Missing column names filled in", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(read_delim(paste0(folder, file), ";", na = c("None", "Non Num\xe9rique"), locale = locale("fr", decimal_mark = ","), col_types = cols(), col_names = cn), warning = h) %>%
    select(-matches("X1")) %>%
    setNames(make.names(names(.)))
}

read_date <- function(cfx_base) {
  read_delim_silent(cfx_base, "_Run Information.csv") %>%
    filter(variable == "Run Ended") -> d
  parse_date(d[[1, 2]], "%m/%d/%Y %H:%M:%S UTC")
}

read_gene2well <- function(cfx_base) {
  gene2well <- read_delim_silent(cfx_base, " -  Quantification Summary_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    select(Well, Target, Sample) %>%
    mutate(Date = read_date(cfx_base)) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE)
}

get_cfx_base <- function(cfx_run) {
  # If cfx_run is a folder we can try to extract a single run information file
  # To manage more than one file, it is cleaner and easier to use map rather than implementing it in the function
  if (dir.exists(cfx_run)) cfx_run <- list.files(cfx_run, pattern = "^.*_Run Information.csv$", full.names = TRUE)
  if (!(file.exists(cfx_run) && grepl("^(.*)_Run Information.csv", cfx_run) && length(cfx_run) == 1)) {
    stop("Provide a single Run Information file")
  }
  str_match(cfx_run, "^(.*)_Run Information.csv")[,2]
}

#' Load the Cq values
#'
#' @param cfx_run a string with the path to the CFX run information file or a folder containing a single run information file.
#'
#' @return A dataframe containing the Cq values
#'
#' @export
read_cq <- function(cfx_run) {
  cfx_base <- get_cfx_base(cfx_run)
  read_delim_silent(cfx_base, " -  Quantification Cq Results_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Date = read_date(cfx_base))
}

#' Load the RFU amplification values
#'
#' @param cfx_run a string with the path to the CFX run information file or a folder containing a single run information file.
#'
#' @return A dataframe containing the RFU values kinetic
#'
#' @export
read_amplification <- function(cfx_run) {
  cfx_base <- get_cfx_base(cfx_run)
  read_delim_silent(cfx_base, " -  Quantification Amplification Results_SYBR.csv") %>%
    gather(Well, RFU, -Cycle) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Column = parse_number(Column)) %>%
    inner_join(read_gene2well(cfx_base), by = c("Row", "Column"))
}

#' Load the melt cruve peaks calculated by the CFX Manager software
#'
#' @param cfx_run a string with the path to the CFX run information file or a folder containing a single run information file.
#'
#' @return A dataframe containing the peak informations
#'
#' @export
read_meltpeak <- function(cfx_run) {
  cfx_base <- get_cfx_base(cfx_run)
  read_delim_silent(cfx_base, " -  Melt Curve Peak Results_0.csv") %>%
    mutate_at("Sample", as.character) %>%
    mutate(Date = read_date(cfx_base)) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE)
}

#' Load the derivative of the melt curve RFU values kinetic
#'
#' @param cfx_run a string with the path to the CFX run information file or a folder containing a single run information file.
#'
#' @return A dataframe containing the melt curve RFU derivative values
#'
#' @export
read_meltcurve <- function(cfx_run) {
  cfx_base <- get_cfx_base(cfx_run)
  read_delim_silent(cfx_base, " -  Melt Curve Derivative Results_SYBR.csv") %>%
    gather(Well, RFU, -Temperature) %>%
    separate(Well, c("Row", "Column"), sep = 1, convert = TRUE) %>%
    mutate(Column = parse_number(Column)) %>%
    inner_join(read_gene2well(cfx_base), by = c("Row", "Column"))
}
