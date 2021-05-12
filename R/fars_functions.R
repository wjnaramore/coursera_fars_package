#' fars_read
#'
# Uploads data fatal analysis reporting system (fars) data from a csv file into
#' tbl_df object. If the filename does not exist, error message will be displayed.
#' Data should be sourced from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System, which is a nationwide census providing
#' the American public yearly data regarding fatal injuries suffered in motor
#' vehicle traffic crashes.
#' @source \url{http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' Functions from other packages that will be needed are: readr::read_csv and dplyr::tbl_df
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename the name of the file which the data are to be read from.
#' @param package_file if TRUE, this function looks for files in the package
#' directory inst/extdata. Otherwise, the file needs to be in the working directory.
#'
#' @return This function returns a tbl_df object
#'
#' @examples
#' fars_read(make_filename(2013))
#'
#' @export
fars_read <- function(filename,package_file = TRUE) {
  if(package_file){
    filename = system.file("extdata", filename, package = "fars")
  }
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#'
#' Given the year input, this function creates a filename consistent with the
#' naming convention used for fars csv files from US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System
#'
#' @param year the year of interest with 4 digits
#'
#' @return This function returns a character object of the full filename string
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' Given a year or vector of years, this function returns a list of tibbles with
#' the MONTH and year columns of the fars data. If the input includes an invalid
#' year, that particular tibble will be \code{NULL} and a warning message will be
#' displayed.
#' Functions from other packages that will be needed are: dplyr::mutate and dplyr::select
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @param years the years of interest with 4 digits. This can be a vector
#' of years including the
#'
#' @return This function returns a list of tibbles, one tibble per year. The tibble
#' includes the MONTH and year columns of the fars data
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' Given a year or vector of years, this function returns a tibble with the number
#' of incidents broken down by month and year. If the input includes an invalid
#' year, an error message will occur.
#' Functions from other packages that will be needed are: dplyr::bind_rows,
#' dplyr::group_by, dplyr::summarize, and tidyr::spread
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @param years the years of interest with 4 digits. This can be a vector
#'
#' @return This function returns a tibble with the number of incidents broken
#'  down by month (rows) and year (column(s)).
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' Given a state number and year, this function returns a plots the incidents
#' on a map of the state by latitude and longitude coordinates. If the input
#' includes an invalid state number or year, an error message will occur. If
#' there are are no incidents to plot, a message will be displayed.
#' Functions from other packages that will be needed are: dplyr::filter,
#' maps::map, and graphics::points
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num state number, using fars state numbering convention
#' @param year the years of interest with 4 digits.
#'
#' @return This function returns a tibble with the number of incidents broken
#'  down by month (rows) and year (column(s)).
#'
#' @examples
#' fars_map_state(1,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
