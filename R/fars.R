#' Reader Fars data
#' 
#' This is a function to read the data given a filename and return a datatable
#' containing it. If the filename does not exist gives an error.
#'
#' @param filename A character string giving the path of the file to read
#'
#' @returns This function returns the data table for the data read
#'
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{data <- fars_read("accident_2013.csv")}
#' \dontrun{data <- fars_read(make_filename(2013))}
#' 
#' @export
fars_read <- function(filename) {
  
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Write filename
#' 
#' This function generates the filename given a specific year.
#'
#' @param year An integer to specify the year
#'
#' @returns A character filename for the specified year
#'
#' @examples
#' \dontrun{year2013 <- make_filename(2013)}
#' \dontrun{year2014 <- make_filename(2014)}
#' 
#' \export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Multiple year reader
#' 
#' This function reads the files of the specified years and returns an error if 
#' the year file does not exist.
#'
#' @param years A list of years to read
#'
#' @returns List of tables with the data read for each specified year
#' 
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{data <- fars_read_years(c(2013, 2014))}
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

#' Summarise years
#' 
#' This function creates an overview of the different years specified.
#'
#' @param years A list of years to summarise
#'
#' @returns A summary for the different specified years
#' 
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{data <-  fars_summarize_years(c(2013, 2014))}
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map state
#' 
#' This function maps the state and pinpoints the accident in the map of the
#' state and year specified. Returns and error if the state number is invalid
#' and a message if there are no accidents.
#'
#' @param state.num An integer of the state number to map
#' @param year An integer of the year to map
#'
#' @returns A plot of the given state and the points of accidents for the given 
#' year
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
