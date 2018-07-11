#' daily accumulated precipitation (in millimeter) at London, UK and 24-hours-ahead forecasts of the ECMWF (1969-2011)
#'
#' 24-hour ahead forecasts of daily accumulated precipitation at London,
#' UK from the high-resolution model of the
#' European Centre for Medium-Range Weather Forecasts (ECMWF). The data contains observations from 2012 to 2016.
#'
#' We thank the ECMWF for their support. Further details can be found on \url{https://www.ecmwf.int/}.
#'
#' @format A data frame with 2192 rows and 2 variables:
#' \itemize{
#'   \item Y: daily accumulated precipitation in millimeter at London, UK (0 -- 45)
#'   \item X: according point forecast in millimeter issued one day ahead (0 -- 40)
#' }
#'
#'
"precipitation"

#' real GDP realized values and one quarter ahead Greenbook forecasts (1969-2012)
#'
#' A dataset containing real GDP growth rate in the United States and according one quarter ahead point forecasts from Federal Reserve's Greenbook.
#' The forecasts were selected to be closest to the middle of the respective quarter among the published Greenbook forecasts of one quarter.
#'
#' @format A data frame with 176 rows and 2 variables:
#' \itemize{
#'   \item observation: realized GDP growth rate in percentage measured at second vintage (-4.7 -- 8.5)
#'   \item forecast: according point forecast issued one quarter before (-10.4 -- 11.2)
#' }
#'
#' @source \url{https://www.philadelphiafed.org/research-and-data/real-time-center/greenbook-data/philadelphia-data-set}
#'
"GDP"
