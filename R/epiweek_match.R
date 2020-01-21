#' Calendar variable management
#' 
#' Functions and objects to manage calendar-related variables (e.g., 
#' week matching, flu season labeling).
#' 
#' Epiweeks are the same as MMWR Weeks: 
#'    https://wwwn.cdc.gov/nndss/document/MMWR_Week_overview.pdf

#' @param epiweek Numeric. An epiweek: [1, 53]
#' @describeIn calendar_mgmt Assign an integer label based on epiweek.
#' @export
assign_weekint <- function(epiweek) {
  ((epiweek >= 40) * (epiweek - 40)) + ((epiweek <= 39) * (epiweek + 14))
}

#' @describeIn calendar_mgmt Return a vector of season labels. Standardizes the 
#' assignment of flu season labels (e.g., 2008-09).
#' @export

seas_levels <- function() {
  paste(2003:2018, str_extract(2004:2019, "[0-9]{2}$"), sep = "-")
}