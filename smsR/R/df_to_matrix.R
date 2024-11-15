#' Turn data frames into multidimensional matrices for TMB
#'
#' @param df.in data frame with info
#' @param season number of seasons to standardize over
#'
#' @return
#' returns a matrix
#' @export
#'
#' @examples
#' df_to_matrix(sandeel_1r$survey, season = 1:2)
df_to_matrix <- function(df.in, season = 1:4) {
  ages <- 0:max(df.in$Age)
  year <- unique(df.in$year)


  cmx <- array(NA, dim = c(length(ages), length(year), length(season)))



  for (i in 1:length(year)) {
    for (j in 1:length(ages)) {
      for (k in 1:length(season)) {
        tmp <- df.in[df.in$Quarter == season[k] & df.in$year == year[i] & df.in$Age == ages[j], 4]

        if (length(tmp) > 0) {
          cmx[j, i, k] <- tmp[[1]]
        }
      }
    }
  }


  return(cmx)
}
