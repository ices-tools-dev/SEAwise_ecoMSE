#' Take a dataframe of surveys and converts them to matrix for smsR
#'
#' @param survey data frame containing survey
#' @param year years  to plot
#' @param season which seasons  are modeled
#'
#' @return
#' returns a matrix of observed surveys
#' @export
#'
#' @examples
#' survey_to_matrix(x, y)
survey_to_matrix <- function(survey, year) {
  ages <- unique(survey$Age)
  surveys <- unique(survey$Survey)


  smx <- array(-1,
    dim = c(length(ages), length(year), length(surveys)),
    dimnames = list(
      "age" = ages,
      "year" = year,
      "survey" = surveys
    )
  )

  # survey <- survey[-is.na(survey$cpue),]


  for (i in 1:length(year)) {
    for (j in 1:length(ages)) {
      for (l in 1:length(surveys)) {
        eff <- survey[survey$year == year[i] & survey$Age == ages[j] &
          survey$Survey == surveys[l], ]$eff

        tmp <- survey[survey$year == year[i] & survey$Age == ages[j] &
          survey$Survey == surveys[l], ]$cpue


        if (length(tmp) > 0) {
          ss <- tmp / eff


          if (ss > 0) {
            smx[j, i, l] <- ss
          }
        }
      }
    }
  }



  return(smx)
}
