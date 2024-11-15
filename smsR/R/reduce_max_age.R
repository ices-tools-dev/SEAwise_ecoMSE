#' Make the plus group smaller than in the data
#'
#' @param dat data frame containing stock assesssment input
#' @param maxage new plus group age
#'
#' @return
#' returns the same data frame
#' @export
#'
#' @examples reduce_max_age(dat, maxage = 7)
#'
reduce_max_age <- function(dat, maxage){

  # Tage the ave

  drop_matrix <- function(x, fn = 'mean'){

    if((maxage+1) > ncol(x)){
      stop('Check if maxage is larger than input files')
    }

    if(fn == 'mean'){
    x[maxage+1,,] <- apply(x[(maxage+1):nrow(x),,, drop = FALSE], c(2,3), FUN = mean)
    }

    if(fn == 'sum'){
      x[maxage+1,,] <- apply(x[(maxage+1):nrow(x),,,drop = FALSE], c(2,3), FUN = sum)
    }

    x <- x[1:(maxage +1 ),, ,drop = FALSE]

  }



  dat$mtrx$weca <- drop_matrix(dat$mtrx$weca)
  dat$mtrx$west <- drop_matrix(dat$mtrx$west)
  dat$mtrx$mat <- drop_matrix(dat$mtrx$mat)
  dat$mtrx$M <- drop_matrix(dat$mtrx$M)
  dat$mtrx$propF <- drop_matrix(dat$mtrx$propF)
  dat$mtrx$propM <- drop_matrix(dat$mtrx$propM)
  dat$Catchobs <- drop_matrix(dat$Catchobs, fn = 'sum')
  dat$Surveyobs <- dat$Surveyobs[1:(maxage + 1),,,drop = FALSE]


return(dat)
}
