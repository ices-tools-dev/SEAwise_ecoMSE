#' Do a simple 1 year projection of the stock
#'
#' @param df.tmb list of sms parameters
#' @param N_current Numbers at age of the start of the forecast year
#' @param F0 Fishing mortality
#' @param M Natural mortality
#' @param weca weight at age in catch
#' @param west weight at age in stock
#' @param mat Maturity ogive
#'
#' @return
#' returns a list of derived values from the projection
#' @export
#'
#' @examples
#' # Not run
#' simple_projection(df.tmb, N_current, F0, M, weca, mat)
#'
simple_projection <- function(df.tmb,
                              N_current,
                              F0,
                              M,
                              weca,
                              mat,
                              ag_years) {
  M <- matrix(rowMeans(df.tmb$M[, (df.tmb$nyears - avg_years[1] + 1):df.tmb$nyears, ]), nrow = df.tmb$nage, ncol = df.tmb$nseason)
  mat <- matrix(rowMeans(df.tmb$Mat[, (df.tmb$nyears - avg_years[2] + 1):df.tmb$nyears, ]), nrow = df.tmb$nage, ncol = df.tmb$nseason)
  weca <- matrix(rowMeans(df.tmb$weca[, (df.tmb$nyears - avg_years[3] + 1):df.tmb$nyears, ]), nrow = df.tmb$nage, ncol = df.tmb$nseason)
  west <- matrix(rowMeans(df.tmb$weca[, (df.tmb$nyears - avg_years[4] + 1):df.tmb$nyears, ]), nrow = df.tmb$nage, ncol = df.tmb$nseason)


  N_new <- matrix(NA, df.tmb$nage, df.tmb$nseason)
  N_future <- matrix(NA, df.tmb$nage) # For the following year SSB
  C_new <- matrix(NA, df.tmb$nage, df.tmb$nseason)

  Z <- F0 + M
  N_new[, 1] <- N_current


  for (qrts in 1:df.tmb$nseason) {
    for (i in 1:df.tmb$nage) {
      C_new[i, qrts] <- (F0[i, qrts] / Z[i, qrts]) * N_new[i, qrts] * weca[i, qrts]

      if (i < df.tmb$nseason) {
        N_new[i, qrts + 1] <- N_new * exp(Z[i, qrts])
      } else {
        N_future[1] <- 0
        if (i < df.tmb$nage) {
          N_future[i + 1] <- N_new[i] * exp(Z[i, qrts])
        } else {
          N_future[i] <- N_future[i, qrts] + N_new[i, qrts] * exp(-Z[i, qrts])
        }
      }
      SSB_next <- sum(N_future * weca * mat)
    }
  }


  ls.out <- list(
    TAC = sum(C_new),
    SSB_next = SSB_next,
    N_future = N_future
  )

  return(ls.out)
}
