#' Two level function of a combined dataframe
#'
#' @param x containing the first three numeric elements of this sequence
#' @param n denotes the final nth element of the sequence to calculate
#'
#' @return a combined function of x and n
#' @export myseq_n
#'
#' @examples
#' myseq_n(x = c(2,3,3), n = 3)
#' myseq_n(x = c(2,4,3), n = 4)
#' myseq_n(x = c(2, 4, 3), n = 5)
#' myseq_n(x = c(2, 4, 3), n = 6)
#' myseq_n(x = c(2, 4, 3), n = 7)
#'

myseq_n = function(x, n) {
  stopifnot(length(x) == 3)
  stopifnot(length(n) == 1)
  if (n <= 0) {
    stop("to proceed, n must be greater than 0")
  } else if (n >= 1 & n <= 3) {

    return(x[n])
  }
  else {
    for (i in 4:n) {
      x[i] = x[i-1] + (x[i-3] - x[i-2])/i
    }

  }
  return(x[n])
}






