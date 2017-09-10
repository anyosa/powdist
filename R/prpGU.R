#' @name ReversalPowerGumbel
#' @examples
#' prpGU(1, 1, 3, 4)
#' @export
prpGU <- function(q, lambda = 1, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE){
  p = 1 - (pGU((-q+mu)/sigma)**lambda)
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}
