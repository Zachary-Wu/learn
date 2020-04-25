#' sign_level
#'
#' @param data
#' @param sig.level
#' @param mark
#'
#' @return
#' @export
#'
#' @examples
sign_level  <- function(data,
                      sig.level = c(0.001, 0.01, 0.05, 0.1),
                      mark = c("***", "**", "*", ".")) {
  p <- subset(data, select = p, drop = T)
  if(!is.numeric(p))
    p <- as.numeric(p)
  ord <- order(sig.level)
  sig.level <- sig.level[ord]
  mark <- mark[ord]
  brks <- c(0, sig.level, 1)
  lbs <- c(mark, "")
  sign <- cut(p, breaks = brks, labels = lbs,
              include.lowest = FALSE, right = TRUE)
  ifelse(p == 0, mark[1], as.character(sign ))
  data <- cbind(data, sign)
}
