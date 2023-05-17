#' Yimin's hw
#' @param x A vector of numeric
#' @return log of summed exponential
#' @examples
#' library(YiminZhao)
#' test = seq(1, 2000, 1)
#' log_summed_exps(test) # expected to get 2000.459
#' @export
log_summed_exps = function(x){
  len = length(x)
  ordered_vec = x[order(x)]
  result = ordered_vec[len] + log(1 + sum(exp(ordered_vec[1:len-1] - rep(ordered_vec[len], len - 1))))
  return(result)
}
