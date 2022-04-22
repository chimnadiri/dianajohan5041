
#' @title 2 sample test
#' This is my constructor function. This function will create a list. The constructor runs the hypotheses on two samples, and creates and object with the confidence interval and p statistics
#'
#' @param x sample 1 data
#' @param y sample 2 data
#' @param alpha significance level
#'
#' @return list of data, confidence interval and p value
#' @export
#'
#' @examples
#' \dontrun{obj = myconstr(x = x, y = y, alpha = 0.05)}
myconstr = function(x, y, alpha){

  test = stats::t.test(x, y, mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = 1-alpha)
  l = max(length(x), length(y))
  x = c(x, rep(NA, l - length(x)))
  y = c(y, rep(NA, l - length(y)))
  p.val = test$p.value
  conf.int = test$conf.int
  #obj = list(data = list(x = x, y = y), conf.int = test$conf.int, p.val = test$p.value)
  obj = list(data = data.frame(x, y), conf.int = conf.int, p.val = p.val)
  class(obj) = "Rttest"
  obj
}


#' Prints out two samples, confidence interval and significance level to a table.
#'
#' @param x Rttest object
#' @importFrom kableExtra kable_styling kable
#' @return a table with two samples, confidence interval and significance level
#' @export
#'
#' @examples
#' \dontrun{print(obj)}
print.Rttest = function(x)
{

  options(knitr.kable.NA = '')
  data = x[["data"]]
  l = length(data$x)

  p.val = x[["p.val"]]
  p.val = c(p.val, rep(NA, l-1))
  conf.int = x[["conf.int"]]
  conf.int = c(conf.int, rep(NA, l - 2))


  df = data.frame(data, conf.int,p.val)


  kableExtra::kable_styling(kableExtra::kable(df, align = "c"))

}

