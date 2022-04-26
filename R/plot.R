#' @title plot
#' Generates a boxplot of two samples
#'
#' @param x Rttest object
#' @param ... other
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_boxplot aes labs
#' @return boxplot of two samples
#' @export
#' @examples
#' \dontrun{plot(obj)}
#'
plot.Rttest <- function(x,...)
{

  variable = NULL
  value = NULL
  data = x[["data"]]
  df = data.frame(melt(data, id.vars = NULL))
  ggplot(data = df, aes(x = variable, y = value )) +
    geom_boxplot() +
    labs(title="Boxplot of two samples",
         x ="samples", y = "data")

}

