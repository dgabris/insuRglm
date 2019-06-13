leverage_plots <- function(glm) {
  pdf(NULL)
  dev.control(displaylist="enable")

  plot(glm, which = 4)
  first <- recordPlot()

  plot.new()

  plot(glm, which = 5)
  second <- recordPlot()

  dev.off()

  list(first, second)
}
