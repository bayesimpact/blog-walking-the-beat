#ggplot helper functions

require(ggplot2)

#Uses a frequency table to build a bar chart that is ordered by the counts
ggplot.freqtable.1d = function(x) {
  freq = data.frame(x); colnames(freq)[1] = "label"
  freq.filtered = droplevels(subset(freq, label != "" & !is.na(label)))
  freq.filtered$label = factor(freq.filtered$label,
                               levels = as.character(freq.filtered[order(freq.filtered$Freq, decreasing = F),]$label),
                               ordered = F)
  ggplot(freq.filtered,
         aes(x = label, y = Freq, fill = label)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_grey(base_size = 25)
}

ggplot.corplot = function(x, dot = .5) {
  x.melt = melt(x)
  ggplot(x.melt,
         aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = value), colour = "white") + 
    geom_point(data = subset(x.melt, value >= dot), aes(x = Var1, y = Var2), color = 'red') +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(axis.ticks = element_blank(), axis.text.x = element_text(angle = 330, hjust = 0, colour = "grey50"))
}

#adjust x axis labels when using facet wrap
facetAdjust <- function(x, pos = c("up", "down"))
{
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}
