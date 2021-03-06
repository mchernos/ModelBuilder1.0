denscomp1 = function (ft, breaks = 10, xlim, ylim, probability = TRUE, main, xlab, ylab, 
          datapch, datacol, fitlty, fitcol, addlegend = TRUE, legendtext, 
          xlegend = "topright", ylegend = NULL, demp = FALSE, dempcol = "grey", 
          ...) 
{
  if (inherits(ft, "fitdist")) {
    ft <- list(ft)
  }
  else if (!is.list(ft)) {
    stop("argument ft must be a list of 'fitdist' objects")
  }
  else {
    if (any(sapply(ft, function(x) !inherits(x, "fitdist")))) 
      stop("argument ft must be a list of 'fitdist' objects")
  }
  if (!is.null(ft[[1]]$weights)) 
    stop("denscomp is not yet available when using weights")
  nft <- length(ft)
  if (missing(datapch)) 
    datapch <- 16
  if (missing(datacol)) 
    datacol <- NULL
  if (missing(fitcol)) 
    fitcol <- 2:(nft + 1)
  if (missing(fitlty)) 
    fitlty <- 1:nft
  fitcol <- rep(fitcol, length.out = nft)
  fitlty <- rep(fitlty, length.out = nft)
  if (missing(xlab)) 
    xlab <- "data"
  if (missing(ylab)) 
    ylab <- ifelse(probability, "Density", "Frequency")
  if (missing(main)) 
    main <- ifelse(probability, "Histogram and theoretical densities", 
                   "Histogram and theoretical frequencies")
  mydata <- ft[[1]]$data
  if (missing(xlim)) {
    xmin <- min(mydata)
    xmax <- max(mydata)
    xlim <- range(mydata)
  }
  else {
    xmin <- xlim[1]
    xmax <- xlim[2]
  }
  verif.ftidata <- function(fti) {
    if (any(fti$data != mydata)) 
      stop("All compared fits must have been obtained with the same dataset")
    invisible()
  }
  lapply(ft, verif.ftidata)
  n <- length(mydata)
  sfin <- seq(xmin, xmax, length.out = 101)
  reshist <- hist(mydata, plot = FALSE)
  scalefactor <- ifelse(probability, 1, n * diff(reshist$breaks))
  comput.fti <- function(i, ...) {
    fti <- ft[[i]]
    para <- c(as.list(fti$estimate), as.list(fti$fix.arg))
    distname <- fti$distname
    ddistname <- paste("d", distname, sep = "")
    do.call(ddistname, c(list(x = sfin), as.list(para))) * 
      scalefactor
  }
  fitteddens <- sapply(1:nft, comput.fti, ...)
  if (NCOL(fitteddens) != nft || NROW(fitteddens) != length(sfin)) 
    stop("problem when computing fitted densities.")
  if (missing(ylim)) {
    if (!probability) 
      ylim <- c(0, max(reshist$counts))
    else ylim <- c(0, max(reshist$density))
    ylim <- range(ylim, fitteddens)
  }
  else ylim <- range(ylim)
  reshist <- hist(mydata, breaks = breaks, main = main, xlab = xlab, ylab = ylab, 
                  xlim = xlim, ylim = ylim, col = datacol, probability = probability, 
                  ...)
  for (i in 1:nft) lines(sfin, fitteddens[, i], lty = fitlty[i], 
                         col = fitcol[i], ...)
  if (demp) 
    lines(density(mydata)$x, density(mydata)$y * scalefactor, 
          col = dempcol)
  if (addlegend) {
    if (missing(legendtext) && !demp) {
      legendtext <- paste("fit", 1:nft)
    }
    else if (missing(legendtext) && demp) {
      legendtext <- c(paste("fit", 1:nft), "emp.")
      fitlty <- c(fitlty, 1)
      fitcol <- c(fitcol, dempcol)
    }
    else if (demp) {
      legendtext <- c(legendtext, "emp.")
      fitlty <- c(fitlty, 1)
      fitcol <- c(fitcol, dempcol)
    }
    legend(x = xlegend, y = ylegend, bty = "n", legend = legendtext, 
           lty = fitlty, col = fitcol, ...)
  }
}