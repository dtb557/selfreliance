require(car)

modified_spreadLevelPlot <- function(x, robust.line = TRUE, xlab = "Fitted Values", 
                                     ylab = "Absolute Studentized Residuals", 
                                     las = par("las"), main = paste("Spread-Level Plot for\n", 
                                                                    deparse(substitute(x))), pch = 1, col = palette()[1], 
                                     col.lines = palette()[2], lwd = 2, grid = TRUE, ...) {
    resid <- abs(rstudent(x))
    good <- !is.na(resid) & resid > 0
    resid <- resid[good]
    fitval <- fitted.values(x)[good]
    non.pos <- fitval <= 0
    if (any(non.pos)) {
        fitval <- fitval[!non.pos]
        resid <- resid[!non.pos]
        n.non.pos <- sum(non.pos)
        warning(n.non.pos, " negative", if (n.non.pos > 1) 
            " fitted values"
            else " fitted value", " removed")
    }
    min <- min(fitval)
    plot(fitval, resid, log = "xy", main = main, xlab = xlab, 
         ylab = ylab, las = las, col = col, pch = pch, type = "n", 
         ...)
    if (grid) {
        grid(lty = 1, equilogs = FALSE)
        box()
    }
    points(fitval, resid, col = col, pch = pch)
    mod <- if (robust.line) 
        rlm(log(resid) ~ log(fitval))
    else lm(log(resid) ~ log(fitval), ...)
    first <- which.min(fitval)
    last <- which.max(fitval)
    lines((fitval)[c(first, last)], exp(fitted.values(mod)[c(first, 
                                                             last)]), lwd = lwd, col = col.lines, ...)
    p <- 1 - (coefficients(mod))[2]
    names(p) <- NULL
    result <- list(PowerTransformation = p)
    class(result) <- "spreadLevelPlot"
    result   
}

environment(modified_spreadLevelPlot) <- environment(spreadLevelPlot)