require(car)

modified_qqPlot <- function (x, xlab = paste(distribution, "Quantiles"), ylab = paste("Studentized Residuals(", 
                                                                   deparse(substitute(x)), ")", sep = ""), 
          main = NULL, distribution = c("t", "norm"), line = c("robust", "quartiles", "none"), las = par("las"), 
          simulate = TRUE, envelope = 0.95, reps = 100, col = palette()[1], 
          col.lines = palette()[2], lwd = 2, pch = 1, cex = par("cex"), 
          labels, id.method = "y", id.n = if (id.method[1] == "identify") Inf else 0, 
          id.cex = 1, id.col = palette()[1], grid = TRUE, ...) 
{
    result <- NULL
    distribution <- match.arg(distribution)
    line <- match.arg(line)
    rstudent <- rstudent(x)
    if (missing(labels)) 
        labels <- names(rstudent)
    good <- !is.na(rstudent)
    rstudent <- rstudent[good]
    labels <- labels[good]
    sumry <- summary.lm(x)
    res.df <- sumry$df[2]
    if (!simulate) 
        result <- qqPlot(rstudent, distribution = if (distribution == 
                                                          "t") 
            "t"
            else "norm", df = res.df - 1, line = line, main = main, 
            xlab = xlab, ylab = ylab, las = las, envelope = envelope, 
            col = col, col.lines = col.lines, lwd = lwd, pch = pch, 
            cex = cex, labels = labels, id.method = id.method, 
            id.n = id.n, id.cex = id.cex, id.col = id.col, ...)
    else {
        n <- length(rstudent)
        ord <- order(rstudent)
        ord.x <- rstudent[ord]
        ord.lab <- labels[ord]
        P <- ppoints(n)
        z <- if (distribution == "t") 
            qt(P, df = res.df - 1)
        else qnorm(P)
        plot(z, ord.x, type = "n", xlab = xlab, ylab = ylab, 
             main = main, las = las)
        if (grid) 
            grid(lty = 1, equilogs = FALSE)
        points(z, ord.x, pch = pch, col = col, cex = cex)
        yhat <- fitted.values(x)[good]
        S <- sumry$sigma
        Y <- matrix(yhat, n, reps) + matrix(rnorm(n * reps, sd = S), 
                                            n, reps)
        X <- model.matrix(x)[good, ]
        rstud <- apply(rstudent(lm(Y ~ X - 1)), 2, sort, na.last=TRUE)
        lower <- apply(rstud, 1, quantile, prob = (1 - envelope)/2, na.rm=TRUE)
        upper <- apply(rstud, 1, quantile, prob = (1 + envelope)/2, na.rm=TRUE)
        lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
        lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
        if (line == "quartiles") {
            Q.x <- quantile(rstudent, c(0.25, 0.75))
            Q.z <- if (distribution == "t") 
                qt(c(0.25, 0.75), df = res.df - 1)
            else qnorm(c(0.25, 0.75))
            b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
            a <- Q.x[1] - b * Q.z[1]
            abline(a, b, col = col.lines, lwd = lwd)
        }
        if (line == "robust") {
            coef <- coefficients(rlm(ord.x ~ z))
            a <- coef[1]
            b <- coef[2]
            abline(a, b, col = col.lines, lwd = lwd)
        }
        result <- showLabels(z, ord.x, labels = ord.lab, id.method = id.method, 
                             id.n = id.n, id.cex = id.cex, id.col = id.col)
    }
    if (length(result) == 0) 
        invisible(result)
    else if (is.numeric(result)) 
        sort(result)
    else result
}

environment(modified_qqPlot) <- environment(qqPlot)