myautocorr<-
    function (xdis, geodis, zdis = NULL, method = "pearson", alfa = 0.05, 
          nclass = NULL, breaks = NULL, permutations = 999, strata, 
          simil = FALSE, plot = TRUE, print = TRUE) 
{
    require(vegan)
    xdis <- as.dist(xdis)
    ydis <- as.dist(geodis)
    if (!is.null(zdis)) 
        zdis <- as.dist(zdis)
    if (is.null(breaks)) {
        if (!is.null(nclass)) {
            nclas <- nclass
        }
        else {
            m <- length(ydis)
            nclas <- round(1 + 3.3 * log10(m))
        }
        cmin <- range(ydis)[1]
        cmax <- range(ydis)[2]
        breaks <- seq(cmin, cmax, le = nclas + 1)
        breaks[nclas + 1] <- breaks[nclas + 1] + 1
    }
    pepe <- function(x) ifelse(x[1] >= 0, length(which(x >= x[1]))/length(x), 
                               length(which(x <= x[1]))/length(x))
    ydis <- as.matrix(ydis)
    geoclas <- NULL
    mantelr <- NULL
    estadistico <- NULL
    significatividad <- NULL
    pvalues <- NULL
    clases <- NULL
    if (!is.null(zdis)) {
        cat("evaluating distance class ")
        for (i in 1:nclas) {
            cat(paste(i, ", ", sep = ""))
            geoclas[[i]] <- as.dist((ydis >= breaks[i] & ydis < 
                breaks[i + 1]) * 1)
            mantelr[[i]] <- mantel.partial(xdis, geoclas[[i]], 
                                           zdis, method, permutations, strata)
            estadistico <- c(estadistico, ifelse(simil == FALSE, 
                                                 mantelr[[i]]$statistic * -1, mantelr[[i]]$statistic))
            significatividad <- c(significatividad, mantelr[[i]]$signif)
            pvalues <- c(pvalues, pepe(c(mantelr[[i]]$statistic, 
                                         mantelr[[i]]$perm)))
            clases <- c(clases, paste(signif(breaks[i], 3), " - ", 
                                      signif(breaks[i + 1], 3)))
        }
        cat("\n")
    }
    else {
        cat("evaluating distance class ")
        for (i in 1:nclas) {
            cat(paste(i, ", ", sep = ""))
            geoclas[[i]] <- as.dist((ydis >= breaks[i] & ydis < 
                breaks[i + 1]) * 1)
            mantelr[[i]] <- mantel(xdis, geoclas[[i]], method, 
                                   permutations, strata)
            estadistico <- c(estadistico, ifelse(simil == FALSE, 
                                                 mantelr[[i]]$statistic * -1, mantelr[[i]]$statistic))
            significatividad <- c(significatividad, mantelr[[i]]$signif)
            pvalues <- c(pvalues, pepe(c(mantelr[[i]]$statistic, 
                                         mantelr[[i]]$perm)))
            clases <- c(clases, paste(round(breaks[i], 3), " - ", 
                                      round(breaks[i + 1], 3)))
        }
        cat("\n")
    }
    pvalues.adj <- pvalues * (1:length(pvalues))
    if (plot == TRUE) {
        colores <- as.numeric(pvalues.adj < alfa)
        plot(1:nclas, estadistico, cex = 1.5, pch = 22 - colores * 
            7, type = "b", ylab = expression(r[M]), xlab = "Distance classes", 
             xlim = c(0, nclas + 1), ylim = c(-0.1, 0.1))
    }
    if (print == TRUE) {
        print(data.frame(class = 1:nclas, distance.range = clases, 
                         rM = estadistico, p = pvalues, p.Bonferroni = pvalues.adj))
    }
    result <- list(breaks = breaks, rM = estadistico, signif = significatividad, 
                   pvalues = pvalues, pval.Bonferroni = pvalues.adj, clases = clases)
    class(result) <- c("mpmcorrelogram", class(result))
    return(result)
}