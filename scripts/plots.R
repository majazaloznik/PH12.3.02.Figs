###############################################################################
#' Nice versions of figures for PopHzn 12(3) 
#' Asefa Hailemariam: The 1993 Population Policy of Ethiopia: how well did it work?
###############################################################################
#' data copy pasted from submitted word document. double checked
#' sources were not checked but are given nonetheless
###############################################################################


#' 00. Preliminaries
###############################################################################
require(dplyr)
require(tidyr)
library(extrafont)
loadfonts()

#' load data
filenames <- list.files("data", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, function(x) read.csv(x, stringsAsFactors=FALSE))

#' clean data funciton
FunCleanYears <- function(XX) {
  XX %>%
    separate(Year, c("x.min", "x.dmax"), remove = FALSE) %>%
    separate(x.min, c("mm",  "dd"), sep = 2, remove = FALSE) %>%
    mutate(mm = ifelse(x.dmax == "00", 20, mm )) %>%
    unite(x.max, mm, x.dmax, sep="") %>%
    select(-dd) %>%
    mutate(x = (as.numeric(x.min) + as.numeric(x.max))/2) -> XX
  XX
}

#' get mid period values for the 4 tables that have period data
ldf[[2]] <- FunCleanYears(ldf[[2]])
ldf[[4]] <- FunCleanYears(ldf[[4]])
ldf[[6]] <- FunCleanYears(ldf[[6]])
ldf[[7]] <- FunCleanYears(ldf[[7]])

#' 01. Fig 01 - Total Fertility Rate
#' Source: CSA, 1993, 2014; CSA and ORC Macro International, 2001, 2006; 
#' CSA and ICF International 2012; UN (2015)

#' Basic plot function 
###############################################################################
FunPlot <- function(x, y, ylim){
  par(mar=c(4.1, 4.6, 2.1, 2.1))
  plot(mean(x), mean(unlist(y)),
       type = "n",
       axes = FALSE, 
       ylab = "",
       xlab = "", 
       xlim = range(x), 
       ylim = ylim)
}

FunLine <- function(x, y, lty = 1){
    lines(x, y, lwd = 2, lty = lty)
    points(x, y, pch = 19)
}
#' Figure 1
###############################################################################
postscript(file="figures/fig1.eps", width=8, height=8, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[1]]$Year,list(ldf[[1]]$TFR), ylim = c(4,8))
axis(1, at = ldf[[1]]$Year, cex.axis = 1.2)
axis(2, at = 4:8, las = 2, cex.axis = 1.2)
lapply(4:8, function (x){
  lines(range(ldf[[1]]$Year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[1]]$Year,ldf[[1]]$TFR)
mtext("Total Fertility Rate", side = 2, line = 2.5, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
dev.off()

#' Figure 2
###############################################################################
postscript(file="figures/fig2.eps", width=8, height=8, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[2]]$x,list(ldf[[2]]$IMR,ldf[[2]]$U5MR), ylim = c(50, 250))
axis(1, at = ldf[[2]]$x, labels= ldf[[2]]$Year, cex.axis = 1.2)
axis(2, at = seq(50, 250, 50), las = 2, cex.axis = 1.2)
lapply(seq(50, 250, 50), function (x){
  lines(range(ldf[[1]]$Year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[2]]$x,ldf[[2]]$IMR)
FunLine(ldf[[2]]$x,ldf[[2]]$U5MR, lty = 5)
mtext("Mortality Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
dev.off()

#' Figure 3
###############################################################################
postscript(file="figures/fig3.eps", width=8, height=8, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[3]]$x,list(ldf[[3]]$mmr), ylim = c(300,1500))
axis(1, at = ldf[[1]]$x, cex.axis = 1.2)
axis(2, at = seq(300, 1500, 300), las = 2, cex.axis = 1.2)
lapply(seq(300, 1500, 300), function (X){
  lines(range(ldf[[3]]$x), c(X,X),
        lty = 2, col = "gray")
})
FunLine(ldf[[3]]$x,ldf[[3]]$mmr)
mtext("Maternal Mortality Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
dev.off()

#' Figure 4
###############################################################################
postscript(file="figures/fig4.eps", width=8, height=8, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[4]]$x,list(ldf[[4]]$pop.growth.rate), ylim = c(1.25, 3.25))
axis(1, at = ldf[[2]]$x, labels= ldf[[2]]$Year, cex.axis = 1.2)
axis(2, at = seq(1.25, 3.25, 0.5), las = 2, cex.axis = 1.2)
lapply(seq(1.25, 3.25, 0.5), function (x){
  lines(range(ldf[[1]]$Year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[4]]$x,ldf[[4]]$pop.growth.rate)

mtext("Mortality Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
dev.off()
