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
#'postscript(file="figures/fig1.eps", width=8, height=8, family="Garamond", 
#'           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[1]]$Year,list(ldf[[1]]$TFR), ylim = c(0,8))
axis(1, at = ldf[[1]]$Year, cex.axis = 1.2)
axis(2, at = 0:8, las = 2, cex.axis = 1.2)
lapply(seq(0,8,2), function (x){
  lines(range(ldf[[1]]$Year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[1]]$Year,ldf[[1]]$TFR)
mtext("Total Fertility Rate", side = 2, line = 2.5, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
#'dev.off()

#' Figure 2
###############################################################################
#'postscript(file="figures/fig2.eps", width=8, height=8, family="Garamond", 
#'           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[2]]$x,list(ldf[[2]]$IMR,ldf[[2]]$U5MR), ylim = c(0, 250))
axis(1, at = ldf[[2]]$x, labels= ldf[[2]]$Year, cex.axis = 1.2)
axis(2, at = seq(0, 250, 50), las = 2, cex.axis = 1.2)
lapply(seq(0, 250, 50), function (x){
  lines(range(ldf[[1]]$Year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[2]]$x,ldf[[2]]$IMR)
FunLine(ldf[[2]]$x,ldf[[2]]$U5MR, lty = 5)
mtext("Mortality Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
legend(2002.5, 250, c("Infant M.R.", "Under 5 M.R."), lty = c(1,5), 
       cex=1.2, lwd = 2, bty = "n",       y.intersp = 2)
legend(2002.5, 250, c("", ""), lty = c(0,0), 
       cex=1.2, pt.cex = 1,  bty = "n", pch = 19,
       y.intersp = 2)
#'dev.off()

#' Figure 3
###############################################################################
#'postscript(file="figures/fig3.eps", width=8, height=8, family="Garamond", 
#'           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[3]]$x,list(ldf[[3]]$mmr), ylim = c(0,1500))
axis(1, at = ldf[[1]]$x, cex.axis = 1.2)
axis(2, at = seq(0, 1500, 300), las = 2, cex.axis = 1.2)
lapply(seq(0, 1500, 300), function (X){
  lines(range(ldf[[3]]$x), c(X,X),
        lty = 2, col = "gray")
})
FunLine(ldf[[3]]$x,ldf[[3]]$mmr)
mtext("Maternal Mortality Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
#'dev.off()

#' Figure 4
###############################################################################
#'postscript(file="figures/fig4.eps", width=8, height=8, family="Garamond", 
#'           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[4]]$x,list(ldf[[4]]$pop.growth.rate.new), ylim = c(0, 3.50))
axis(1, at = ldf[[2]]$x, labels= ldf[[2]]$Year, cex.axis = 1.2)
axis(2, at = seq(0, 3.5, 0.5), las = 2, cex.axis = 1.2)
lapply(seq(0, 3.5, 0.5), function (x){
  lines(range(ldf[[4]]$x), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[4]]$x,ldf[[4]]$pop.growth.rate.new)

mtext("Population Growth Rate", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
#'dev.off()

#' Figure 5
###############################################################################
#postscript(file="figures/fig5.eps", width=8, height=8, family="Garamond", 
#           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[5]]$year,list(ldf[[5]]$all.women,ldf[[5]]$ever.married.women), ylim = c(0, 40))
axis(1, at = ldf[[5]]$year, labels= ldf[[5]]$year, cex.axis = 1.2)
axis(2, at = seq(0, 40, 5), las = 2, cex.axis = 1.2)
lapply(seq(0, 40, 10), function (x){
  lines(range(ldf[[5]]$year), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[5]]$year,ldf[[5]]$all.women)
FunLine(ldf[[5]]$year,ldf[[5]]$ever.married.women, lty = 5)
mtext("Use of Family Planning Methods", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
legend(1985, 40, c("Ever married women", "All women"), lty = c(5,1), 
       cex=1.2, lwd = 2, bty = "n",       y.intersp = 2)
legend(1985, 40, c("", ""), lty = c(0,0), 
       cex=1.2, pt.cex = 1,  bty = "n", pch = 19,
       y.intersp = 2)
#dev.off()

#' Figure 6
###############################################################################
#postscript(file="figures/fig6.eps", width=8, height=8, family="Garamond", 
#           onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[6]]$x,list(ldf[[6]]$Male,ldf[[6]]$Female), ylim = c(0, 100))
axis(1, at = ldf[[6]]$x, labels= ldf[[6]]$Year, cex.axis = 1.2)
axis(2, at = seq(0, 100, 10), las = 2, cex.axis = 1.2)
lapply(seq(0, 100, 20), function (x){
  lines(range(ldf[[6]]$x), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[6]]$x,ldf[[6]]$Male)
FunLine(ldf[[6]]$x,ldf[[6]]$Female, lty = 5)
mtext("Primary Enrolment Ratio", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
legend(1985, 100, c("Male", "Female"), lty = c(1,5), 
       cex=1.2, lwd = 2, bty = "n",       y.intersp = 2)
legend(1985, 100, c("", ""), lty = c(0,0), 
       cex=1.2, pt.cex = 1,  bty = "n", pch = 19,
       y.intersp = 2)
#dev.off()

#' Figure 7
###############################################################################
#postscript(file="figures/fig7.eps", width=8, height=8, family="Garamond", 
#          onefile=FALSE, horizontal=FALSE,paper = "special")
FunPlot(ldf[[7]]$x,list(ldf[[7]]$Male,ldf[[7]]$Female), ylim = c(0, 60))
axis(1, at = ldf[[7]]$x, labels= ldf[[7]]$Year, cex.axis = 1.2)
axis(2, at = seq(0, 60, 10), las = 2, cex.axis = 1.2)
lapply(seq(0, 60, 10), function (x){
  lines(range(ldf[[7]]$x), c(x,x),
        lty = 2, col = "gray")
})
FunLine(ldf[[7]]$x,ldf[[7]]$Male)
FunLine(ldf[[7]]$x,ldf[[7]]$Female, lty = 5)
mtext("Secondary Gross Enrolment", side = 2, line = 3, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
legend(1980, 60, c("Male", "Female"), lty = c(1,5), 
       cex=1.2, lwd = 2, bty = "n",       y.intersp = 2)
legend(1980, 60, c("", ""), lty = c(0,0), 
       cex=1.2, pt.cex = 1,  bty = "n", pch = 19,
       y.intersp = 2)
#dev.off()
