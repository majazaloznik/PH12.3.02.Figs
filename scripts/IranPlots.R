###############################################################################
#' Nice versions of figures for PopHzn 12(3) 
#' Fertility, Marriage, and Family Planning in Iran: Implications for Future Policy
#' Meimanat Hosseini-Chavoshi 1 , Mohammad Jalal Abbasi-Shavazi 2 and Peter McDonald
###############################################################################
#' data copy pasted from submitted excel file. double checked
#' sources were not checked but are given nonetheless
###############################################################################

#' 00. Preliminaries
###############################################################################
require(dplyr)
require(tidyr)
library(extrafont)
# font_import()
(device="postscript")

#' load data
filenames <- list.files("data", pattern="(03.fig).*\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(x) read.csv(x, stringsAsFactors=FALSE))

FunPlot <- function(x, y, ylim){
  plot(mean(x), mean(unlist(y)),
       type = "n",
       axes = FALSE, 
       ylab = "",
       xlab = "", 
       xlim = range(x), 
       ylim = ylim)
}
FunLine <- function(x, y, lty = 1, lwd = 2, pch = 19, ...){
  lines(x, y, lwd = lwd, lty = lty,...)
  points(x, y, pch = pch, ...)
}

#' Figure 1
###############################################################################
postscript(file="figures/03.fig1x.eps", width=12, height=6, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

par(mar=c(4.1, 3.6, 1.1, 0.1))
FunPlot(ldf[[1]]$year, ldf[[1]]$Synthetic.PPR, c(0,7))
FunLine(ldf[[1]]$year, ldf[[1]]$Synthetic.PPR, lwd = 2, pch = 21, col = "gray 60", bg = "black",cex = 1.2)
FunLine(ldf[[1]]$year, ldf[[1]]$Real.PPR, pch = 21, col= "gray60", bg = "white", lwd = 2, cex = 1.2)
FunLine(ldf[[1]]$year, ldf[[1]]$Own.Children..DHSs, col = "black",  pch = NA,  lwd = 2)
FunLine(ldf[[1]]$year, ldf[[1]]$Own.Children..Censuses, col = "black", pch = NA, lty = 2, lwd = 2)

axis(1,cex.axis = 1.2)     
axis(2, las = 2, cex.axis = 1.2)
for (i in seq(0,7,1)){
lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}

legend(1997, 7, c("Synthetic PPR", "Real PPR", "Own children - DHSs", "Own children - censuses"), 
       cex=1.2,lty = c(1,1,1,2), lwd = 2, bty = "n",  y.intersp = 2,
       col = c("gray60", "gray60", "black", "black"))
legend(1997, 7, c("", ""), lty = c(0,0, 0, 0),
       cex=1.2,   bty = "n", pch = c(21,21, NA, NA),   y.intersp = 2, 
       col = c("gray60", "gray60", NA, NA),
       pt.bg= c("black", "white", NA, NA), pt.cex = c(1.2, 1.2, NA, NA))


mtext("Total Fertility Rate", side = 2, line = 2.5, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)
dev.off()


#' Figure 2
###############################################################################
postscript(file="figures/03.fig2x.eps", width=12, height=8, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

par(mfrow = c(2,2), xpd = TRUE)
par(mar=c(2.6, 3.6, 3.1, 1.1))

FunPlot(ldf[[2]]$X, ldf[[2]][,2], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,2], lwd = 2, pch = 21, col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,3], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(2, las = 2, cex.axis = 1.2)
mtext("From first marriage to 1st birth", side =3, line =1)

par(mar=c(2.6, 1.1, 3.1, 3.6))
FunPlot(ldf[[2]]$X, ldf[[2]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,4], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,5], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)

axis(4, las = 2, cex.axis = 1.2, labels = rep("", 6), at = seq(0,1,0.2))
mtext("From 1st birth to 2nd birth", side =3, line = 1)
mtext("Parity Progression Ratio", side =4, line =1.5)


par(mar=c(5.6, 3.6, 0.1, 1.1))
FunPlot(ldf[[2]]$X, ldf[[2]][,6], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,6], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,7], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(1,cex.axis = 1.2)     
axis(2, las = 2, cex.axis = 1.2, labels = rep("", 6), at = seq(0,1,0.2))
mtext("From 2nd birth to 3nd birth", side =3, line =1)
mtext("Year", side =1, line =2.5)
mtext("Parity Progression Ratio", side =2, line =1.5)



legend(1995, -0.25, c("2000 Iran DHS"), cex=1.2,lty = 1, lwd = 2, bty = "n",  
       col = c("gray50"))
legend(1995, -0.25, c(""), cex=1.2,lty = 0, pch = 21, bty = "n",  
       col = c("gray50"), pt.bg = "gray80", pt.cex = 1.2)

par(mar=c(5.6, 1.1,0.1, 3.6))
FunPlot(ldf[[2]]$X, ldf[[2]][,8], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,8], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,9], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(1,cex.axis = 1.2)     

axis(4, las = 2, cex.axis = 1.2)
axis(1,cex.axis = 1.2, at = seq(1980, 2010, 5), labels = rep("",7))     
mtext("From 3rd birth to 4th birth", side =3, line =1)
mtext("Year", side =1, line =2.5)


legend(1980, -0.25, c("2000 Iran MiDHS"), cex=1.2,lty = 1, lwd = 2, bty = "n",  
       col = c("gray1"))
legend(1980, -0.25, c(""), cex=1.2,lty = 0, pch = 21, bty = "n",  
       col = c("gray1"), pt.bg = "gray60", pt.cex = 1.2)
dev.off()

#' Figure 1
###############################################################################
postscript(file="figures/03.fig1x.eps", width=12, height=6, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

par(mar=c(4.1, 3.6, 1.1, 0.1))