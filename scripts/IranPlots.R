###############################################################################
#' Nice versions of figures for PopHzn 12(3) 
#' Fertility, Marriage, and Family Planning in Iran: Implications for Future Policy
#' Meimanat Hosseini-Chavoshi 1 , Mohammad Jalal Abbasi-Shavazi 2 and Peter McDonald
###############################################################################
#' data copy pasted from submitted excel file. double checked
#' sources were not checked but are given nonetheless
#' 17.1. redid the figures because the typesetting fucked up the font
#' now they are without embedding fonts, this seems to work?
###############################################################################

#' 00. Preliminaries
###############################################################################
require(dplyr)
require(tidyr)
library(extrafont)
# font_import()
loadfonts(device="postscript")

# for some reason r seems to forget where ghostscript is every time - i 
# need it for embedding fonts.. 
# Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.16/bin/gswin64c.exe")

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
w <- 6
h <-6

postscript(file="figures/03.fig1x.eps", width=w, height=h, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")
layout(1)
par(mar=c(4.1, 3.6, 1.1, 2.1), xpd = TRUE)
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

legend(1994.5, 7, c("Synthetic PPR", "Real PPR", "Own children (DHSs)", "Own children (censuses)"), 
       cex=1.2,lty = c(1,1,1,2), lwd = 2, bty = "n",  y.intersp = 2,
       col = c("gray60", "gray60", "black", "black"))
legend(1994.5, 7, c("", ""), lty = c(0,0, 0, 0),
       cex=1.2,   bty = "n", pch = c(21,21, NA, NA),   y.intersp = 2, 
       col = c("gray60", "gray60", NA, NA),
       pt.bg= c("black", "white", NA, NA), pt.cex = c(1.2, 1.2, NA, NA))


mtext("Total Fertility Rate", side = 2, line = 2.5, cex = 1.5)
mtext("Year", side = 1, line = 3, cex = 1.5)


dev.off()


#' Figure 2
###############################################################################
w <- 12
h <- 8

postscript(file="figures/03.fig2x.eps", width=w, height=h, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

layout(mat=matrix(c(1,2,3,4,5,5), c(3,2), byrow = TRUE),
       height = c(1,1,0.18))
par(xpd = TRUE)
par(mar=c(2.6, 3.6, 3.1, 1.1))

FunPlot(ldf[[2]]$X, ldf[[2]][,2], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,2], lwd = 2, pch = 21, col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,3], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(2, las = 2, cex.axis = 1.2)
mtext("From first marriage to 1st birth", side =3, line =1, cex = 1.2)
axis(1,cex.axis = 1.2)     

par(mar=c(2.6, 1.1, 3.1, 3.6))
FunPlot(ldf[[2]]$X, ldf[[2]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,4], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,5], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)

axis(4, las = 2, cex.axis = 1.2, labels = rep("", 6), at = seq(0,1,0.2))
mtext("From 1st birth to 2nd birth", side =3, line = 1, cex = 1.2)
mtext("Parity Progression Ratio", side =4, line =1.5, cex = 1.2)
axis(1,cex.axis = 1.2)     


par(mar=c(2.6, 3.6, 3.1, 1.1))
FunPlot(ldf[[2]]$X, ldf[[2]][,6], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,6], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,7], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(1,cex.axis = 1.2)     
axis(2, las = 2, cex.axis = 1.2, labels = rep("", 6), at = seq(0,1,0.2))
mtext("From 2nd birth to 3nd birth", side =3, line =1, cex = 1.2)
mtext("Year", side =1, line =2.5)
mtext("Parity Progression Ratio", side =2, line =1.5, cex = 1.2)



par(mar=c(2.6, 1.1,3.1, 3.6))
FunPlot(ldf[[2]]$X, ldf[[2]][,8], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(1980, 2010), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[2]]$X, ldf[[2]][,8], lwd = 2, pch = 21,  col = "gray 50", bg = "gray80",cex = 1.2)
FunLine(ldf[[2]]$X, ldf[[2]][,9], pch = 21, col= "gray1", bg = "gray60", lwd = 2, cex = 1.2)
axis(1,cex.axis = 1.2)     

axis(4, las = 2, cex.axis = 1.2)
axis(1,cex.axis = 1.2, at = seq(1980, 2010, 5), labels = rep("",7))     
mtext("From 3rd birth to 4th birth", side =3, line =1, cex = 1.2)
mtext("Year", side =1, line =2.5)


par(mar = c(0,0,0,0))
plot.new()
legend("center", c("2000 Iran DHS", "2000 Iran MiDHS"), cex=1.7,lty = 1, lwd = 2, bty = "n",  
       col = c("gray50", "gray1"), pt.bg = c("gray60", "gray80"), horiz = TRUE, pch = 21, pt.cex = 1.2)

dev.off()
#embedFonts(file="figures/03.fig2x.eps", outfile="figures/03.fig2xEmbedded.eps",
#           options = paste0("-dDEVICEWIDTHPOINTS=", w*72,  " -dDEVICEHEIGHTPOINTS=", h*72))


#' Figure 3
###############################################################################
w <- 6
h <- 6
postscript(file="figures/03.fig3x.eps", width=w, height=h, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

par(mar=c(4.1, 4.1, 1.1, 0.1))

FunPlot(ldf[[3]]$Age, ldf[[3]][,1], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(10, 35), c(i,i), lty = 2, col = "gray80")
}

FunLine(ldf[[3]]$Age, ldf[[3]][,2], lwd = 2, pch = 19, col = "gray70",cex = 1)
FunLine(ldf[[3]]$Age, ldf[[3]][,3], lwd = 2, pch = 19, col = "gray55",cex = 1)
FunLine(ldf[[3]]$Age, ldf[[3]][,4], lwd = 2, pch = 19, col = "gray40",cex = 1)
FunLine(ldf[[3]]$Age, ldf[[3]][,5], lwd = 2, pch = 19, col = "gray10",cex = 1)

axis(1,cex.axis = 1.2)     
axis(2, las = 2, cex.axis = 1.2,  at = seq(0,1,0.2))
mtext("Age", side =1, line =2.5, cex = 1.5)
mtext("Proportion never married", side =2, line =3, cex = 1.5)

legend(25, 1, rev(c("1966 Birth cohort", "1976 Birth cohort", "1981 Birth cohort", "1986 Birth cohort")), 
      lwd = 2, bty = "n",  y.intersp = 2,pch = rep(19,4),
       col = c("gray10", "gray40", "gray55", "gray70"))

dev.off()

#embedFonts(file="figures/03.fig3x.eps", outfile="figures/03.fig3xEmbedded.eps",
#           options = paste0("-dDEVICEWIDTHPOINTS=", w*72,  " -dDEVICEHEIGHTPOINTS=", h*72))


#' Figure 4
###############################################################################
w <- 12
h <- 8
postscript(file="figures/03.fig4x.eps", width=w, height=h, family="Garamond", 
           onefile=FALSE, horizontal=FALSE,paper = "special")

layout(mat = matrix(c(1,2,3,4,5,6,7,7,7), c(3,3), byrow = TRUE),
       heights = c(1,1,0.15))
par( xpd = TRUE)
par(mar=c(4.1, 3.6, 5.1, 1.1))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}

FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Illiterate or primary"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "F2S" &
                             ldf[[4]]$education == "Illiterate or primary"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Illiterate or primary"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "F2S" &
                             ldf[[4]]$education == "Illiterate or primary"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)
axis(2, las = 2, cex.axis = 1.2)
mtext("Illiterate & Primary School", side =3, line =0)
axis(1, cex.axis = 1.2)
mtext("Months since first birth", side =1, line =3, cex = 0.9)


par(mar=c(4.1, 2.35, 5.1, 2.35))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Secondary"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "F2S" &
                             ldf[[4]]$education == "Secondary"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Secondary"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "F2S" &
                                              ldf[[4]]$education == "Secondary"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)

mtext("Secondary School", side =3, line =0)
axis(1, cex.axis = 1.2)
mtext("Progression ratios from first brith to the second birth", side =3, line =2.5, cex = 1.5)
mtext("Months since first birth", side =1, line =3, cex = 0.9)



par(mar=c(4.1, 1.1, 5.1, 3.6))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Diploma or university"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "F2S" &
                             ldf[[4]]$education == "Diploma or university"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "F2S" & 
                     ldf[[4]]$education == "Diploma or university"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "F2S" &
                                              ldf[[4]]$education == "Diploma or university"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)

mtext("Diploma or university ", side =3, line =0)
mtext("Parity Prigression Ratio", side =4, line =1.5, cex = 1)
axis(4, at = seq(0,1,0.2), labels = rep("", 6))

axis(1, cex.axis = 1.2)
mtext("Months since first birth", side =1, line =3, cex = 0.9)


par(mar=c(4.1, 3.6, 5.1, 1.1))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}

FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Illiterate or primary"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "S2T" &
                             ldf[[4]]$education == "Illiterate or primary"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Illiterate or primary"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "S2T" &
                                              ldf[[4]]$education == "Illiterate or primary"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)
axis(2, at = seq(0,1,0.2), labels = rep("", 6), cex.axis = 1.2)
mtext("Illiterate & Primary School", side =3, line =0)
axis(1, cex.axis = 1.2)
mtext("Parity Prigression Ratio", side =2, line =1.5, cex = 1)
mtext("Months since second birth", side =1, line =3, cex = 0.9)


par(mar=c(4.1, 2.35, 5.1, 2.35))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Secondary"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "S2T" &
                             ldf[[4]]$education == "Secondary"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Secondary"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "S2T" &
                                              ldf[[4]]$education == "Secondary"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)

mtext("Secondary School", side =3, line =0)
axis(1, cex.axis = 1.2)
mtext("Progression ratios from second brith to the third birth", side =3, line =2.5, cex = 1.5)
mtext("Months since second birth", side =1, line =3, cex = 0.9)


par(mar=c(4.1, 1.1, 5.1, 3.6))

FunPlot(ldf[[4]]$X, ldf[[4]][,4], c(0,1))
for (i in seq(0,1,0.2)){
  lines(c(0, 60), c(i,i), lty = 2, col = "gray80")
}
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Diploma or university"],
        ldf[[4]]$All.women[ldf[[4]]$parity.progression == "S2T" &
                             ldf[[4]]$education == "Diploma or university"], 
        lwd = 2, pch = 21, col = "gray 60", bg = "gray60",cex = 1.2)
FunLine(ldf[[4]]$X[ldf[[4]]$parity.progression == "S2T" & 
                     ldf[[4]]$education == "Diploma or university"],
        ldf[[4]]$Non.users.of.contraception[ldf[[4]]$parity.progression == "S2T" &
                                              ldf[[4]]$education == "Diploma or university"], 
        lwd = 2, pch = 21, col = "gray 40", bg = "gray40",cex = 1.2)

mtext("Diploma or university ", side =3, line =0)
axis(1, cex.axis = 1.2)
mtext("Months since second birth", side =1, line =3, cex = 0.9)

axis(4, las = 2, cex.axis = 1.2)
par(mar=c(0,0,0,0))

plot.new()
legend("center", c("All women", "Non-users of contraception"), cex=1.3,lty = 1, lwd = 2, bty = "n",  
       col = c("gray60", "gray40"), horiz = TRUE, pch = 19, pt.cex = 1.2)


dev.off()
#embedFonts(file="figures/03.fig4x.eps", outfile="figures/03.fig4xEmbedded.eps",
#           options = paste0("-dDEVICEWIDTHPOINTS=", w*72,  " -dDEVICEHEIGHTPOINTS=", h*72))

## reproducible example of embedding 
# 
# require(extrafont)
# w <- 6
# h <- 8
# postscript(file="test.eps", width=w, height=h, family="Garamond", 
#            horizontal=FALSE, # to make sure chart is printed in portrait orientation 
#            paper = "special") # to make sure paper is not a4, but whatever you tell it to be
# plot(1:10, runif(10), type = "l",
#      main = "Title in my fave font",
#      xlab = "x-label",
#      ylab = "y-label")
# 
# dev.off()
# 
# embedFonts(file="test.eps",
#            options = paste0("-dDEVICEWIDTHPOINTS=", w*72,  
#                             " -dDEVICEHEIGHTPOINTS=", h*72))
