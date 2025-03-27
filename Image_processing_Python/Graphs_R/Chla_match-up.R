library(RPMG)
library(Metrics)

#x <- c(24.6,6.81, 12.6, 8.28, 12.7, 12, 33) # valor in situ
#y <- c(25.91819572, 9.172709, 13.72306, 7.48801, 13.91085, 13.15464, 33.817)  # valor satelite

chla <- read.csv(file = "matchup_7puntos_headers.csv",
                     header = TRUE, stringsAsFactors=FALSE)

par(mar=c(6.5, 7.5, 1, 1), xpd=F)

dataname <- c("July_2021","August_2021", "March_2022", "May_2023")
coll <- c("#C6E297", "#7CB342", "#11692E", "#42A787") ## "#6EB6D9", , "#1C6CAA")
cexaxis = 1.8
cexpoints = 2.5

plot(x = chla$chla.in_situ, y = chla$chla.satelite, type = "n", ylim=c(5, 35), 
     xlim = c(5, 35), xlab = "", ylab = "", axes = FALSE)


lines(x = chla$in_situ[1:1], y = chla$satelite[1:1], #August_2021
      type = "p", col = coll[2], pch = 19, cex = cexpoints) 
lines(x = chla$in_situ[2:6], y = chla$satelite[2:6], #July_2021
      type = "p", col = coll[1], pch = 19, cex = cexpoints)
lines(x = chla$in_situ[7:7], y = chla$satelite[7:7], #March_2022
      type = "p", col = coll[3], pch = 19, cex = cexpoints)
lines(x = chla$in_situ[8:8], y = chla$satelite[8:8], #May_2023
      type = "p", col = coll[4], pch = 19, cex = cexpoints)


#Linea 1:1
lines(x = seq(0, 35, 1), y = seq(0, 35, 1), 
      type = "l", col = "#757575", lwd = 3, lty = 2) # cambiar type="l" para que dibuje la linea. lwd = grosor, lty = type


model = lm(satelite~in_situ, data = chla)


axis(side = 1, at = seq(0, 35, 5), labels = seq(0, 35, 5), lwd = 2, cex.axis = cexaxis)
axis(side = 2, at = seq(0, 35, 5), labels = seq(0, 35, 5), lwd = 2, cex.axis = cexaxis, las = 1)


grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de linea
     col = "gray", # Color
     lwd = 2)      # Ancho de linea


clip(7, 34, 0, 35)
abline(lm(satelite~in_situ, data = chla), col = "darkgreen", lwd = 3, lty = 3)



mtext(text = expression(paste("Chl-a in-situ (mg/", m^3, ")", sep = "")), 
      side = 1, line = 3.3, cex = 2.2)
mtext(text = expression(paste("Chl-a Sentinel-2 (mg/", m^3, ")", sep = "")), 
      side = 2, line = 4.5, cex = 2.2)

legend(7, 33, legend=c("July 2021", "August 2021", "March 2022", "May 2023"), bty="n",
             cex=2, col=coll, text.col="black", pch = rep(19, 3))


summary(model)

{
    dev.off()
}

predicted <- na.omit(chla$satelite)
observed <- as.numeric(model[["fitted.values"]])

maedat <- mae(observed, predicted) # Mean absolute error
maedat
maddat <- mad(observed, predicted) # Median absolute error
maddat


# Calculate bias
bias(observed, predicted)
print(bias)
bias = (sum(predicted-observed))/length(observed)
bias

# Calculate MAE and RMSE
mae <- mean(abs(predicted - observed))
rmse <- sqrt(mean((predicted - observed)^2))
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))

jpng(paste("export/", "chla", sep = ""), P=c(14,9.8)) 

