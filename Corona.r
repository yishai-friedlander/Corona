library(reshape)
# library(ReporteRs)
library(png)
library(plotrix)
library(gplots)
library(reshape2)
library(ggplot2)
library(openxlsx)

Sys.setlocale(category = "LC_ALL", locale = "USA")

Corona <- read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")), stringsAsFactors = F)
# Corona <- read.csv("Corona.csv", stringsAsFactors = F)
names(Corona)

Corona.Country <- aggregate(Corona[5:length(Corona)], by = list(Corona$Country.Region), FUN = sum)
names(Corona.Country)[1] <- c("Country")
names(Corona.Country)[2:length(Corona.Country)] <- format.Date(as.Date(substr(names(Corona.Country)[2:length(Corona.Country)], 2, nchar(names(Corona.Country)[2:length(Corona.Country)])), format = "%m.%d.%y"), "%d %b %Y")


sigmoid = function(params, x) {
        params[1] / (1 + exp(-params[2] * (x - params[3])))
}

statistx <- readPNG("logo1.png")



# Add population ----------------------------------------------------------

pop <- read.csv("pop.csv", stringsAsFactors = F)
pop$Population <- as.numeric(gsub(pattern = ",", replacement = "", x = pop$Population))

pop$Country[pop$Country == "South Korea"] <- "Korea, South"
pop$Country[pop$Country == "Czech Republic (Czechia)"] <- "Czechia"
pop$Country[pop$Country == "Taiwan"] <- "Taiwan*"
pop$Country[pop$Country == "United States"] <- "US"
pop$Country[pop$Country == "State of Palestine"] <- "West Bank and Gaza"


Corona.Country. <- merge(Corona.Country, pop, "Country", all = T)

# value in big
Corona.Country.[is.na(Corona.Country.$Population) & !is.na(Corona.Country.$`11 Aug 2020`), c("Country", "Population", "11 Mar 2020")]

# value in pop
# Corona.Country[!is.na(Corona.Country$Population) & is.na(Corona.Country$`11 Mar 2020`), c("Country", "Population", "11 Mar 2020")]









# Outbreak Curve ----------------------------------------------------------

Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
Corona.Country.$Date <- Corona.Country.$variable
Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")



Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
Top.Countries <- unique(c(Top.Countries$Country))

Outbreak.Curve <- Corona.Country.[Corona.Country.$Country %in% Top.Countries,]
Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
Outbreak.Curve$Change <- 0
for (i in 2:length(Outbreak.Curve$Country)) {
        if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
                Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
        }
}


Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000

Colors <- data.frame(Country = c("Italy", "China", "Iran", "Chile", "France", "Switzerland", "US",
                                 "Bahrain", "Israel", "Germany", "United Kingdom", "Uruguay", "Brazil", "Russia",
                                 "Hungary", "Belarus", "Qatar", "Singapore"), 
                     Color = c("bisque4", "light green", "cornflowerblue", "cyan", "magenta", "dark gray", "black", "red", "orange",
                               "dark red", "brown", "red", "blue", "pink", "darkmagenta", 
                               "chartreuse3", "darkgoldenrod1", "goldenrod4"))
Colors$Color <- as.character(Colors$Color)

# , "green"

# Outbreak.Curve <- Outbreak.Curve[Outbreak.Curve$Country %in% c("China", "Italy", "Spain", "Iran", "Bahrain", "Israel"),]
# Top.Countries <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve$Country)]


Outbreak.Curve. <- Outbreak.Curve[Outbreak.Curve$Country %in% c("France", "Uruguay", "US", "United Kingdom", "Brazil",
                                                                "Italy", "Chile", "Germany", "Israel", "Hungary", "Switzerland"),]
Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]

jpeg(paste0("plots/outbreak curve.per.1M.controled ", Sys.Date(), ".jpg"), width = 800, height = 800)

i <- 1
tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
              "\nPlot shows population adjusted epidemiological curves of COVID-19\nThe epidemiological curves are smoothed to emphasis the curve trend")
layout(matrix(c(2,1)), 0.4, 2)
par(mar = c(4, 4, 4, 4))
textplot(tex, halign = "center", cex = 1)

plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
     col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,1400), 
     ylab="New Cases per 1M", xlab="", main="Epidemiological Curve per 1M for selected countries", xaxt = 'n')
# lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
#       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)

for (i in 2:length(Top.Countries.)) {
        lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                            Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
              col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
}
axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
     labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)

rasterImage(statistx, 
            xleft=min(Outbreak.Curve.$Date.) + 60, min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25 + 60, 
            ybottom=max(max(Outbreak.Curve.$Change))*(0.20), ytop=max(max(Outbreak.Curve.$Change))*0.30)


dev.off()







# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Liechtenstein", "France", "Belgium", "Singapore",
#                                                                  "Italy", "Switzerland", "Spain", "Germany", "Israel", "Iran"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# jpeg(paste0("plots/outbreak curve.per.1M.rising ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# i <- 1
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with active outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,400),
#      xlim=as.numeric(c(as.Date("2020-02-15"), max(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]]))), 
#      ylab="New Cases per 1M", xlab="", main="Epidemiological Curve per 1M for selected countries\nRising Curve", xaxt = 'n')
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)
# 
# rasterImage(statistx, 
#             xleft=as.Date("2020-02-15"), as.Date("2020-02-15") + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.20, 
#             ybottom=max(max(Outbreak.Curve.$Change))*(0.15), ytop=max(max(Outbreak.Curve.$Change))*0.20)
# 
# 
# dev.off()



# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Korea, South", "Liechtenstein", "France", "Sweden", 
#                                                                  "Turkey", "Italy", "Switzerland"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# jpeg(paste0("plots/outbreak curve.per.1M.rising ", Sys.Date(), ".jpg"), width = 800, height = 1200)
# 
# # par(mfrow=c(2,1))
# 
# i <- 1
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with active outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(mat = matrix(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4), ncol = 3), widths = 1, heights = c(2,0.5,2,1))
# layout.show(n = 4)
# par(mar = c(6, 6, 6, 6))
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,max(Outbreak.Curve.$Change)*0.9), 
#      ylab="New Cases per 1M", xlab="", main="Outbreak Curve per 1M for selected countries\nRising Curve", xaxt = 'n',
#      xlim = c(as.Date("2020-03-10"), max(Outbreak.Curve.$Date.)), cex.main=2, cex.lab=2, cex.axis=2)
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2, cex.axis=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color, cex=2)
# 
# rasterImage(statistx, 
#             xleft=as.numeric(as.Date("2020-03-10"))*1.0004, as.numeric(as.Date("2020-03-10"))*1.0004 + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.05, 
#             ybottom=max(Outbreak.Curve.$Change)*(0.65), ytop=max(Outbreak.Curve.$Change)*0.83)
# 
# 
# par(mar = c(1, 1, 1, 1))
# textplot(" ", halign = "center", cex = 2)
# 
# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Korea, South", "Liechtenstein", "Sweden", "Italy", "Spain", "Switzerland",
#                                                                  "Turkey"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# 
# i <- 1
# 
# par(mar = c(6, 6, 6, 6))
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,max(Outbreak.Curve.$Change)*0.7), 
#      ylab="New Cases per 1M", xlab="", main="Outbreak Curve per 1M for selected countries\nMild Rising Curve", xaxt = 'n',
#      xlim = c(as.Date("2020-03-10"), max(Outbreak.Curve.$Date.)), cex.main=2, cex.lab=2, cex.axis=2)
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2, cex.axis=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color, cex=2)
# 
# 
# textplot(tex, halign = "center", cex = 2)
# 
# dev.off()






# Curve Exploratory -------------------------------------------------------



# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# 
# 
# Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
# Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
# Top.Countries <- unique(c(Top.Countries$Country[1:10], "Israel", "Korea, South", "Turkey", "Poland", "Philippines"))
# 
# Outbreak.Curve <- Corona.Country.
# Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
# Outbreak.Curve$Change <- 0
# for (i in 2:length(Outbreak.Curve$Country)) {
#         if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
#                 Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
#         }
# }
# 
# 
# Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
# Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000
# 
# 
# 
# 
# Outbreak.Curve. <- Outbreak.Curve
# Top.Countries. <- unique(Outbreak.Curve$Country)
# 
# 
# 
# layout(matrix(c(1,1)), 0.4, 2)
# for (i in 1:length(Top.Countries.)) {
#         if (sum(!is.na(Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]]) > 0) & max(Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]]) > 20) {
#                 jpeg(paste0("plots/countries/outbreak curve.", Top.Countries.[i], ".", Sys.Date(), ".jpg"), width = 800, height = 800)
#                 plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#                      col = 1, lwd=2, type='l', main=Top.Countries.[i])
#                 dev.off()
#         }
# 
# }





# Second Wave -------------------------------------------------------------


# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# 
# # "Brunei", "Monaco" , "Croatia", "North Macedonia"
# Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
# Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
# Top.Countries <- c("Azerbaijan", "Iran", "Lithuania", "Malta", "Djibouti")
# 
# Outbreak.Curve <- Corona.Country.[Corona.Country.$Country %in% Top.Countries,]
# Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
# Outbreak.Curve$Change <- 0
# for (i in 2:length(Outbreak.Curve$Country)) {
#         if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
#                 Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
#         }
# }
# 
# 
# Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
# Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000
# 
# Colors <- data.frame(Country = c("Azerbaijan", "Brunei", "Iran", "Monaco", "Lithuania", "Croatia", "Spain",
#                                  "North Macedonia", "Djibouti", "Malta", "United Kingdom", "Belgium", "Brazil", "Russia",
#                                  "Sweden", "Belarus", "Qatar", "Singapore"),
#                      Color = c("chartreuse3", "light green", "cornflowerblue", "cyan", "magenta", "dark gray", "black", "black", "orange",
#                                "dark red", "brown", "red", "blue", "pink", "darkmagenta",
#                                "darkgoldenrod1", "bisque4", "goldenrod4"))
# Colors$Color <- as.character(Colors$Color)
# 
# # , "green"
# 
# # Outbreak.Curve <- Outbreak.Curve[Outbreak.Curve$Country %in% c("China", "Italy", "Spain", "Iran", "Korea, South", "Israel"),]
# # Top.Countries <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve$Country)]
# 
# 
# Outbreak.Curve. <- Outbreak.Curve
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# 
# 
# jpeg(paste0("plots/Second.Wave ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"),
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with potential second wave outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# aa <- 0
# for (i in 1:length(Top.Countries.)) {
#        aa <- max(aa, smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7)$y) 
# }
# 
# i <- 1
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l',
#      ylab="New Cases per 1M", xlab="", main="Second Wave", xaxt = 'n',
#      ylim=c(0,max(aa)))
# 
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)
# 
# rasterImage(statistx,
#             xleft=min(Outbreak.Curve.$Date.), min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25,
#             ybottom=max(max(Outbreak.Curve.$Change))*(0.20), ytop=max(max(Outbreak.Curve.$Change))*0.25)
# 
# 
# dev.off()

# South America -------------------------------------------------------------


# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# 
# 
# Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
# Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
# Top.Countries <- c("Bolivia", "Brazil", "Peru", "Chile", "Argentina", "Colombia", "Venezuela")
# 
# Outbreak.Curve <- Corona.Country.[Corona.Country.$Country %in% Top.Countries,]
# Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
# Outbreak.Curve$Change <- 0
# for (i in 2:length(Outbreak.Curve$Country)) {
#         if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
#                 Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
#         }
# }
# 
# 
# Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
# Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000
# 
# Colors <- data.frame(Country = c("Bolivia", "Brazil", "Peru", "Chile", "Mexico", "Venezuela", "Costa Rica", "Uruguay",
#                                  "Nicaragua", "Jamaica", "Bahamas", "Colombia", "Russia", "Argentina",
#                                  "Sweden", "Belarus", "Qatar", "Singapore"),
#                      Color = c("chartreuse3", "red", "cornflowerblue", "cyan", "magenta", "dark gray", "black", "black", "orange",
#                                "dark red", "brown", "light green", "blue", "pink", "darkmagenta",
#                                "darkgoldenrod1", "bisque4", "goldenrod4"))
# Colors$Color <- as.character(Colors$Color)
# 
# # , "green"
# 
# # Outbreak.Curve <- Outbreak.Curve[Outbreak.Curve$Country %in% c("China", "Italy", "Spain", "Iran", "Korea, South", "Israel"),]
# # Top.Countries <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve$Country)]
# 
# 
# Outbreak.Curve. <- Outbreak.Curve
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# 
# 
# jpeg(paste0("plots/Americas ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"),
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# aa <- 0
# for (i in 1:length(Top.Countries.)) {
#         aa <- max(aa, smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7)$y) 
# }
# 
# i <- 1
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l',
#      ylab="New Cases per 1M", xlab="", main="South America", xaxt = 'n',
#      ylim=c(0,max(aa)))
# 
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)
# 
# rasterImage(statistx,
#             xleft=min(Outbreak.Curve.$Date.), min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25,
#             ybottom=max(max(Outbreak.Curve.$Change))*(0.20), ytop=max(max(Outbreak.Curve.$Change))*0.35)
# 
# 
# dev.off()

# First Wave -------------------------------------------------------------


# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# 
# # "Brunei", "Monaco" , "Croatia", "North Macedonia"
# Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
# Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
# Top.Countries <- c("Armenia", "Bahrain", "Belarus", "Brazil", "Chile", "Gabon", "Kuwait", "Maldives", "Oman", "Peru", "Qatar", "Russia",
#                    "Saudi Arabia", "Singapore", "Sweeden", "United Arab Emirates")
# 
# Outbreak.Curve <- Corona.Country.[Corona.Country.$Country %in% Top.Countries,]
# Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
# Outbreak.Curve$Change <- 0
# for (i in 2:length(Outbreak.Curve$Country)) {
#         if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
#                 Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
#         }
# }
# 
# 
# Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
# Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000
# 
# Colors <- data.frame(Country = c("Armenia", "Bahrain", "Belarus", "Brazil", "Chile", "Gabon", "Kuwait", "Maldives", "Oman", "Peru", "Qatar", "Russia",
#                                  "Saudi Arabia", "Singapore", "Sweeden", "United Arab Emirates"),
#                      Color = c("chartreuse3", "light green", "cornflowerblue", "cyan", "magenta", "dark gray", 
#                                "black", "orange",
#                                "dark red", "brown", "red", "blue", "pink", "darkmagenta",
#                                "bisque4", "goldenrod4"))
# Colors$Color <- as.character(Colors$Color)
# 
# # , "green"
# 
# # Outbreak.Curve <- Outbreak.Curve[Outbreak.Curve$Country %in% c("China", "Italy", "Spain", "Iran", "Korea, South", "Israel"),]
# # Top.Countries <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve$Country)]
# 
# 
# Outbreak.Curve. <- Outbreak.Curve
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# 
# 
# jpeg(paste0("plots/First.Wave ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"),
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with rising outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# aa <- 0
# for (i in 1:length(Top.Countries.)) {
#         aa <- max(aa, smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7)$y) 
# }
# 
# i <- 1
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l',
#      ylab="New Cases per 1M", xlab="", main="Second Wave", xaxt = 'n',
#      ylim=c(0,max(aa)))
# 
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)
# 
# rasterImage(statistx,
#             xleft=min(Outbreak.Curve.$Date.), min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25,
#             ybottom=max(max(Outbreak.Curve.$Change))*(0.20), ytop=max(max(Outbreak.Curve.$Change))*0.25)
# 
# 
# dev.off()

# Fataity Rate ------------------------------------------------------------

# DATE <- format.Date(Sys.Date()-1, "%m-%d-%Y")
# Fatality <- read.csv(url(paste0("http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", DATE, ".csv")), stringsAsFactors = F)
# # Fatality <- read.csv("daily.csv", stringsAsFactors = F)
# Fatality <- aggregate(Fatality[c("Confirmed", "Deaths", "Recovered")], by = list(Fatality$Country_Region), FUN = sum)
# names(Fatality) <- c("Country", "Cases", "Death", "Recovered")
# if (class(Fatality$Cases) == "character") {
#         Fatality$Cases <- as.numeric(gsub(pattern = ",", replacement = "", x = Fatality$Cases))
#         Fatality$Death <- as.numeric(gsub(pattern = ",", replacement = "", x = Fatality$Death))
#         Fatality$Recovered <- as.numeric(gsub(pattern = ",", replacement = "", x = Fatality$Recovered))
# }
# # Fatality <- Corona.Country[order(-Corona.Country[length(names(Corona.Country))]), c("Country", names(Corona.Country)[length(names(Corona.Country))])]
# # names(Fatality)[2] <- "Cases"
# 
# # Fatality. <- Death.Country[c("Country", names(Death.Country)[length(names(Death.Country))])]
# # names(Fatality.)[2] <- "Death"
# # 
# # Fatality <- merge(Fatality, Fatality., "Country")
# Fatality <- Fatality[order(-Fatality$Death),]
# Fatality$Country <- paste0(1:length(Fatality$Country), ". ", Fatality$Country)
# 
# Fatality.Israel <- Fatality[grepl("Israel", Fatality$Country),]
# Fatality <- Fatality[1:20,]
# Fatality <- rbind(Fatality, Fatality.Israel)
# Fatality$Fatality <- (Fatality$Death / Fatality$Cases) * 100
# 
# Fatality <- Fatality[order(-Fatality$Fatality),]
# 
# Fatality$Active <- Fatality$Cases - Fatality$Death - Fatality$Recovered
# Fatality$Estimated <- ((Fatality$Death + (Fatality$Active * (Fatality$Fatality/100))) / Fatality$Cases) * 100
# 
# 
# jpeg(paste0("plots/Fatality ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
#               "\nPlot shows fatality rates (death / cases) for the 20 countries with the most death cases and Israel.\nIn red is the estimated fatality rate when appling the current fatality rate\non the remaining active cases and adding them to the death count.")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# barplot(Fatality$Estimated, names.arg = Fatality$Country, cex.names = 1, 
#         las=2, main = paste0("COVID-19 Fatality Rates"), 
#         cex.axis = 1, ylab="%", col='red')
# barplot(Fatality$Fatality, names.arg = Fatality$Country, cex.names = 1, 
#         las=2,  add=T, xaxt = 'n')
# 
# 
# 
# statistx <- readPNG("logo1.png")
# rasterImage(statistx, 
#             xleft=8, xright=(15), 
#             ybottom=17, ytop=20)
# 
# 
# tab <- Fatality[c("Country", "Cases", "Death", "Fatality", "Estimated")]
# tab$Fatality <- round(tab$Fatality,1)
# tab$Estimated <- round(tab$Estimated,1)
# addtable2plot(17.5 ,10,tab,bty="o",display.rownames=F,hlines=TRUE,
#               vlines=TRUE, cex = 0.80)
# 
# dev.off()




# Age Stratification Israel -----------------------------------------------

# TOTAL.CONFIRMED <- 7030
# 
# Israel.Population <- read.csv("israel population.csv", stringsAsFactors = F)
# Israel.Population$pop
# Israel.Population$pop <- as.numeric(gsub(pattern = ",", replacement = "", x = Israel.Population$pop))
# Israel.Population$Age[Israel.Population$Age == "95+"] <- 96
# Israel.Population$Age <- as.numeric(Israel.Population$Age)
# 
# is.pop <- data.frame(Ages = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
#                      Confirmed = c(0.04, 0.09, 0.23, 0.15, 0.13, 0.14, 0.12, 0.09)*TOTAL.CONFIRMED)
# is.pop$population <- c(sum(Israel.Population$pop[Israel.Population$Age %in% 0:9]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 10:19]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 20:29]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 30:39]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 40:49]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 50:59]),
#                        sum(Israel.Population$pop[Israel.Population$Age %in% 60:69]),
#                        sum(Israel.Population$pop[Israel.Population$Age >= 70]))
# 
# (sum(Israel.Population$pop[Israel.Population$Age %in% 70:79]) * 0.174 + 
# sum(Israel.Population$pop[Israel.Population$Age %in% 80:89]) * 0.252 + 
# sum(Israel.Population$pop[Israel.Population$Age %in% 90:99]) * 0.254) /  sum(Israel.Population$pop[Israel.Population$Age %in% 70:99])
# 
# israel.Death <- read.csv("Israel.Death.csv")
# is.pop$Death.Israel <- c(length(israel.Death$Age[israel.Death$Age %in% 0:9]),
#                        length(israel.Death$Age[israel.Death$Age %in% 10:19]),
#                        length(israel.Death$Age[israel.Death$Age %in% 20:29]),
#                        length(israel.Death$Age[israel.Death$Age %in% 30:39]),
#                        length(israel.Death$Age[israel.Death$Age %in% 40:49]),
#                        length(israel.Death$Age[israel.Death$Age %in% 50:59]),
#                        length(israel.Death$Age[israel.Death$Age %in% 60:69]),
#                        length(israel.Death$Age[israel.Death$Age >= 70]))
# 
# is.pop$Actual.Fatality <- (is.pop$Death.Israel / is.pop$Confirmed) * 100 
# 
# 
# is.pop$Italy.Fatality <- c(0, 0, 0, 0.3, 0.7, 1.7, 6.1, 20.3) / 100
# 
# is.pop$Estimated.Cases <- is.pop$Confirmed * (TOTAL.CONFIRMED/sum(is.pop$Confirmed)) 
# 
# is.pop$Expected.Death.Italy <- is.pop$Estimated.Cases * is.pop$Italy.Fatality
# 
# 
# 
# ceiling(sum(is.pop$Expected.Death.Italy))
# 
# 
# 
# barplot(is.pop$Expected.Death.Italy, col='red')
# barplot(is.pop$Death.Israel, col='gray', add=T)


# Denmark Outbreak ---------------------------------------------------------



# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# Corona.Country.Denmark <- Corona.Country.[Corona.Country.$Country == "Denmark" & Corona.Country.$Date. > "2020-02-18", ]
# 
# 
# 
# 
# 
# Death <- read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")), stringsAsFactors = F)
# # Death <- read.csv("Death.csv", stringsAsFactors = F)
# names(Death)
# 
# Death.Country <- aggregate(Death[5:length(Death)], by = list(Death$Country.Region), FUN = sum)
# names(Death.Country)[1] <- c("Country")
# names(Death.Country)[2:length(Death.Country)] <- format.Date(as.Date(substr(names(Death.Country)[2:length(Death.Country)], 2, nchar(names(Death.Country)[2:length(Death.Country)])), format = "%m.%d.%y"), "%d %b %Y")
# 
# 
# 
# Corona.Country.Denmark <- Corona.Country.Denmark[Corona.Country.Denmark$value > 0,]
# 
# START <- min(Corona.Country.Denmark$Date.)
# 
# Death.Denmark <- Death.Country[Death.Country$Country == "Denmark",]
# Death.Denmark <- Death.Denmark[!(names(Death.Denmark) %in% format.Date(as.Date(as.numeric(as.Date("2020-01-22")):(as.numeric(START)-1), origin = "1970-01-01"), "%d %b %Y"))]
# 
# 
# 
# Corona.Country.Denmark$Date. <- as.numeric(Corona.Country.Denmark$Date. - START)
# 
# 
# 
# 
# jpeg(paste0("plots/Denmark.outbreak ", Sys.Date(), ".jpg"), width = 800, height = 500)
# 
# 
# 
# 
# par(mar = c(8,5,5,5))
# plot(Corona.Country.Denmark$Date., Corona.Country.Denmark$value, col='blue', las = 2, xlab="",
#      xaxt = 'n', main = "Coronavirus outbreak in Denmark", ylab = "Total Cases",
#      xlim = c(0, max(Corona.Country.Denmark$Date.)),
#      ylim=c(0,12000), yaxt='n')
# grid()
# 
# 
# 
# lines(Corona.Country.Denmark$Date., Corona.Country.Denmark$value, col='blue', lwd=3)
# # lines(smooth.spline(Corona.Country.Denmark$Date., Corona.Country.Denmark$value, spar = 0.7), col='blue', lty=2)
# axis(2, col='blue')
# axis(1, at = min(Corona.Country.Denmark$Date.):(max(Corona.Country.Denmark$Date.)+10), las = 2,
#      labels = format.Date(as.Date(START:(max(Corona.Country.Denmark$Date.)+10+START), origin = "1970-01-01"), "%d %b %Y"), cex.axis=0.8)
# 
# abline(v=15, col='green', lty=2)
# 
# abline(v=48, col='orange', lty=2)
# 
# 
# legend('topleft', legend = c("Total Cases", "Total Death", "Closed Borders", "Opened Schools"),
#        col = c("blue", "red", "green", "orange"), lty=c(1, 1, 2, 2))
# 
# dth.isr <- as.numeric((Death.Denmark[names(Death.Denmark)[2:length(Death.Denmark)]]))
# dth.isr. <- dth.isr
# dth.isr <- (dth.isr * sd(Corona.Country.Denmark$value) / sd(dth.isr)) * 0.5
# 
# points(Corona.Country.Denmark$Date., dth.isr, col='red')
# lines(Corona.Country.Denmark$Date., dth.isr, col='red', lwd=3)
# 
# 
# 
# 
# 
# 
# 
# 
# axis(side = 4, at = (seq(0,2000,50) * (sd(Corona.Country.Denmark$value) / sd(dth.isr.))) * 0.5, labels = seq(0,2000,50), col='red')
# mtext(text = "Death", side = 4, line=2)
# 
# 
# rasterImage(statistx,
#             xleft=30, xright=30 + length(Corona.Country.Denmark$Date.)*0.15,
#             ybottom=max(Corona.Country.Denmark$value)*(0.8), ytop=max(Corona.Country.Denmark$value)*1.00)
# text(27 + length(Corona.Country.Denmark$Date.)*0.125, max(Corona.Country.Denmark$value)*(0.75), paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()









# Switzerland Outbreak ---------------------------------------------------------



# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# Corona.Country.Switzerland <- Corona.Country.[Corona.Country.$Country == "Switzerland" & Corona.Country.$Date. > "2020-02-18", ]
# 
# 
# 
# 
# 
# Death <- read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")), stringsAsFactors = F)
# # Death <- read.csv("Death.csv", stringsAsFactors = F)
# names(Death)
# 
# Death.Country <- aggregate(Death[5:length(Death)], by = list(Death$Country.Region), FUN = sum)
# names(Death.Country)[1] <- c("Country")
# names(Death.Country)[2:length(Death.Country)] <- format.Date(as.Date(substr(names(Death.Country)[2:length(Death.Country)], 2, nchar(names(Death.Country)[2:length(Death.Country)])), format = "%m.%d.%y"), "%d %b %Y")
# 
# 
# 
# Corona.Country.Switzerland <- Corona.Country.Switzerland[Corona.Country.Switzerland$value > 0,]
# 
# START <- min(Corona.Country.Switzerland$Date.)
# 
# Death.Switzerland <- Death.Country[Death.Country$Country == "Switzerland",]
# Death.Switzerland <- Death.Switzerland[!(names(Death.Switzerland) %in% format.Date(as.Date(as.numeric(as.Date("2020-01-22")):(as.numeric(START)-1), origin = "1970-01-01"), "%d %b %Y"))]
# 
# 
# 
# Corona.Country.Switzerland$Date. <- as.numeric(Corona.Country.Switzerland$Date. - START)
# 
# 
# 
# 
# jpeg(paste0("plots/Switzerland.outbreak ", Sys.Date(), ".jpg"), width = 800, height = 500)
# 
# 
# 
# 
# par(mar = c(8,5,5,5))
# plot(Corona.Country.Switzerland$Date., Corona.Country.Switzerland$value, col='blue', las = 2, xlab="",
#      xaxt = 'n', main = "Coronavirus outbreak in Switzerland", ylab = "Total Cases",
#      xlim = c(0, max(Corona.Country.Switzerland$Date.)),
#      ylim=c(0,35000), yaxt='n')
# grid()
# 
# 
# 
# lines(Corona.Country.Switzerland$Date., Corona.Country.Switzerland$value, col='blue', lwd=3)
# # lines(smooth.spline(Corona.Country.Switzerland$Date., Corona.Country.Switzerland$value, spar = 0.7), col='blue', lty=2)
# axis(2, col='blue')
# axis(1, at = min(Corona.Country.Switzerland$Date.):(max(Corona.Country.Switzerland$Date.)+10), las = 2,
#      labels = format.Date(as.Date(START:(max(Corona.Country.Switzerland$Date.)+10+START), origin = "1970-01-01"), "%d %b %Y"), cex.axis=0.8)
# 
# # abline(v=15, col='green', lty=2)
# # 
# # abline(v=48, col='orange', lty=2)
# 
# 
# legend('topleft', legend = c("Total Cases", "Total Death"),
#        col = c("blue", "red"), lty=c(1, 1))
# 
# dth.isr <- as.numeric((Death.Switzerland[names(Death.Switzerland)[2:length(Death.Switzerland)]]))
# dth.isr. <- dth.isr
# dth.isr <- (dth.isr * sd(Corona.Country.Switzerland$value) / sd(dth.isr)) * 0.5
# 
# points(Corona.Country.Switzerland$Date., dth.isr, col='red')
# lines(Corona.Country.Switzerland$Date., dth.isr, col='red', lwd=3)
# 
# 
# 
# 
# 
# 
# 
# 
# axis(side = 4, at = (seq(0,10000,200) * (sd(Corona.Country.Switzerland$value) / sd(dth.isr.))) * 0.5, labels = seq(0,10000,200), col='red')
# mtext(text = "Death", side = 4, line=2)
# 
# 
# rasterImage(statistx,
#             xleft=30, xright=30 + length(Corona.Country.Switzerland$Date.)*0.15,
#             ybottom=max(Corona.Country.Switzerland$value)*(0.9), ytop=max(Corona.Country.Switzerland$value)*1.15)
# text(27 + length(Corona.Country.Switzerland$Date.)*0.125, max(Corona.Country.Switzerland$value)*(0.82), paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()








# Confirmed-Death Correlations --------------------------------------------



# Corona.Country <- aggregate(Corona[5:length(Corona)], by = list(Corona$Country.Region), FUN = sum)
# names(Corona.Country)[1] <- c("Country")
# names(Corona.Country)[2:length(Corona.Country)] <- format.Date(as.Date(substr(names(Corona.Country)[2:length(Corona.Country)], 2, nchar(names(Corona.Country)[2:length(Corona.Country)])), format = "%m.%d.%y"), "%d %b %Y")
# 
# Death.Country <- aggregate(Death[5:length(Death)], by = list(Death$Country.Region), FUN = sum)
# names(Death.Country)[1] <- c("Country")
# names(Death.Country)[2:length(Death.Country)] <- format.Date(as.Date(substr(names(Death.Country)[2:length(Death.Country)], 2, nchar(names(Death.Country)[2:length(Death.Country)])), format = "%m.%d.%y"), "%d %b %Y")
# 
# 
# 
# Countries <- Corona.Country$Country[Corona.Country[,length(names(Corona.Country))] > 1000]
# 
# Confirmed <- Corona.Country[Corona.Country$Country %in% Countries,]
# Mortality <- Death.Country[Death.Country$Country %in% Countries,]
# 
# 
# DAYS <- NULL
# country <- "Israel"
# for (country in Countries) {
#         days <- NULL
#         data1 <- melt(data = Confirmed[Confirmed$Country == country,], id.vars = "Country", measure.vars = names(Confirmed)[2:length(Confirmed)], variable_name = "Date")
#         data2 <- melt(data = Mortality[Mortality$Country == country,], id.vars = "Country", measure.vars = names(Mortality)[2:length(Mortality)], variable_name = "Date")
#         data1$delta <- 0
#         data2$delta <- 0
#         for (j in 2:length(data1$Country)) {
#                 data1$delta[j] <- data1$value[j] - data1$value[j-1]
#                 data2$delta[j] <- data2$value[j] - data2$value[j-1]
#         }
#         data2 <- data2[data2$value > 0,]
#         data1 <- data1[data1$Date %in% data2$Date,]
#         data1$value <- data1$delta
#         data1$delta <- NULL
#         data2$value <- data2$delta
#         data2$delta <- NULL
# 
#         data <- cbind(data1, data2$value)
#         data$Country <- NULL
#         names(data) <- c("Date", "Confirmed", "Death")
#         for (i in 0:(length(data$Date)-7)) {
#                 data1. <- data$Confirmed[1:(length(data$Date)-i)]
#                 data2. <- data$Death[(i+1):length(data$Date)]
#                 COR <- cor(data1., data2.)
#                 Slope <- 1/coef(lm(data2. ~ data1.))[2]
#                 days <- rbind(days, cbind(i, COR, Slope))
# 
#         }
#         DAYS <- rbind(DAYS, c(country, days[days[,2] == max(days[,2], na.rm = T) & !is.na(days[,2]),]))
# }
# 
# DAYS <- as.data.frame(DAYS)
# DAYS$i <- as.numeric(as.character(DAYS$i))
# DAYS$COR <- as.numeric(as.character(DAYS$COR))
# DAYS$Slope <- as.numeric(as.character(DAYS$Slope))






# Days from 5000 cases to 100 death ---------------------------------------


# Corona.Country. <- merge(Corona.Country, pop, "Country", all = T)
# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# Death.Country. <- melt(data = Death.Country, id.vars = "Country", measure.vars = names(Death.Country)[2:length(Death.Country)], variable_name = "Date")
# Death.Country.$value[is.na(Death.Country.$value)] <- 0
# Death.Country.$Date. <- as.Date(as.character(Death.Country.$Date), "%d %b %Y")
# 
# country <- "Israel"
# 
# tab <- NULL
# for (country in unique(Corona.Country.$Country)) {
#         Date.5000 <- Corona.Country.$Date.[Corona.Country.$Country == country & Corona.Country.$value >= 5000][1]
#         Death.100 <- Death.Country.$Date.[Death.Country.$Country == country & Death.Country.$value >= 100][1]
#         if (max(Death.Country.$value[Death.Country.$Country == country]) < 100) {
#                 Death.100 <- Death.Country.$Date.[Death.Country.$Country == country][length(Death.Country.$Date.[Death.Country.$Country == country])]
#                 D100 <- FALSE
#         } else {
#                 D100 <- TRUE
#         }
#         tab <- rbind(tab, cbind(country, as.numeric(Death.100 - Date.5000), D100, max(Death.Country.$value[Death.Country.$Country == country])))
# }
# tab <- as.data.frame(tab)
# names(tab) <- c("Country", "Days", "D.100", "Death")
# tab$Days <- as.numeric(as.character(tab$Days))
# tab <- tab[!is.na(tab$Days),]
# tab <- tab[order(-tab$Days),]
# tab$D.100 <- as.logical(as.character(tab$D.100))
# 
# israel <- (1:length(tab$Country))[tab$Country == "Israel"]
# colors <- rep("gray", length(tab$Days))
# colors[israel] <- "red"
# colors[!tab$D.100] <- "green"
# 
# tab$Country <- as.character(tab$Country)
# tab$Country[!tab$D.100] <- paste0(tab$Country[!tab$D.100], " (", tab$Death[!tab$D.100], ")")
# 
# jpeg(paste0("plots/Days from 5k to 100 ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"),
#               "\nPlot shows the number of days between the 5000 confirmed COVID-19 case\nto the 100th death per country\n",
#               "In green are countries with less than 100 death cases")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(3, 3, 3, 3))
# textplot(tex, halign = "center")
# barplot(tab$Days, names.arg = tab$Country, cex.names = 0.9,
#         las=2, main = paste0("Days from 5000 confirmed COVID-19 case to the 100's death case"),
#         col = colors, cex.axis = 1, ylim=c(-30,30))
# abline(h = tab$Days[tab$Country == "Israel"], col="red", lty = 2)
# 
# 
# 
# statistx <- readPNG("logo1.png")
# rasterImage(statistx,
#             xleft=3, xright=(13),
#             ybottom=-10, ytop=-5)
# 
# 
# 
# dev.off()












# More recoveries than active cases ---------------------------------------

# Corona.Country. <- merge(Corona.Country, pop, "Country", all = T)
# Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
# Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
# Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")
# 
# Death.Country. <- melt(data = Death.Country, id.vars = "Country", measure.vars = names(Death.Country)[2:length(Death.Country)], variable_name = "Date")
# Death.Country.$value[is.na(Death.Country.$value)] <- 0
# Death.Country.$Date. <- as.Date(as.character(Death.Country.$Date), "%d %b %Y")
# 
# recoveries <- read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")), stringsAsFactors = F)
# recoveries.Country <- aggregate(recoveries[5:length(recoveries)], by = list(recoveries$Country.Region), FUN = sum)
# names(recoveries.Country)[1] <- c("Country")
# names(recoveries.Country)[2:length(recoveries.Country)] <- format.Date(as.Date(substr(names(recoveries.Country)[2:length(recoveries.Country)], 2, nchar(names(recoveries.Country)[2:length(recoveries.Country)])), format = "%m.%d.%y"), "%d %b %Y")
# recoveries.Country. <- merge(recoveries.Country, pop, "Country", all = T)
# recoveries.Country. <- melt(data = recoveries.Country, id.vars = "Country", measure.vars = names(recoveries.Country)[2:length(recoveries.Country)], variable_name = "Date")
# recoveries.Country.$value[is.na(recoveries.Country.$value)] <- 0
# recoveries.Country.$Date. <- as.Date(as.character(recoveries.Country.$Date), "%d %b %Y")
# 
# country <- "Israel"
# 
# tab <- NULL
# for (country in unique(Corona.Country.$Country)) {
#         out <- cbind(country, as.data.frame(as.Date(Corona.Country.$Date.[Corona.Country.$Country == country])),
#                      Corona.Country.$value[Corona.Country.$Country == country],
#                      Death.Country.$value[Death.Country.$Country == country],
#                      recoveries.Country.$value[recoveries.Country.$Country == country])
#         names(out) <- c("Country", "Date", "Confirmed", "Death", "Recovered")
#         out$Active <- out$Confirmed - out$Recovered - out$Death
#         tab <- rbind(tab, out)
# }
# 
# 
# country.list <- tab[tab$Recovered > 1000,]
# country.list <- country.list[country.list$Recovered > tab$Active, ]
# country.list <- country.list[!duplicated(country.list$Country),]
# # country.list <- country.list[country.list$Recovered > 1000,]
# 
# country.list.2 <- tab[order(tab$Country, -as.numeric(tab$Date)),]
# country.list.2 <- country.list.2[!duplicated(country.list.2$Country),]
# country.list.2 <- country.list.2[country.list.2$Recovered > country.list.2$Active, ]
# 
# list.100 <- tab[tab$Confirmed >= 100,]
# list.100 <- list.100[!duplicated(list.100$Country), c("Country", "Date")]
# names(list.100)[2] <- "Case.100"
# 
# country.list <- merge(country.list, list.100, "Country", all.x = T)
# country.list$Days <- as.numeric(country.list$Date - country.list$Case.100)
# 
# country.list <- country.list[country.list$Country %in% country.list.2$Country,]
# 
# country.list$Country.Days <- paste0(country.list$Country, " (", country.list$Days, ")")
# 
# country.list <- merge(country.list, pop, "Country", all.x = T)
# 
# country.list$Recovered.pop <- (country.list$Recovered / country.list$Population) * 1000000
# 
# jpeg(paste0("plots/recoveries and active ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"),
#               "\nPlot shows the countries who has more recoveries that active cases\n",
#               "only countries with more than 1000 recoveries are included\n",
#               "date shows the first time of recovery number passing active number\n",
#               "in Parenthesis are the days from the 100 confirmed death to the turnpoint")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 3, 3))
# textplot(tex, halign = "center", cex=0.9)
# 
# plot(c(min(country.list$Date)-8, max(country.list$Date))+5,
#      c(0, max(country.list$Recovered.pop))*1.2, col=0, xlab="",
#      ylab="Number of recoveries per 1M at turn-point", main="Countries who have more recoveries than active cases",
#      xaxt='n', yaxt='n')
# axis(1, at = min(Corona.Country.$Date.):(max(Corona.Country.$Date.) + 10),
#      labels = format.Date(as.Date(min(Corona.Country.$Date.):(max(Corona.Country.$Date.) + 10), origin="1970-01-01"), "%d %b %Y"), las = 2)
# axis(2, at = seq(0, 6000, 1000), labels = format(seq(0, 6000, 1000), scientific = F, big.mark = ","))
# 
# for (country in unique(country.list$Country)) {
#         # abline(v = country.list$Date[country.list$Country == country], col='lightgray', lty=2)
#         segments(x0 = country.list$Date[country.list$Country == country],
#                  x1 = country.list$Date[country.list$Country == country], y0 = 0,
#                  y1 = country.list$Recovered.pop[country.list$Country == country] + 100, col = 'lightgray', lty=2)
#         text(x = country.list$Date[country.list$Country == country],
#              y = country.list$Recovered.pop[country.list$Country == country],
#              labels = country.list$Country.Days[country.list$Country == country], cex = 0.9)
# }
# 
# statistx <- readPNG("logo1.png")
# rasterImage(statistx,
#             xleft=min(country.list$Date), min(country.list$Date) + 15,
#             ybottom=1000, ytop=2000)
# 
# 
# 
# dev.off()





# Israel Outbreak ---------------------------------------------------------

YESTERDAY <- Sys.Date()-1
YESTERDAY.F <- format.Date(YESTERDAY, "%d %b %Y")

Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
Corona.Country.$Date <- Corona.Country.$variable
Corona.Country.$variable <- NULL
Corona.Country.$Date. <- as.Date(as.character(Corona.Country.$Date), "%d %b %Y")

Corona.Country.Israel <- Corona.Country.[Corona.Country.$Country == "Israel" & Corona.Country.$Date. > "2020-02-18", ]



if (sum(Corona.Country.Israel$Date == YESTERDAY.F) == 0) {
        Corona.Country.Israel <- rbind(Corona.Country.Israel, data.frame(Country = "Israel", Date = YESTERDAY.F,
                                                                 value = 1314213, Date. = YESTERDAY))
}


Death <- read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")), stringsAsFactors = F)
# Death <- read.csv("Death.csv", stringsAsFactors = F)
names(Death)

Death.Country <- aggregate(Death[5:length(Death)], by = list(Death$Country.Region), FUN = sum)
names(Death.Country)[1] <- c("Country")
names(Death.Country)[2:length(Death.Country)] <- format.Date(as.Date(substr(names(Death.Country)[2:length(Death.Country)], 2, nchar(names(Death.Country)[2:length(Death.Country)])), format = "%m.%d.%y"), "%d %b %Y")



Corona.Country.Israel <- Corona.Country.Israel[Corona.Country.Israel$value > 0,]

# START <- as.numeric(as.Date("2020-05-15"))
START <- as.numeric(as.Date("2020-02-21"))

Death.Israel <- Death.Country[Death.Country$Country == "Israel",]
Death.Israel <- Death.Israel[!(names(Death.Israel) %in% format.Date(as.Date(as.numeric(as.Date("2020-01-20")):(as.numeric(START)-1), origin = "1970-01-01"), "%d %b %Y"))]
if (!YESTERDAY.F %in% names(Death.Israel)) {
        Death.Israel[, YESTERDAY.F] <- 7976
}



Corona.Country.Israel$Date. <- as.numeric(Corona.Country.Israel$Date. - START)



Corona.Country.Israel <- Corona.Country.Israel[Corona.Country.Israel$Date. >= 0,]

Corona.Country.Israel$New.Cases <- 0
for (i in 2:length(Corona.Country.Israel$Country)) {
        Corona.Country.Israel$New.Cases[i] <- Corona.Country.Israel$value[i] - Corona.Country.Israel$value[i-1]
}


Death.Israel. <- melt(data = Death.Israel, id.vars = "Country", measure.vars = names(Death.Israel)[2:length(Death.Israel)])

Death.Israel.$New.Death <- 0
for (i in 2:length(Death.Israel.$Country)) {
        Death.Israel.$New.Death[i] <- Death.Israel.$value[i] - Death.Israel.$value[i-1]
}


# israel.daily <- read.csv("israel.csv")
israel.daily <- read.xlsx("israel.xlsx")
names(israel.daily)[1] <- "Date"

israel.daily$Date[!is.na(as.numeric(israel.daily$Date))] <- format.Date(as.Date(as.numeric(israel.daily$Date[!is.na(as.numeric(israel.daily$Date))]), origin = "1899-12-30"), "%d/%m/%Y")

israel.daily$Date <- format.Date(as.Date(israel.daily$Date, "%d/%m/%Y"), "%d %b %Y")
Corona.Country.Israel <- merge(Corona.Country.Israel, israel.daily, "Date", all.x = T)
Corona.Country.Israel <- Corona.Country.Israel[order(Corona.Country.Israel$Date.),]


# DAYS <- (length(Corona.Country.Israel$New.Cases)-20):length(Corona.Country.Israel$New.Cases)
DAYS <- ((length(Corona.Country.Israel$New.Cases) - 50):length(Corona.Country.Israel$New.Cases))
YLIMIT <- 13000


yy <- smooth.spline(Corona.Country.Israel$Date.[DAYS],
                    Corona.Country.Israel$New.Cases[DAYS], spar = 0.7)


yylog <- log(yy$x)
yy2 <- yy$x ^ 2
yy3 <- yy$x ^ 3
# mod <- lm(yy$y ~ yylog)
# mod <- lm(yy$y ~ yy$x + yy2 + yy3)
mod <- lm(yy$y ~ yy$x + yy2)
# mod <- lm(yy$y ~ yy$x)
mod <- coef(summary(mod))[,1]
xx <- 1:(max(Corona.Country.Israel$Date.)+10)
yy2 <- xx ^ 2
yy3 <- xx ^ 3
# yy <- mod[1] + xx * log(mod[2])
# yy <- mod[1] + xx * mod[2] + yy2 * mod[3] + yy3 * mod[4]
yy <- mod[1] + xx * mod[2] + yy2 * mod[3]
# yy <- mod[1] + xx * mod[2]

ADD <- -200
plot(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.5), col='blue', lty=3, type = 'l',
     xlim = c(100,(max(Corona.Country.Israel$Date.)+20)), ylim=c(0,YLIMIT))
lines(xx[DAYS[(length(DAYS)-20)]:(DAYS[length(DAYS)]+10)], yy[DAYS[(length(DAYS)-20)]:(DAYS[length(DAYS)]+10)]+ADD, lty=3, col='blue')
abline(v=DAYS[1])
# lines(xx, yy, lty=3, col='blue')



jpeg(paste0("plots/Israel.outbreak.parameters ", Sys.Date(), ".jpg"), width = 800, height = 500)




par(mar = c(8,5,5,5))
plot(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, col=0, las = 2, xlab="",
     xaxt = 'n', main = "Coronavirus outbreak parameters in Israel", ylab = "New Cases/Critical/Hospital",
     # yaxt='n', xlim = c((min(Corona.Country.Israel$Date.)),(max(Corona.Country.Israel$Date.)+10+20)),
     yaxt='n', xlim = c((300),(max(Corona.Country.Israel$Date.)+10)),
     ylim = c(0, 12000), cex=0.5)
grid()



mtext(text = "Ventilation/New Death", side = 4, line = 2)

# abline(h=max(yy[87:length(yy)]), lty=2, col='orange')
# abline(h=Corona.Country.Israel$New.Cases[length(Corona.Country.Israel$New.Cases)], lty=2, col='orange')

# lines(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, col='blue', lwd=1, lty=2)
# points(Corona.Country.Israel$Date.[length(Corona.Country.Israel$Date.)], Corona.Country.Israel$New.Cases[length(Corona.Country.Israel$Date.)], 
       # col='blue', cex=1, pch=16)

# lines(smooth.spline(Corona.Country.Israel$Date.[1:87], Corona.Country.Israel$New.Cases[1:87], spar = 0.7), col='blue', lty=3)
lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.5), col='blue', lty=1, lwd=3)
axis(2, col='blue', col.axis="blue", at = seq(0, 20000, 1000), labels = seq(0, 20000, 1000))
axis(1, at = min(Corona.Country.Israel$Date.):(max(Corona.Country.Israel$Date.)+10), las = 2,
     labels = format.Date(as.Date(min(as.Date(Corona.Country.Israel$Date, "%d %b %Y")):(max(as.Date(Corona.Country.Israel$Date, "%d %b %Y"))+10), origin = "1970-01-01"), "%d %b %Y"), cex.axis=0.8)


# yyy <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.7)
# yyy.y <- yyy$y[length(yyy$y)]
# yyy.x <- yyy$x[length(yyy$y)]
# segments(x0 = yyy.x, y0 = yyy.y, x1 = yyy.x+10, y1 = yyy.y + 10 * mod[2], lty=3, col='blue')


aa <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.5)$y
aa <- aa[length(aa)]
ADD <- aa - yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)][1]
# lines(xx[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)], yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)]+ADD, lty=3, col='blue', lwd=3)



Corona.Country.Israel$Critical. <- Corona.Country.Israel$Critical * sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Critical) * 0.7

# lines(Corona.Country.Israel$Date., Corona.Country.Israel$Critical., col='purple', lwd=1, lty=2)
# points(Corona.Country.Israel$Date.[length(Corona.Country.Israel$Date.)], Corona.Country.Israel$Critical.[length(Corona.Country.Israel$Date.)], 
#        col='purple', cex=1, pch=16)

# points(Corona.Country.Israel$Date., Corona.Country.Israel$Critical., col='purple', cex=0.5)
lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Critical., spar = 0.5), col='purple', lty=1, lwd=3)


yy <- smooth.spline(Corona.Country.Israel$Date.[(length(Corona.Country.Israel$Critical.)-35):length(Corona.Country.Israel$Critical.)], Corona.Country.Israel$Critical.[(length(Corona.Country.Israel$Critical.)-35):length(Corona.Country.Israel$Critical.)], spar = 0.5)
yy2 <- yy$x ^ 2
yy3 <- yy$x ^ 3
# mod <- lm(yy$y ~ yy$x + yy2 + yy3)
mod <- lm(yy$y ~ yy$x + yy2)
mod <- coef(summary(mod))[,1]
xx <- 1:(max(Corona.Country.Israel$Date.)+10)
yy2 <- xx ^ 2
yy3 <- xx ^ 3
# yy <- mod[1] + xx * mod[2] + yy2 * mod[3] + yy3 * mod[4]
yy <- mod[1] + xx * mod[2] + yy2 * mod[3]

aa <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Critical., spar = 0.5)$y
aa <- aa[length(aa)]
ADD <- aa - yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)][1]

# lines(xx[(length(xx)-11):length(xx)], yy[(length(xx)-11):length(xx)] + ADD, lty=3, col='purple', lwd=3)




Corona.Country.Israel$Hspital. <- Corona.Country.Israel$Hspital * sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Critical) * 0.7

# points(Corona.Country.Israel$Date., Corona.Country.Israel$Hspital., col='green', cex = 0.5)
# lines(Corona.Country.Israel$Date., Corona.Country.Israel$Hspital., col='green', lwd=1, lty=2)
lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Hspital., spar = 0.5), col='green', lty=1, lwd=3)
# points(Corona.Country.Israel$Date.[length(Corona.Country.Israel$Date.)], Corona.Country.Israel$Hspital.[length(Corona.Country.Israel$Date.)], 
       # col='green', cex=1, pch=16)

yy <- smooth.spline(Corona.Country.Israel$Date.[(length(Corona.Country.Israel$Hspital.)-50):length(Corona.Country.Israel$Hspital.)], 
                    Corona.Country.Israel$Hspital.[(length(Corona.Country.Israel$Hspital.)-50):length(Corona.Country.Israel$Hspital.)], spar = 0.5)
yy2 <- yy$x ^ 2
yy3 <- yy$x ^ 3
mod <- lm(yy$y ~ yy$x + yy2)
# mod <- lm(yy$y ~ yy$x + yy2 + yy3)
mod <- coef(summary(mod))[,1]
xx <- 1:(max(Corona.Country.Israel$Date.)+10)
yy2 <- xx ^ 2
yy3 <- xx ^ 3
yy <- mod[1] + xx * mod[2] + yy2 * mod[3]
# yy <- mod[1] + xx * mod[2] + yy2 * mod[3] + yy3 * mod[4]

aa <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Hspital., spar = 0.5)$y
aa <- aa[length(aa)]
ADD <- aa - yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)][1]
# lines(xx[(length(xx)-11):length(xx)], yy[(length(xx)-11):length(xx)] + ADD, lty=3, col='green', lwd=3)

axis(4, col='purple', col.axis=c("purple"), at = c(seq(300, 2400, 300)*sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Critical)) * 0.7, 
     labels = seq(300, 2400, 300), line = -45, lwd = 1)
axis(4, col='green', col.axis="green", at = c(seq(0, 2400, 300)*sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Critical)) * 0.7, 
     labels = seq(0, 2400, 300), line = -45, lty = 2)



Corona.Country.Israel$Ventilation. <- Corona.Country.Israel$Ventilation * sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Ventilation)

# points(Corona.Country.Israel$Date., Corona.Country.Israel$Ventilation., col='darkgoldenrod3', cex = 0.5)
# lines(Corona.Country.Israel$Date., Corona.Country.Israel$Ventilation., col='darkgoldenrod3', lwd=1, lty=2)
lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Ventilation., spar = 0.5), col='darkgoldenrod3', lty=1, lwd=3)
# points(Corona.Country.Israel$Date.[length(Corona.Country.Israel$Date.)], Corona.Country.Israel$Ventilation.[length(Corona.Country.Israel$Date.)], 
#        col='darkgoldenrod3', cex=1, pch=16)

yy <- smooth.spline(Corona.Country.Israel$Date.[(length(Corona.Country.Israel$Ventilation.)-50):length(Corona.Country.Israel$Ventilation.)], 
                    Corona.Country.Israel$Ventilation.[(length(Corona.Country.Israel$Ventilation.)-50):length(Corona.Country.Israel$Ventilation.)], spar = 0.5)
yy2 <- yy$x ^ 2
yy3 <- yy$x ^ 3
# mod <- lm(yy$y ~ yy$x + yy2 + yy3)
mod <- lm(yy$y ~ yy$x + yy2)
mod <- coef(summary(mod))[,1]
xx <- 1:(max(Corona.Country.Israel$Date.)+10)
yy2 <- xx ^ 2
yy3 <- xx ^ 3
# yy <- mod[1] + xx * mod[2] + yy2 * mod[3] + yy3 * mod[4]
yy <- mod[1] + xx * mod[2] + yy2 * mod[3]

aa <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Ventilation., spar = 0.5)$y
aa <- aa[length(aa)]
ADD <- aa - yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)][1]
# lines(xx[(length(xx)-11):length(xx)], yy[(length(xx)-11):length(xx)] + ADD, lty=3, col='darkgoldenrod3', lwd=3)

axis(2, col='darkgoldenrod3', col.axis=c("darkgoldenrod3"), at = c(seq(0, 800, 50)*sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Ventilation)), 
     labels = seq(0, 800, 50), line = -45, lwd = 1)




Corona.Country.Israel$Death <- Death.Israel.$New.Death

dth.isr <- Death.Israel.$New.Death
dth.isr. <- dth.isr

Corona.Country.Israel$Death. <- Corona.Country.Israel$Death * sd(Corona.Country.Israel$New.Cases) / sd(Corona.Country.Israel$Death) * 1.1

# points(Corona.Country.Israel$Date., dth.isr.., col='red', cex=0.5)
# lines(Corona.Country.Israel$Date.[dth.isr.. < 10000], dth.isr..[dth.isr.. < 10000], col='red', lwd=1, lty=2)
lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Death., spar = 0.5), col='red', lty=1, lwd=3)
# points(Corona.Country.Israel$Date.[length(Corona.Country.Israel$Date.)], dth.isr..[length(Corona.Country.Israel$Date.)], 
#        col='red', cex=1, pch=16)

yy <- smooth.spline(Corona.Country.Israel$Date.[Corona.Country.Israel$Date. > 230], Corona.Country.Israel$Death.[Corona.Country.Israel$Date. > 230], spar = 0.5)
yy2 <- yy$x ^ 2
yy3 <- yy$x ^ 3
# mod <- lm(yy$y ~ yy$x + yy2 + yy3)
mod <- lm(yy$y ~ yy$x + yy2)
mod <- coef(summary(mod))[,1]
xx <- 1:(max(Corona.Country.Israel$Date.)+10)
yy2 <- xx ^ 2
yy3 <- xx ^ 3
# yy <- mod[1] + xx * mod[2] + yy2 * mod[3] + yy3 * mod[4]
yy <- mod[1] + xx * mod[2] + yy2 * mod[3]

aa <- smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$Death., spar = 0.5)$y
aa <- aa[length(aa)]
ADD <- aa - yy[DAYS[(length(DAYS)-1)]:(DAYS[length(DAYS)]+10)][1]
# lines(xx[(length(xx)-11):length(yy)], yy[(length(xx)-11):length(yy)] + ADD, lty=3, col='red', lwd=3)




axis(4, col='red', col.axis=c("red"), at = c(seq(0, 100, 5)*sd(Corona.Country.Israel$New.Cases) / sd(dth.isr.)) * 1.1, 
     labels = seq(0, 100, 5), line = 0, lwd = 1)

rasterImage(statistx,
            xleft=450, xright=490,
            ybottom=2000, ytop=3500)
text(175, 6500, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))


legend(x = 450, y = 10000, legend = c("New Cases", "New Death", "Severe", "Hospital", "Ventilation"),
       col = c("blue", "red", "purple", "green", "darkgoldenrod3"), 
       lty=c(1, 1, 1, 1, 1), lwd = c(rep(3, 5)), cex=0.9)



text(x = 490, y = 5000, labels = paste0("Last week average: ", round(mean(dth.isr[(length(dth.isr)-6):length(dth.isr)]),1), " death per day\nand ",
                                        round(mean(Corona.Country.Israel$New.Cases[(length(Corona.Country.Israel$New.Cases)-6):length(Corona.Country.Israel$New.Cases)])),
                                        " new cases per day"))



dev.off()
















Corona.Country.Israel <- Corona.Country.Israel[Corona.Country.Israel$Date. >= 0,]

Corona.Country.Israel$New.Cases <- 0
for (i in 2:length(Corona.Country.Israel$Country)) {
        Corona.Country.Israel$New.Cases[i] <- Corona.Country.Israel$value[i] - Corona.Country.Israel$value[i-1]
}


Death.Israel. <- melt(data = Death.Israel, id.vars = "Country", measure.vars = names(Death.Israel)[2:length(Death.Israel)])

Death.Israel.$New.Death <- 0
for (i in 2:length(Death.Israel.$Country)) {
        Death.Israel.$New.Death[i] <- Death.Israel.$value[i] - Death.Israel.$value[i-1]
}




jpeg(paste0("plots/Israel.outbreak ", Sys.Date(), ".jpg"), width = 1000, height = 500)




par(mar = c(8,5,5,5))
plot(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, col='blue', las = 2, xlab="",
     xaxt = 'n', main = "Coronavirus outbreak in Israel", ylab = "New Cases",
     yaxt='n', xlim = c(1,(max(Corona.Country.Israel$Date.))),
     ylim = c(0, YLIMIT), cex=0.5)
grid()



# lines(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, col='blue', lwd=1, lty=3)
# lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.5), col='blue', lty=3, lwd=5)
axis(2, col='blue')
axis(1, at = Corona.Country.Israel$Date., las = 2,
     labels = Corona.Country.Israel$Date, cex.axis=0.8)
mtext(text = "New Death", side = 4, line=2)


# lines(xx, yy, lty=3, col='blue')

# abline(v=15, col='green', lty=2)
#
# abline(v=48, col='orange', lty=2)


legend('topleft', legend = c("New Cases", "New Death"),
       col = c("blue", "red"), lty=c(1, 1))

dth.isr <- Death.Israel.$New.Death
dth.isr. <- (dth.isr*sd(Corona.Country.Israel$New.Cases) / sd(dth.isr)) * 0.7




points(Corona.Country.Israel$Date., dth.isr., col='red', cex=0.5)
# lines(Corona.Country.Israel$Date., dth.isr., col='red', lwd=1, lty=3)
# lines(smooth.spline(Corona.Country.Israel$Date., dth.isr., spar = 0.5), col='red', lty=3, lwd=5)

# lines(xx, yy, lty=3, col='red')

axis(side = 4, at = seq(0,200,10)*(sd(Corona.Country.Israel$New.Cases) / sd(dth.isr)) * 0.7, labels = seq(0,200,10), col='red')


lines(smooth.spline(Corona.Country.Israel$Date., Corona.Country.Israel$New.Cases, spar = 0.5), col='blue', lty=1, lwd=5)
lines(smooth.spline(Corona.Country.Israel$Date., dth.isr., spar = 0.5), col='red', lty=1, lwd=5)

rasterImage(statistx,
            xleft=20, xright=80,
            ybottom=6000, ytop=9000)
text(50, 5000, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))

dev.off()












# Outbreak All Curve ----------------------------------------------------------

Corona.Country. <- melt(data = Corona.Country, id.vars = "Country", measure.vars = names(Corona.Country)[2:length(Corona.Country)], variable_name = "Date")
Corona.Country.$value[is.na(Corona.Country.$value)] <- 0
Corona.Country.$Date <- Corona.Country.$variable
Corona.Country.$Date. <- as.character(Corona.Country.$Date)
Corona.Country.$Date. <- as.Date(Corona.Country.$Date., "%d %b %Y")



Top.Countries <- Corona.Country.[order(-Corona.Country.$value),]
Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
Top.Countries <- Top.Countries$Country

Outbreak.Curve <- Corona.Country.[Corona.Country.$Country %in% Top.Countries,]
Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
Outbreak.Curve$Change <- 0
for (i in 2:length(Outbreak.Curve$Country)) {
        if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
                Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
        }
}


Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000


List.Countries <- "Israel"
for (i in 1:length(Top.Countries)) {
        if (max(Outbreak.Curve$value[Outbreak.Curve$Country == Top.Countries[i]]) > 20000) {
                List.Countries <- c(List.Countries, Top.Countries[i])
        }
}
List.Countries <- unique(List.Countries)

Outbreak.Curve. <- Outbreak.Curve[Outbreak.Curve$Country %in% List.Countries,]

# Outbreak.Curve. <- Outbreak.Curve
Top.Countries. <- List.Countries

Outbreak.Curve. <- Outbreak.Curve.[order(Outbreak.Curve.$Country, Outbreak.Curve.$Date.),]
Outbreak.Curve.$Smooth <- NA
for (i in 1:(length(unique(Outbreak.Curve.$Country)))) {
        if (sum(!is.na(Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]]))) {
                Outbreak.Curve.$Smooth[Outbreak.Curve.$Country == Top.Countries.[i]] <- 
                        smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                                      Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7)$y
        }
        
}

aa <- Outbreak.Curve.[Outbreak.Curve.$Date. == (Sys.Date()-1), c("Country", "Smooth")]
if (length(aa$Country) == 0) {
        aa <- Outbreak.Curve.[Outbreak.Curve.$Date. == (Sys.Date()-2), c("Country", "Smooth")]
}
aa <- aa[order(-aa$Smooth),]
names(aa)[2] <- "New Cases/1M"
ISRAEL.OUTBREAK.RANK <- (1:length(aa$Country))[aa$Country == "Israel"]
TOP.10.OUTBREAK.RANK <- aa[1:10,]


jpeg(paste0("plots/outbreak curve.per.1M.All ", Sys.Date(), ".jpg"), width = 1200, height = 1200)


WORLD <- aggregate(Corona.Country.$value, list(Corona.Country.$Date.), sum, na.rm=T)
WORLD$Change <- 0
for (i in 2:length(WORLD$Group.1)) {
        WORLD$Change[i] <- WORLD$x[i] - WORLD$x[i-1]
}
WORLD$Change <- (WORLD$Change / 7800000000) * 1000000



i <- 1
tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
              "\nPlot shows population adjusted epidemiological curves of COVID-19\nThe epidemiological curves are smoothed to emphasis the curve trend")
layout(matrix(c(2,1)), 0.4, 2)
par(mar = c(4, 4, 4, 4))
textplot(tex, halign = "center", cex = 1)

plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                   Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.5), 
     col = 1, lwd=1, type='l', ylim=c(0,1400),  lty=1,
     ylab="New Cases per 1M", xlab="", main="Epidemiological Curve per 1M for all countries", xaxt = 'n')


for (i in 2:(length(unique(Outbreak.Curve.$Country)))) {
        if (sum(!is.na(Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]]))) {
                lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
                      col = 1, lwd=1)
        }
        
}
lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == "Israel"], 
                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == "Israel"], spar = 0.5),
      col = 'red', lwd=5)
axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
     labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)


lines(smooth.spline(WORLD$Group.1, WORLD$Change, spar = 0.5),
      col = 'purple', lwd=5)

legend("topleft", legend = c("Israel", "World"), lty=1, lwd=2, col=c('red', 'purple'))

rasterImage(statistx, 
            xleft=min(Outbreak.Curve.$Date.), min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25, 
            ybottom=400, ytop=600)


dev.off()







# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Liechtenstein", "France", "Belgium", "Singapore",
#                                                                  "Italy", "Switzerland", "Spain", "Germany", "Israel", "Iran"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# jpeg(paste0("plots/outbreak curve.per.1M.rising ", Sys.Date(), ".jpg"), width = 800, height = 800)
# 
# i <- 1
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with active outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(matrix(c(2,1)), 0.4, 2)
# par(mar = c(4, 4, 4, 4))
# textplot(tex, halign = "center", cex = 1)
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,400),
#      xlim=as.numeric(c(as.Date("2020-02-15"), max(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]]))), 
#      ylab="New Cases per 1M", xlab="", main="Epidemiological Curve per 1M for selected countries\nRising Curve", xaxt = 'n')
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)
# 
# rasterImage(statistx, 
#             xleft=as.Date("2020-02-15"), as.Date("2020-02-15") + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.20, 
#             ybottom=max(max(Outbreak.Curve.$Change))*(0.15), ytop=max(max(Outbreak.Curve.$Change))*0.20)
# 
# 
# dev.off()



# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Korea, South", "Liechtenstein", "France", "Sweden", 
#                                                                  "Turkey", "Italy", "Switzerland"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# jpeg(paste0("plots/outbreak curve.per.1M.rising ", Sys.Date(), ".jpg"), width = 800, height = 1200)
# 
# # par(mfrow=c(2,1))
# 
# i <- 1
# tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
#               "\nPlot shows population adjusted epidemiological curves of COVID-19\nfor countries with active outbreak.\nThe epidemiological curves are smoothed to emphasis the curve trend")
# layout(mat = matrix(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4), ncol = 3), widths = 1, heights = c(2,0.5,2,1))
# layout.show(n = 4)
# par(mar = c(6, 6, 6, 6))
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,max(Outbreak.Curve.$Change)*0.9), 
#      ylab="New Cases per 1M", xlab="", main="Outbreak Curve per 1M for selected countries\nRising Curve", xaxt = 'n',
#      xlim = c(as.Date("2020-03-10"), max(Outbreak.Curve.$Date.)), cex.main=2, cex.lab=2, cex.axis=2)
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2, cex.axis=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color, cex=2)
# 
# rasterImage(statistx, 
#             xleft=as.numeric(as.Date("2020-03-10"))*1.0004, as.numeric(as.Date("2020-03-10"))*1.0004 + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.05, 
#             ybottom=max(Outbreak.Curve.$Change)*(0.65), ytop=max(Outbreak.Curve.$Change)*0.83)
# 
# 
# par(mar = c(1, 1, 1, 1))
# textplot(" ", halign = "center", cex = 2)
# 
# Outbreak.Curve. <- Outbreak.Curve[!Outbreak.Curve$Country %in% c("China", "Korea, South", "Liechtenstein", "Sweden", "Italy", "Spain", "Switzerland",
#                                                                  "Turkey"),]
# Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
# 
# i <- 1
# 
# par(mar = c(6, 6, 6, 6))
# 
# plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
#      col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,max(Outbreak.Curve.$Change)*0.7), 
#      ylab="New Cases per 1M", xlab="", main="Outbreak Curve per 1M for selected countries\nMild Rising Curve", xaxt = 'n',
#      xlim = c(as.Date("2020-03-10"), max(Outbreak.Curve.$Date.)), cex.main=2, cex.lab=2, cex.axis=2)
# # lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# # lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
# #                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
# #       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)
# 
# for (i in 2:length(Top.Countries.)) {
#         lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                             Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#               col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
# }
# axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#      labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2, cex.axis=2)
# Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
# legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color, cex=2)
# 
# 
# textplot(tex, halign = "center", cex = 2)
# 
# dev.off()








# Death All Curve ----------------------------------------------------------

Death.Country. <- melt(data = Death.Country, id.vars = "Country", measure.vars = names(Death.Country)[2:length(Death.Country)], variable_name = "Date")
Death.Country.$value[is.na(Death.Country.$value)] <- 0
Death.Country.$Date <- Death.Country.$variable
Death.Country.$Date. <- as.character(Death.Country.$Date)
Death.Country.$Date. <- as.Date(Death.Country.$Date., "%d %b %Y")



Top.Countries <- Death.Country.[order(-Death.Country.$value),]
Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
Top.Countries <- Top.Countries$Country

Outbreak.Curve <- Death.Country.[Death.Country.$Country %in% Top.Countries,]
Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
Outbreak.Curve$Change <- 0
for (i in 2:length(Outbreak.Curve$Country)) {
        if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
                Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
        }
}


Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000


List.Countries <- "Israel"
for (i in 1:length(Top.Countries)) {
        if (max(Outbreak.Curve$value[Outbreak.Curve$Country == Top.Countries[i]]) > 300) {
                List.Countries <- c(List.Countries, Top.Countries[i])
        }
}
List.Countries <- unique(List.Countries)

Outbreak.Curve. <- Outbreak.Curve[Outbreak.Curve$Country %in% List.Countries,]

# Outbreak.Curve. <- Outbreak.Curve


Outbreak.Curve. <- Outbreak.Curve.[order(Outbreak.Curve.$Country, Outbreak.Curve.$Date.),]
Outbreak.Curve. <- Outbreak.Curve.[!is.na(Outbreak.Curve.$Change),]

Top.Countries. <- unique(Outbreak.Curve.$Country)
Top.Countries. <- Top.Countries.[!Top.Countries. %in% "Kosovo"]

Outbreak.Curve.$Smooth <- NA
for (i in 1:(length(unique(Outbreak.Curve.$Country)))) {
        Outbreak.Curve.$Smooth[Outbreak.Curve.$Country == Top.Countries.[i]] <-
                smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]],
                              Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7)$y
}


aa <- Outbreak.Curve.[Outbreak.Curve.$Date. == (Sys.Date()-1), c("Country", "Smooth")]
if (length(aa$Country) == 0) {
        aa <- Outbreak.Curve.[Outbreak.Curve.$Date. == (Sys.Date()-2), c("Country", "Smooth")]
}
aa <- aa[order(-aa$Smooth),]
names(aa)[2] <- "New Cases/1M"
ISRAEL.DEATH.RANK <- (1:length(aa$Country))[aa$Country == "Israel"]
TOP.10.DEATH.RANK <- aa[1:10,]


WORLD <- aggregate(Outbreak.Curve.$value, list(Outbreak.Curve.$Date.), sum, na.rm=T)
WORLD$Change <- 0
for (i in 2:length(WORLD$Group.1)) {
        WORLD$Change[i] <- WORLD$x[i] - WORLD$x[i-1]
}
WORLD$Change <- (WORLD$Change / 7800000000) * 1000000



jpeg(paste0("plots/Death curve.per.1M.All ", Sys.Date(), ".jpg"), width = 1200, height = 1200)

i <- 1
tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
              "\nPlot shows population adjusted new death curves of COVID-19\nThe epidemiological curves are smoothed to emphasis the curve trend")
layout(matrix(c(2,1)), 0.4, 2)
par(mar = c(4, 4, 4, 4))
textplot(tex, halign = "center", cex = 1)

plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                   Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
     col = 1, lwd=1, type='l', ylim=c(0,30),  lty=1,
     ylab="New Death per 1M", xlab="", main="New Death Curve per 1M for all countries", xaxt = 'n')


for (i in 2:(length(unique(Outbreak.Curve.$Country)))) {
        lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                            Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
              col = 1, lwd=1)
}
lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == "Israel"], 
                    Outbreak.Curve.$Change[Outbreak.Curve.$Country == "Israel"], spar = 0.7),
      col = 'red', lwd=3)
axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
     labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)

lines(smooth.spline(WORLD$Group.1, WORLD$Change, spar = 0.5),
      col = 'purple', lwd=5)

legend("topleft", legend = c("Israel", "World"), lty=1, lwd=2, col=c('red', 'purple'))


rasterImage(statistx, 
            xleft=min(Outbreak.Curve.$Date.), min(Outbreak.Curve.$Date.) + length(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]])*0.25, 
            ybottom=20, ytop=24)


dev.off()













# Death Outbreak Curve selected ----------------------------------------------------------

Death.Country. <- melt(data = Death.Country, id.vars = "Country", measure.vars = names(Death.Country)[2:length(Death.Country)], variable_name = "Date")
Death.Country.$value[is.na(Death.Country.$value)] <- 0
Death.Country.$Date. <- as.Date(as.character(Death.Country.$variable), "%d %b %Y")



Top.Countries <- Death.Country.[order(-Death.Country.$value),]
Top.Countries <- Top.Countries[!duplicated(Top.Countries$Country),]
Top.Countries <- unique(c(Top.Countries$Country))

Outbreak.Curve <- Death.Country.[Death.Country.$Country %in% Top.Countries,]
Outbreak.Curve <- Outbreak.Curve[order(Outbreak.Curve$Country, Outbreak.Curve$Date.),]
Outbreak.Curve$Change <- 0
for (i in 2:length(Outbreak.Curve$Country)) {
        if (Outbreak.Curve$Country[i] == Outbreak.Curve$Country[i-1]) {
                Outbreak.Curve$Change[i] <- Outbreak.Curve$value[i] - Outbreak.Curve$value[i-1]
        }
}


Outbreak.Curve <- merge(Outbreak.Curve, pop, "Country", all.x = T)
Outbreak.Curve$Change <- (Outbreak.Curve$Change / Outbreak.Curve$Population) * 1000000

Colors <- data.frame(Country = c("Italy", "China", "Iran", "Chile", "France", "Switzerland", "US",
                                 "Bahrain", "Israel", "Germany", "United Kingdom", "Uruguay", "Brazil", "Russia",
                                 "Hungary", "Belarus", "Qatar", "Singapore"), 
                     Color = c("bisque4", "light green", "cornflowerblue", "cyan", "magenta", "dark gray", "black", "red", "orange",
                                                      "dark red", "brown", "red", "blue", "pink", "darkmagenta", 
                                                      "chartreuse3", "darkgoldenrod1", "goldenrod4"))
Colors$Color <- as.character(Colors$Color)

# , "green"

# Outbreak.Curve <- Outbreak.Curve[Outbreak.Curve$Country %in% c("China", "Italy", "Spain", "Iran", "Bahrain", "Israel"),]
# Top.Countries <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve$Country)]


Outbreak.Curve. <- Outbreak.Curve[Outbreak.Curve$Country %in% c("France", "Uruguay", "US", "United Kingdom", "Brazil",
                                                                "Italy", "Chile", "Germany", "Israel", "Hungary", "Switzerland"),]
Top.Countries. <- Top.Countries[Top.Countries %in% unique(Outbreak.Curve.$Country)]
jpeg(paste0("plots/Death.outbreak curve.per.1M.", Sys.Date(), ".jpg"), width = 800, height = 800)

i <- 1
tex <- paste0("Plot Produced by StatistX at ", format.Date(Sys.time(), "%d %B %Y %H:%M"), 
              "\nPlot shows population adjusted new death curves of COVID-19\nThe epidemiological curves are smoothed to emphasis the curve trend")
layout(matrix(c(2,1)), 0.4, 2)
par(mar = c(4, 4, 4, 4))
textplot(tex, halign = "center", cex = 1)

plot(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                   Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7), 
     col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2, type='l', ylim=c(0,30), 
     ylab="New Death per 1M", xlab="", main="New Death Curve per 1M for selected countries", xaxt = 'n')
# lines(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#       Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], 
#       col = Colors$Color[Colors$Country == Top.Countries.[i]])
# lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
#                     Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
#       col = Colors$Color[Colors$Country == Top.Countries.[i]], lty=2, lwd=4)

for (i in 2:length(Top.Countries.)) {
        lines(smooth.spline(Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
                            Outbreak.Curve.$Change[Outbreak.Curve.$Country == Top.Countries.[i]], spar = 0.7),
              col = Colors$Color[Colors$Country == Top.Countries.[i]], lwd=2)
}
axis(side = 1, at = Outbreak.Curve.$Date.[Outbreak.Curve.$Country == Top.Countries.[i]], 
     labels = Outbreak.Curve.$Date[Outbreak.Curve.$Country == Top.Countries.[i]], las=2)
Colors.Legend <- Colors[Colors$Country %in% unique(Outbreak.Curve.$Country),]
legend("topleft", legend = Colors.Legend$Country, lty=1, lwd=2, col=Colors.Legend$Color)

rasterImage(statistx, 
            xleft=18414, xright = 18474, 
            ybottom=15, 20)


dev.off()



# VAERS -------------------------------------------------------------------

# DATA2020 <- read.csv("2020VAERSData/2020VAERSDATA.csv")
# VAX2020 <- read.csv("2020VAERSData/2020VAERSVAX.csv")
# DATA2021 <- read.csv("2021VAERSData/2021VAERSDATA.csv")
# VAX2021 <- read.csv("2021VAERSData/2021VAERSVAX.csv")
# 
# DEATH <- rbind(DATA2020, DATA2021)
# DEATH <- DEATH[DEATH$DIED == "Y", c("VAERS_ID", "RECVDATE", "AGE_YRS", "SEX", "VAX_DATE", "ONSET_DATE", "DATEDIED")]
# 
# VAX <- rbind(VAX2020, VAX2021)
# 
# DEATH <- merge(DEATH, VAX, "VAERS_ID", all.x = T)
# table(DEATH$VAX_TYPE)
# 
# DEATH$FLU <- grepl(pattern = "FLU", DEATH$VAX_TYPE)
# DEATH$COVID <- DEATH$VAX_TYPE == "COVID19"
# 
# sum(DEATH$FLU & substr(DEATH$RECVDATE, 7, 10) == "2020")
# summary(DEATH$AGE_YRS[DEATH$FLU & substr(DEATH$RECVDATE, 7, 10) == "2020"])
# 
# sum(DEATH$COVID)
# summary(DEATH$AGE_YRS[DEATH$COVID])
# 
# COVID <- DEATH[DEATH$COVID,]
# COVID$ONSET_DATE <- as.Date(COVID$ONSET_DATE, "%m/%d/%Y")
# COVID$VAX_DATE <- as.Date(COVID$VAX_DATE, "%m/%d/%Y")
# COVID$DATEDIED <- as.Date(COVID$DATEDIED, "%m/%d/%Y")
# COVID$ONSET_DATE[is.na(COVID$ONSET_DATE)] <- COVID$DATEDIED[is.na(COVID$ONSET_DATE)]
# COVID$Time <- as.numeric(COVID$ONSET_DATE - COVID$VAX_DATE)
# # COVID <- COVID[COVID$Time < 60,]
# COVID.NN <- length(COVID$VAERS_ID)
# COVID$Time[COVID$Time < 0] <- NA
# 
# NO.COVID <- DEATH[!DEATH$COVID,]
# NO.COVID$ONSET_DATE <- as.Date(NO.COVID$ONSET_DATE, "%m/%d/%Y")
# NO.COVID$VAX_DATE <- as.Date(NO.COVID$VAX_DATE, "%m/%d/%Y")
# NO.COVID$DATEDIED <- as.Date(NO.COVID$DATEDIED, "%m/%d/%Y")
# NO.COVID$ONSET_DATE[is.na(NO.COVID$ONSET_DATE)] <- NO.COVID$DATEDIED[is.na(NO.COVID$ONSET_DATE)]
# NO.COVID$Time <- as.numeric(NO.COVID$ONSET_DATE - NO.COVID$VAX_DATE)
# # NO.COVID <- NO.COVID[NO.COVID$Time < 60,]
# NO.COVID.NN <- length(NO.COVID$VAERS_ID)
# NO.COVID$Time[NO.COVID$Time < 0] <- NA
# 
# jpeg(paste0("plots/Age.Death.Vaccinations ", Sys.Date(), ".jpeg"), width = 800, height = 500)
# hist(COVID$AGE_YRS, main=paste0("Age distribution of peaple\nwho died after Vaccination in USA (2020-2021)"),
#      xlab="Age (Years)", col='lightblue')
# hist(NO.COVID$AGE_YRS, add=T, col='pink', breaks = 20)
# legend('topleft', legend = c(paste("Covid-19 (", COVID.NN, " Deaths)"), paste0("Other Vacinations (", NO.COVID.NN, " Deaths)")),
#        fill = c('lightblue', 'pink'))
# dev.off()
# 
# COVID$Time[COVID$Time < 0] <- NA
# COVID$Time[COVID$Time > 1000] <- NA
# 
# jpeg(paste0("plots/Age.Death.days.Vaccinations ", Sys.Date(), ".jpeg"), width = 800, height = 500)
# hist(COVID$Time, main=paste0("Days from vaccination to death\nUSA (2020-2021)"),
#      xlab = "Days from vaccination", breaks = 200, col='lightblue', xlim=c(0,60))
# hist(NO.COVID$Time, breaks = 1000, col='pink', add=T)
# legend('topright', legend = c(paste("Covid-19 (", COVID.NN, " Deaths)"), paste0("Other Vacinations (", NO.COVID.NN, " Deaths)")),
#        fill = c('lightblue', 'pink'))
# dev.off()
# 
# sort(COVID$Time)
# 
# 
# 
# 1

# Vaccinations and Epidemic curves by age ---------------------------------

# VACCINATIONS <- read.csv("vaccinated-per-day-2021-03-02.csv")
# unique(VACCINATIONS$age_group)
# VACCINATIONS$Age.Group <- NA
# VACCINATIONS$Age.Group[VACCINATIONS$age_group %in% c("0-19", "20-29", "30-39", "40-49", "50-59")] <- "0-59"
# VACCINATIONS$Age.Group[VACCINATIONS$age_group %in% c("60-69", "70-79", "80-89", "90+")] <- "60+"
# names(VACCINATIONS)[1] <- "Date"
# VACCINATIONS$Date <- as.Date(VACCINATIONS$Date)
# VACCINATIONS$second_dose[VACCINATIONS$second_dose == "<15"] <- 8
# VACCINATIONS$second_dose <- as.numeric(VACCINATIONS$second_dose)
# VACCINATIONS <- aggregate(VACCINATIONS$second_dose, list(VACCINATIONS$Date, VACCINATIONS$Age.Group), sum, na.rm = T)
# names(VACCINATIONS) <- c("Date", "Age.Group", "Vaccinated")
# 
# VACCINATIONS$Vaccinated.total <- 0
# for (i in 2:length(VACCINATIONS$Date)) {
#         if (VACCINATIONS$Age.Group[i] == VACCINATIONS$Age.Group[i-1]) {
#                 VACCINATIONS$Vaccinated.total[i] <- VACCINATIONS$Vaccinated.total[i-1] + VACCINATIONS$Vaccinated[i]
#         }
# }
# 
# 
# AGE <- read.csv("corona_age_and_gender_ver_0087.csv")
# names(AGE)[2] <- "Date"
# AGE$weekly_cases[AGE$weekly_cases == "<15"] <- 8
# AGE$weekly_cases <- as.numeric(AGE$weekly_cases)
# 
# AGE$weekly_deceased[AGE$weekly_deceased == "<15"] <- 8
# AGE$weekly_deceased <- as.numeric(AGE$weekly_deceased)
# 
# 
# AGE$Age.Group.1 <- AGE$age_group
# AGE$Age.Group.1[AGE$age_group %in% c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")] <- "0-59"
# AGE$Age.Group.1[AGE$age_group %in% c("60-64", "65-69", "70-74", "75-79", "80+")] <- "60+"
# 
# AGE.CASE <- aggregate(AGE$weekly_cases, list(AGE$Date, AGE$Age.Group.1), sum, na.rm = T)
# AGE.DEATH <- aggregate(AGE$weekly_deceased, list(AGE$Date, AGE$Age.Group.1), sum, na.rm = T)
# 
# AGE <- cbind(AGE.CASE, AGE.DEATH[3])
# 
# names(AGE) <- c("Date", "Age.Group", "Cases", "Death")
# AGE <- AGE[AGE$Age.Group != "NULL",]
# 
# unique(AGE$Age.Group)
# 
# ISR.POP <- read.csv("israel population.csv")
# ISR.POP$Age[ISR.POP$Age == "95+"] <- "95"
# ISR.POP$Age <- as.numeric(ISR.POP$Age)
# ISR.POP$Age.Group <- NA
# ISR.POP$Age.Group[ISR.POP$Age %in% 0:59] <- "0-59"
# ISR.POP$Age.Group[ISR.POP$Age %in% 60:99] <- "60+"
# 
# ISR.POP$pop <- as.numeric(gsub(",", "", ISR.POP$pop))
# ISR.POP <- aggregate(ISR.POP$pop, list(ISR.POP$Age.Group), sum, na.rm = T)
# names(ISR.POP) <- c("Age.Group", "Pop")
# 
# 
# AGE$Age.Group. <- as.numeric(factor(AGE$Age.Group))
# AGE$Date <- as.Date(AGE$Date)
# 
# 
# AGE <- merge(AGE, ISR.POP, "Age.Group")
# 
# AGE$Incidence <- (AGE$Cases / AGE$Pop) * 10000
# AGE$Mortality <- (AGE$Death / AGE$Pop) * 1000000
# AGE$Fatality <- (AGE$Death / AGE$Cases) * 100
# 
# AGE <- merge(AGE, VACCINATIONS, c("Date", "Age.Group"), all.x = T)
# 
# AGE$Percent.Vaccinated <- 100 * AGE$Vaccinated.total / AGE$Pop
# AGE$Percent.Vaccinated[is.na(AGE$Percent.Vaccinated)] <- 0
# 
# jpeg(paste0("plots/vac.and.cases.by.age", Sys.Date(), ".jpeg"), width = 900, height = 500)
# par(mar = c(8,5,5,5))
# plot(AGE$Date, as.numeric(AGE$Date), ylim=c(0, 100), col=0, ylab="%", xaxt='n', xlab='',
#      main="Vaccinations effect by Age Group", xlim=c(AGE$Date[1], max(AGE$Date)))
# 
# lines(AGE$Date[AGE$Age.Group. == 1], AGE$Fatality[AGE$Age.Group. == 1], col = 2, lty=1, lwd=2)
# lines(AGE$Date[AGE$Age.Group. == 1], AGE$Percent.Vaccinated[AGE$Age.Group. == 1], col = 2, lty=2, lwd=2)
# lines(AGE$Date[AGE$Age.Group. == 2], AGE$Fatality[AGE$Age.Group. == 2], col = 4, lty=1, lwd=2)
# lines(AGE$Date[AGE$Age.Group. == 2], AGE$Percent.Vaccinated[AGE$Age.Group. == 2], col = 4, lty=2, lwd=2)
# # for (i in 1:2) {
# #         # lines(AGE$Date[AGE$Age.Group. == i], AGE$Incidence[AGE$Age.Group. == i], col = 2, lty=i, lwd=2)
# #         # lines(AGE$Date[AGE$Age.Group. == i], AGE$Mortality[AGE$Age.Group. == i], col = 3, lty=i, lwd=2)
# #         lines(AGE$Date[AGE$Age.Group. == i], AGE$Fatality[AGE$Age.Group. == i], col = 2, lty=i, lwd=2)
# #         lines(AGE$Date[AGE$Age.Group. == i], AGE$Percent.Vaccinated[AGE$Age.Group. == i], col = 4, lty=i, lwd=2)
# # }
# 
# 
# 
# axis(1, at = AGE$Date, labels = format.Date(AGE$Date, "%d-%b-%Y"), las = 2)
# 
# legend(AGE$Date[1],100, legend = c("0-59 Vaccinated", "60+ Vaccinated", "0-59 Fatality", "60+ Fatality"), lty = c(2,2,1,1), col = c(2,4,2,4), lwd=2)
# 
# 
# START <- min(as.numeric(AGE$Date))
# rasterImage(statistx,
#             xleft=START, xright=START+30,
#             ybottom=35, ytop=50)
# text(START+15, 30, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()


# Vaccinations ------------------------------------------------------------


# VAC <- read.csv(url(paste0("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")), stringsAsFactors = F)
# 
# VAC$date <- as.Date(VAC$date)
# 
# VAC.TOP <- VAC[order(VAC$location, -VAC$people_vaccinated_per_hundred),]
# VAC.TOP <- VAC.TOP[!VAC.TOP$location %in% c("England", "Scotland", "Wales", "Northern Ireland", "North America"),]
# VAC.TOP <- VAC.TOP[VAC.TOP$total_vaccinations > 500000 & !is.na(VAC.TOP$total_vaccinations),]
# VAC.TOP <- VAC.TOP[!duplicated(VAC.TOP$location),]
# VAC.TOP <- VAC.TOP[order(-VAC.TOP$people_vaccinated_per_hundred),]
# VAC.TOP <- VAC.TOP$location[1:10]
# 
# Countries <- as.data.frame(table(VAC$location))
# Countries <- as.character(Countries[Countries$Freq > 1 | Countries$Var1 %in% VAC.TOP, "Var1"])
# 
# 
# Countries <- Countries[(Countries %in% c("Germany", "Italy", "France")) | Countries %in% VAC.TOP]
# Countries <- Countries[(!Countries %in% c("Israel"))]
# # Colors <- c("bisque4", "light green", "cornflowerblue", "cyan", "magenta", "dark gray", "red", "orange",
# #             "dark red", "brown", "red", "pink", "darkmagenta", 
# #             "darkorchid4", "darkgoldenrod1", "goldenrod4", "coral")
# Colors <- c("deeppink2", "darkturquoise", "darksalmon", "darkred", 
#             "darkorchid1", "darkorange4", "darkorange1", "darkolivegreen4", "darkkhaki",
#             "darkgrey", "darkgoldenrod4", "coral4", "darkgreen", "brown1", "darkgoldenrod1")
# START <- min(VAC$date)
# 
# END <- max(VAC$date)
# 
# VAC$DATE <- format.Date(VAC$date, "%d %b %Y")
# 
# jpeg(paste0("plots/vaccinations ", Sys.Date(), ".jpg"), width = 800, height = 500)
# 
# 
# par(mar = c(7, 5, 5, 5))
# plot(c(START, END), c(0, 0), col=0, ylim=c(15,100), 
#      xaxt = 'n', xlab = '', ylab = "Poeple Vaccinated per 100",
#      main = "Poeple Vaccinated around the world", xlim=c(START+150, END+10))
# grid()
# points(VAC$date[VAC$location == "World"], VAC$people_vaccinated_per_hundred[VAC$location == "World"], col = "Black", cex=1.5,  pch=19)
# lines(VAC$date[VAC$location == "World"], VAC$people_vaccinated_per_hundred[VAC$location == "World"], col = "Black", lwd=2)
# text(END+5, max(VAC$people_vaccinated_per_hundred[VAC$location == "World"]) + runif(1, -1, 1),"World", col=1, cex=0.8)
# axis(1, at = START:END, labels = format.Date(as.Date(as.numeric(START:END), "1970-01-01"), "%d %b %Y"), las=2)
# 
# 
# for (i in 1:length(Countries)) {
#         DATES <- VAC$date[VAC$location == Countries[i]][!is.na(VAC$people_vaccinated_per_hundred[VAC$location == Countries[i]])]
#         VALUE <- VAC$people_vaccinated_per_hundred[!is.na(VAC$people_vaccinated_per_hundred) & VAC$location == Countries[i]]
#         points(DATES, VALUE, col = Colors[i], pch=19)
#         lines(DATES, VALUE, col = Colors[i])
#         text(END+5, max(VALUE) + runif(1, -1, 1), Countries[i], col=Colors[i], cex=0.8)
# }
# 
# VAC$Label <- paste0(VAC$location, " (", round(VAC$people_vaccinated_per_hundred,1), "%)")
# 
# points(VAC$date[VAC$location == "Israel"], VAC$people_vaccinated_per_hundred[VAC$location == "Israel"], col = "Blue", cex=1.5, pch=19)
# lines(VAC$date[VAC$location == "Israel"], VAC$people_vaccinated_per_hundred[VAC$location == "Israel"], col = "Blue", lwd=2)
# text(END+5, max(VAC$people_vaccinated_per_hundred[VAC$location == "Israel"]) + runif(1, -1, 1),"Israel", col="Blue", cex=0.8)
# 
# Countries.Label <- VAC[VAC$location %in% Countries,]
# Countries.Label <- Countries.Label[order(Countries.Label$location, -Countries.Label$people_fully_vaccinated_per_hundred),]
# Countries.Label <- Countries.Label[!duplicated(Countries.Label$location), "Label"]
# legend(x = 'topleft', legend = Countries.Label, col = Colors[1:length(Countries)], lty=1, lwd=2)
# 
# Countries.Label <- VAC[VAC$location %in% c("Israel", "World"),]
# Countries.Label <- Countries.Label[order(Countries.Label$location, -Countries.Label$people_fully_vaccinated_per_hundred),]
# Countries.Label <- Countries.Label[!duplicated(Countries.Label$location), "Label"]
# legend(x = 'topright', legend = Countries.Label, col = c("Blue", "Black"), lty=1, lwd=3)
# 
# 
# START. <- as.numeric(START)
# END. <- as.numeric(END)
# rasterImage(statistx,
#             xleft=START.+190, xright=START.+204,
#             ybottom=80, ytop=92)
# 
# text(START.+47, 44, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()
# 
# 
# 
# 
# 
# jpeg(paste0("plots/vaccinations.daily ", Sys.Date(), ".jpg"), width = 800, height = 500)
# 
# 
# par(mar = c(7, 5, 5, 5))
# plot(c(START, END), c(0, 0), col=0, ylim=c(0,15000*1.2), xaxt = 'n', xlab = '', ylab = "Daily Vaccinations per 1M",
#      main = "Daily Vaccinations around the world", xlim=c(START+200, END))
# grid()
# points(VAC$date[VAC$location == "World"], VAC$daily_vaccinations_per_million[VAC$location == "World"], col = "Black", cex=1.5,  pch=19)
# lines(VAC$date[VAC$location == "World"], VAC$daily_vaccinations_per_million[VAC$location == "World"], col = "Black", lwd=2)
# axis(1, at = START:END, labels = format.Date(as.Date(as.numeric(START:END), "1970-01-01"), "%d %b %Y"), las=2)
# for (i in 1:length(Countries)) {
#         DATES <- VAC$date[VAC$location == Countries[i]][!is.na(VAC$daily_vaccinations_per_million[VAC$location == Countries[i]])]
#         VALUE <- VAC$daily_vaccinations_per_million[!is.na(VAC$daily_vaccinations_per_million) & VAC$location == Countries[i]]
#         points(DATES, VALUE, col = Colors[i], pch=19)
#         lines(DATES, VALUE, col = Colors[i])
# }
# 
# legend(x = 'topleft', legend = Countries, col = Colors[1:length(Countries)], lty=1, lwd=2)
# legend(x = 'topright', legend = c("Israel", "World"), col = c("Blue", "Black"), lty=1, lwd=3)
# 
# points(VAC$date[VAC$location == "Israel"], VAC$daily_vaccinations_per_million[VAC$location == "Israel"], col = "Blue", cex=1.5, pch=19)
# lines(VAC$date[VAC$location == "Israel"], VAC$daily_vaccinations_per_million[VAC$location == "Israel"], col = "Blue", lwd=2)
# 
# 
# START. <- as.numeric(START)
# END. <- as.numeric(END)
# rasterImage(statistx,
#             xleft=START.+205, xright=START.+208,
#             ybottom=14000, ytop=18000)
# 
# text(START.+40, 23000, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()






# Cases by Age ------------------------------------------------------------


# AGE <- read.csv("corona_age_and_gender_ver_00132.csv")
# names(AGE)[2] <- "Date"
# AGE$weekly_cases[AGE$weekly_cases == "<15"] <- 8
# AGE$weekly_cases <- as.numeric(AGE$weekly_cases)
# 
# AGE$weekly_deceased[AGE$weekly_deceased == "<15"] <- 8
# AGE$weekly_deceased <- as.numeric(AGE$weekly_deceased)
# 
# 
# AGE$Age.Group.1 <- AGE$age_group
# AGE$Age.Group.1[AGE$age_group %in% c("20-24", "25-29", "30-34", "35-39")] <- "20-39"
# AGE$Age.Group.1[AGE$age_group %in% c("40-44", "45-49", "50-54", "55-59")] <- "40-59"
# AGE$Age.Group.1[AGE$age_group %in% c("60-64", "65-69", "70-74", "75-79")] <- "60-79"
# 
# AGE.CASE <- aggregate(AGE$weekly_cases, list(AGE$Date, AGE$Age.Group.1), sum, na.rm = T)
# AGE.DEATH <- aggregate(AGE$weekly_deceased, list(AGE$Date, AGE$Age.Group.1), sum, na.rm = T)
# 
# AGE <- cbind(AGE.CASE, AGE.DEATH[3])
# 
# names(AGE) <- c("Date", "Age.Group", "Cases", "Death")
# AGE <- AGE[AGE$Age.Group != "NULL",]
# 
# unique(AGE$Age.Group)
# 
# ISR.POP <- read.csv("israel population.csv")
# ISR.POP$Age[ISR.POP$Age == "95+"] <- "95"
# ISR.POP$Age <- as.numeric(ISR.POP$Age)
# ISR.POP$Age.Group <- NA
# ISR.POP$Age.Group[ISR.POP$Age %in% 0:19] <- "0-19"
# ISR.POP$Age.Group[ISR.POP$Age %in% 20:39] <- "20-39"
# ISR.POP$Age.Group[ISR.POP$Age %in% 40:59] <- "40-59"
# ISR.POP$Age.Group[ISR.POP$Age %in% 60:79] <- "60-79"
# ISR.POP$Age.Group[ISR.POP$Age %in% 80:99] <- "80+"
# 
# ISR.POP$pop <- as.numeric(gsub(",", "", ISR.POP$pop))
# ISR.POP <- aggregate(ISR.POP$pop, list(ISR.POP$Age.Group), sum, na.rm = T)
# names(ISR.POP) <- c("Age.Group", "Pop")
# 
# 
# AGE$Age.Group. <- as.numeric(factor(AGE$Age.Group))
# AGE$Date <- as.Date(AGE$Date)
# 
# 
# AGE <- merge(AGE, ISR.POP, "Age.Group")
# 
# AGE$Cases.2.W <- NA
# for (i in 3:length(AGE$Age.Group)) {
#         if (AGE$Age.Group[i] == AGE$Age.Group[i-2]) {
#                 AGE$Cases.2.W[i] <- AGE$Cases[i-2]
#         }
# }
# 
# AGE$Incidence <- (AGE$Cases / AGE$Pop) * 10000
# AGE$Mortality <- (AGE$Death / AGE$Pop) * 1000000
# AGE$Fatality <- (AGE$Death / AGE$Cases.2.W) * 100
# 
# sum(AGE$Cases[AGE$Age.Group  %in% c("60-79", "80+") & AGE$Date == "2020-12-27"])
# 
# 
# jpeg(paste0("plots/cases.by.age", Sys.Date(), ".jpeg"), width = 500, height = 500)
# par(mar = c(8,5,5,5))
# plot(AGE$Date, as.numeric(AGE$Date), ylim=c(0, max(AGE$Incidence)), col=0, ylab="Incidence per 10,000",
#      xaxt='n', xlab='', main="Incidence by Age Group", xlim=c(min(AGE$Date)+250, max(AGE$Date)))
# for (i in 1:5) {
#         lines(AGE$Date[AGE$Age.Group. == i], AGE$Incidence[AGE$Age.Group. == i], col = (i+1))
# }
# axis(1, at = AGE$Date, labels = format.Date(AGE$Date, "%d-%b-%Y"), las = 2)
# 
# legend('topleft', legend = unique(AGE$Age.Group), lty = 1, col = 2:6)
# 
# 
# START <- min(as.numeric(AGE$Date))
# rasterImage(statistx,
#             xleft=START+30, xright=START+70,
#             ybottom=30, ytop=40)
# text(START+50, 25, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()
# 
# 
# 
# 
# jpeg(paste0("plots/death.by.age", Sys.Date(), ".jpeg"), width = 500, height = 500)
# par(mar = c(8,5,5,5))
# plot(AGE$Date, as.numeric(AGE$Date), ylim=c(0, max(AGE$Mortality)), col=0, ylab="Mortality per 1,000,000",
#      xaxt='n', xlab='', main="Mortality by Age Group", xlim=c(min(AGE$Date)+250, max(AGE$Date)))
# for (i in 1:5) {
#         lines(AGE$Date[AGE$Age.Group. == i], AGE$Mortality[AGE$Age.Group. == i], col = (i+1))
# }
# axis(1, at = AGE$Date, labels = format.Date(AGE$Date, "%d-%b-%Y"), las = 2)
# 
# legend('topleft', legend = unique(AGE$Age.Group), lty = 1, col = 2:6)
# 
# 
# START <- min(as.numeric(AGE$Date))
# rasterImage(statistx,
#             xleft=START+290, xright=START+320,
#             ybottom=220, ytop=320)
# text(START+305, 180, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()
# 
# 
# 
# jpeg(paste0("plots/fatality.by.age", Sys.Date(), ".jpeg"), width = 500, height = 500)
# par(mar = c(8,5,5,5))
# plot(AGE$Date, as.numeric(AGE$Date), ylim=c(0, 50), col=0, ylab="Fatality per 100 cases",
#      xaxt='n', xlab='', main="Fatality by Age Group", xlim=c(min(AGE$Date)+250, max(AGE$Date)))
# for (i in 1:5) {
#         lines(AGE$Date[AGE$Age.Group. == i], AGE$Fatality[AGE$Age.Group. == i], col = (i+1))
# }
# axis(1, at = AGE$Date, labels = format.Date(AGE$Date, "%d-%b-%Y"), las = 2)
# 
# legend('topleft', legend = unique(AGE$Age.Group), lty = 1, col = 2:6)
# 
# 
# START <- min(as.numeric(AGE$Date))
# rasterImage(statistx,
#             xleft=START+60, xright=START+90,
#             ybottom=50, ytop=60)
# text(START+75, 45, paste0(format.Date(Sys.time(), "%d %B %Y\n%H:%M")))
# 
# dev.off()




# Death by Age ------------------------------------------------------------

# AGE <- read.csv("corona_deceased_ver_0076.csv")
# 
# unique(AGE$age_group)
# 
# AGE$age_group <- factor(AGE$age_group, c("<65", "65-74", "75-84", "85+"))
# 
# cbind(prop.table(table(AGE$age_group)),
# prop.table(table(AGE$age_group[(length(AGE$age_group)-250):length(AGE$age_group)])))
# 
# 
# AGE.TAB <- NULL
# 
# for (i in 500:length(AGE$age_group)) {
#         out <- prop.table(table(AGE$age_group[(i-499):i]))*100
#         AGE.TAB <- rbind(AGE.TAB, out)
# }
# AGE.TAB <- as.data.frame(AGE.TAB)
# 
# jpeg("plots/death.by.age.jpeg", width = 500, height = 500)
# plot(1:length(AGE.TAB$`65-74`), 1:length(AGE.TAB$`65-74`), ylim=c(0,100), xaxt='n', ylab="%", col=0, xlab='', main="Death by Age moving average trend")
# for (i in 1:4) {
#         lines(1:length(AGE.TAB$`65-74`), AGE.TAB[,levels(AGE$age_group)[i]], col = i+1)
# }
# 
# legend('topleft', legend = levels(AGE$age_group), lty = 1, col = 2:5)
# axis(1, at = c(1, length(AGE.TAB$`65-74`)), labels = c("First 500", "Last 500"))
# dev.off()


# End ---------------------------------------------------------------------


TOP.10.DEATH.RANK

ISRAEL.DEATH.RANK

TOP.10.OUTBREAK.RANK

ISRAEL.OUTBREAK.RANK

dth.isr[(length(dth.isr)-6):length(dth.isr)]
sum(dth.isr[(length(dth.isr)-6):length(dth.isr)])
mean(dth.isr[(length(dth.isr)-6):length(dth.isr)])

Corona.Country.Israel$New.Cases[(length(Corona.Country.Israel$New.Cases)-6):length(Corona.Country.Israel$New.Cases)]
sum(Corona.Country.Israel$New.Cases[(length(Corona.Country.Israel$New.Cases)-6):length(Corona.Country.Israel$New.Cases)])
mean(Corona.Country.Israel$New.Cases[(length(Corona.Country.Israel$New.Cases)-6):length(Corona.Country.Israel$New.Cases)])

