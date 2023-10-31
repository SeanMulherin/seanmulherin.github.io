setwd("Desktop/Portfolio/CO2 Production/Global Socio-Economic & Environmental Indicators/")
co2 <- read.csv("co2_production.csv", header = TRUE, stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(boot)

############## Cleaning Data ############## 

co2 <- na.omit(co2)
co2_skinny <- co2 %>% select(-c("ISO3", "region", "hdi_rank_2021"))

co2_annual_sum <- co2_skinny %>% select(-c("hdicode", "Country")) %>% colSums()
co2_world <- data.frame(x = 1990:2021, y = c(t(co2_annual_sum)))


co2_reshaped <- data.frame(x = 1990:2021,
                           y = c(t(co2_skinny[, 3:34])),
                           Country = rep(c(co2_skinny$Country), each = 32),
                           hdicode = rep(c(co2_skinny$hdicode), each = 32))

############## Correlation HDI x CO2p_Sum ############## 
hdi_co2total_df <- data.frame(Country = co2_skinny$Country,
                              HDI_Code = co2_skinny$hdicode,
                              CO2_total = co2_skinny[ , 3:32] %>% rowSums()) %>%
                    arrange(desc(CO2_total))

dummy_hdi <- hdi_co2total_df
for(i in 1:nrow(dummy_hdi)){
  if(dummy_hdi$HDI_Code[i] == "Low") {
    dummy_hdi$HDI_Code[i] = 1
  } else if(dummy_hdi$HDI_Code[i] == "Medium") {
    dummy_hdi$HDI_Code[i] = 2
  } else if(dummy_hdi$HDI_Code[i] == "High") {
    dummy_hdi$HDI_Code[i] = 3
  } else {
    dummy_hdi$HDI_Code[i] = 4
  }
}
dummy_hdi$HDI_Code <- as.numeric(dummy_hdi$HDI_Code)
     
cor <- cor(dummy_hdi$HDI_Code, dummy_hdi$CO2_total)

plot(x=dummy_hdi$HDI_Code, y=dummy_hdi$CO2_total,
     xlab = "HDI Code", ylab = "CO2 Production (CEI)",
     title("HDI Code vs. CO2 Production"),
     col = "#F8766D", pch = 16)
abline(lm(dummy_hdi$CO2_total ~ dummy_hdi$HDI_Code), col = "#F8766D", lwd = 3)
text(2.2, 1200, "Pearson's Correlation Coefficient = 0.6")



############## Overall CO2 Production Graph  By Country ############## 

overall_graph <- ggplot(co2_reshaped, aes(x = x, y = y, color = Country)) +
                    geom_line() +
                    theme_bw() +
                    ggtitle("Annual CO2 Production 1990-2021 By Country") +
                    xlab("Year") +
                    ylab("CO2 Production (CEI)")
overall_graph + theme(legend.position="none", 
                        plot.title = element_text(hjust = 0.5), 
                        panel.border = element_blank(),
                        strip.background = element_blank())

############## Overall CO2 Production Graph World ############## 
world_graph <- ggplot(co2_world, aes(x = x, y = y)) +
                geom_line(color = "#F8766D", lwd = 1.5) +
                theme_bw() +
                ggtitle("World's Annual CO2 Production 1990-2021") +
                xlab("Year") +
                ylab("CO2 Production (CEI)")
world_graph + theme(legend.position="none", 
                      plot.title = element_text(hjust = 0.5), 
                      panel.border = element_blank(),
                      strip.background = element_blank())

############## Overall CO2 Production Graph - separated by HDI Code ############## 
faceted_co2_graph <- ggplot(co2_reshaped, aes(x = x, y = y, col = Country)) +
                       geom_line() +
                       theme_bw() +
                       facet_wrap(~factor(hdicode, levels = c("Low", "Medium", "High", "Very High")) , ncol = 1) +
                       ggtitle("Annual CO2 Production 1990-2021 Separated by HDI Code") +
                       xlab("Year") +
                       ylab("CO2 Production (CEI)")
faceted_co2_graph + theme(legend.position="none", 
                          plot.title = element_text(hjust = 0.5), 
                          panel.border = element_blank(),
                          strip.background = element_blank())

############## Bar Graphs - total co2 produced ############## 
world_hdi <- hdi_co2total_df[1:50, ] %>%
  mutate(Country = fct_reorder(Country, CO2_total))

world_hdi_bar <- ggplot(world_hdi, aes(x=Country, y=CO2_total, fill= Country)) +
  geom_bar(stat="identity", alpha=.4) +
  scale_fill_hue() +
  coord_flip() + 
  ggtitle("Total CO2 Production 1990-2021 [Top 50 Countries]") +
  ylab("CO2 Produced (CEI)") +
  xlab("")

world_hdi_bar +  theme(legend.position = "none",
                     plot.title = element_text(hjust = 0.5),
                     panel.background = element_rect(fill = "white"),
                     axis.line.x = element_line("grey"))


low_hdi <- hdi_co2total_df %>%
  filter(HDI_Code == "Low") %>%
  mutate(Country = fct_reorder(Country, CO2_total))

low_hdi_bar <- ggplot(low_hdi, aes(x=Country, y=CO2_total, fill= Country)) +
  geom_bar(stat="identity", alpha=.4) +
  scale_fill_hue() +
  coord_flip() + 
  ggtitle("Total CO2 Production - Low HDI Code") +
  ylab("CO2 Produced (CEI)") +
  xlab("")

low_hdi_bar +  theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                panel.background = element_rect(fill = "white"),
                axis.line.x = element_line("grey"))
        

med_hdi <- hdi_co2total_df %>%
  filter(HDI_Code == "Medium") %>%
  mutate(Country = fct_reorder(Country, CO2_total))

med_hdi_bar <- ggplot(med_hdi, aes(x=Country, y=CO2_total, fill= Country)) +
  geom_bar(stat="identity", alpha=.4) +
  scale_fill_hue() +
  coord_flip() + 
  ggtitle("Total CO2 Production - Medium HDI Code") +
  ylab("CO2 Produced (CEI)") +
  xlab("")

med_hdi_bar +  theme(legend.position = "none",
                     plot.title = element_text(hjust = 0.5),
                     panel.background = element_rect(fill = "white"),
                     axis.line.x = element_line("grey"))


high_hdi <- hdi_co2total_df %>%
  filter(HDI_Code == "High") %>%
  mutate(Country = fct_reorder(Country, CO2_total))

high_hdi_bar <- ggplot(high_hdi, aes(x=Country, y=CO2_total, fill= Country)) +
  geom_bar(stat="identity", alpha=.4) +
  scale_fill_hue() +
  coord_flip() + 
  ggtitle("Total CO2 Production - High HDI Code") +
  ylab("CO2 Produced (CEI)") +
  xlab("")

high_hdi_bar +  theme(legend.position = "none",
                     plot.title = element_text(hjust = 0.5),
                     panel.background = element_rect(fill = "white"),
                     axis.line.x = element_line("grey"))


vhigh_hdi <- hdi_co2total_df %>%
  filter(HDI_Code == "Very High") %>%
  mutate(Country = fct_reorder(Country, CO2_total))

vhigh_hdi_bar <- ggplot(vhigh_hdi, aes(x=Country, y=CO2_total, fill= Country)) +
  geom_bar(stat="identity", alpha=.4) +
  scale_fill_hue() +
  coord_flip() + 
  ggtitle("Total CO2 Production - Very High HDI Code") +
  ylab("CO2 Producted (CEI)") +
  xlab("")

vhigh_hdi_bar +  theme(legend.position = "none",
                      plot.title = element_text(hjust = 0.5),
                      panel.background = element_rect(fill = "white"),
                      axis.line.x = element_line("grey"))



############## Overall CO2 Production Graph - separated by HDI Code - Filtering out low avg ROC ############## 
low_total_co2p_countries <- c( 
  low_hdi[low_hdi$co2_production_sum < 14, "Country"],
  medium_hdi[medium_hdi$co2_production_sum < 50, "Country"],
  high_hdi[high_hdi$co2_production_sum < 170, "Country"],
  vhigh_hdi[vhigh_hdi$co2_production_sum < 550, "Country"])

co2_filter_low_roc <- co2_reshaped %>%
  filter(!(Country %in% low_total_co2p_countries))

xs <- split(co2_filter_low_roc, f = co2_filter_low_roc$hdicode)
low <- ggplot(xs$Low, aes(x = x, y = y, col = Country)) +
        geom_line() +
        facet_wrap(~hdicode) +
        xlab("Year") +
        ylab("CO2 Production") + 
        theme_bw() +
        scale_x_continuous(breaks = c(1990, 2000, 2010, 2020), limits = c(1990, 2021)) + 
        theme(plot.title = element_text(hjust = 0.5), 
              panel.border = element_blank(),
              strip.background = element_blank(),
              axis.title = element_blank(),
              legend.title = element_blank())
        
        
med <- low %+% xs$Medium
high <- low %+% xs$High
vhigh <- low %+% xs["Very High"]

filtered_faceted_graph <- grid.arrange(low, med, high, vhigh, ncol = 1,
             top = "Filtered Annual CO2 Production 1990-2021 Separated by HDI Code",
             bottom = "Year",
             left = "CO2 Production (CEI)")



############## Overall CO2 Average Production Country Map ##############                        
avgroc <- (co2_skinny$co2_prod_2021 - co2_skinny$co2_prod_1990)/32
avgroc.df <- data.frame(region = co2_skinny$Country, avgroc = avgroc) %>% arrange(avgroc)
world <- map_data("world")

#avgroc.df$region %in% world$region #there are inconsistencies with region names so we need to rename these
#avgroc.df$region[ which(!(avgroc.df$region %in% world$region)) ]
#View(sort(unique(world$region)))
avgroc.df <- avgroc.df %>% mutate(region = recode(str_trim(region), "United States" = "USA",
                                    "United Kingdom" = "UK",
                                    "Korea (Republic of)" = "South Korea",
                                    "Congo (Democratic Republic of the)" = "Democratic Republic of the Congo",
                                    "Congo" = "Republic of Congo",
                                    "Czechia" = "Czech Republic",
                                    "Syrian Arab Republic" = "Syria",
                                    "Saint Kitts and Nevis" = "Saint Kitts",
                                    "Iran (Islamic Republic of)" = "Iran",
                                    "Moldova (Republic of)" = "Moldova",
                                    "Brunei Darussalam" = "Brunei",
                                    "Antigua and Barbuda" = "Antigua",
                                    "TÃ¼rkiye" = "Turkey",
                                    "Trinidad and Tobago" = "Trinidad",
                                    "Russian Federation" = "Russia",
                                    "Tanzania (United Republic of)" = "Tanzania",
                                    "Bolivia (Plurinational State of)" = "Bolivia",
                                    "Viet Nam" = "Vietnam",
                                    "Venezuela (Bolivarian Republic of)" = "Venezuela",
                                    "Palestine, State of" = "Palestine",
                                    "Saint Vincent and the Grenadines" = "Saint Vincent",
                                    "Lao People's Democratic Republic" = "Laos"
                                    ))

joined_co2 <- inner_join(world, avgroc.df, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)
avgroc_graph <- ggplot(joined_co2, mapping = aes(x = long, y = lat, group = group)) +
                 geom_polygon(aes(fill = avgroc)) +
                 coord_fixed(1.3) +
                 scale_fill_distiller(palette ="RdBu", direction = -1) +
                 plain +
                 ggtitle("Average Rate of Change in CO2 Production 1990-2021")
avgroc_graph


############## Regression Analysis - Predicting Future Global CO2 Production  ##############   
co2_world_noLev <- co2_world[1:30, ]
m1 <- glm(y ~ x, data = co2_world_noLev)
m1.error <- cv.glm(co2_world_noLev, m1)$delta[1] 

m2 <- glm(y ~ x + poly(x, 2), data = co2_world_noLev)
m2.error <- cv.glm(co2_world_noLev, m2)$delta[1]

m3 <- glm(y ~ x + poly(x, 5), data = co2_world_noLev)
m3.error <- cv.glm(co2_world_noLev, m3)$delta[1]

c(m1.error, m2.error, m3.error)

plot(co2_world_noLev$x, co2_world_noLev$y, main = "Generalized Linear Model Fits",
     xlab="Year", ylab="CO2 Production (CEI)")
lines(co2_world_noLev$x, predict(m1), col = 2)
lines(co2_world_noLev$x, predict(m2), col = 3)
lines(co2_world_noLev$x, predict(m3), col = 4)



########## Optimal Regression ##############
m1 <- glm(y ~ x, data = co2_world[1:30, ])
future_co2p <- rep(NA, 60)
future_co2p[1:30] <- co2_world$y[1:30]

plot(1991:2050, future_co2p, main = "Linear Regression on Global CO2 Production",
     xlab="Year", ylab="CO2 Production (CEI)")
abline(a=m1$coef[1], b=m1$coef[2], col = 4, lwd = 2)
abline(h=896, lty = 2)
points(x=2050, y=predict(m1, data.frame(x=2050)), pch = 16, col = "red")
text(x=2047, y=predict(m1, data.frame(x=2050)) + 7, "(2050, 896)")


########## Inoptimal Regression With Leverage Points ##############
m1 <- glm(y ~ x, data = co2_world)
future_co2p <- rep(NA, 60)
future_co2p[1:32] <- co2_world$y

plot(1991:2050, future_co2p, main = "Linear Regression on Global CO2 Production
     With Leverage Points",
     xlab="Year", ylab="CO2 Production (CEI)")
abline(a=m1$coef[1], b=m1$coef[2], col = 4, lwd = 2)
abline(h=845, lty = 2)
points(x=2050, y=predict(m1, data.frame(x=2050)), pch = 16, col = "red")
points(x=2021, y=future_co2p[31], pch = 16, col = "purple")
points(x=2022, y=future_co2p[32], pch = 16, col = "purple")
text(x=2047, y=predict(m1, data.frame(x=2050)) + 7, "(2050, 845)")
text(x=2015, y=future_co2p[32] + 5, "Leverage Points")


########## Global Temperature ##############
temp <- read.table("temp.txt")
temp_graph <- ggplot(temp, aes(x = V1, y = V2)) +
  geom_line(color = "#F8766D", lwd = 1.5) +
  theme_bw() +
  ggtitle("Average Global Land Temperature 1875-2022") +
  xlab("Year") +
  ylab("Average Temperature (F)")
temp_graph + geom_smooth() + theme(plot.title = element_text(hjust = 0.5))


temp2 <- temp[116:147, ]
temp_graph2 <- ggplot(temp2, aes(x = V1, y = V2)) +
  geom_line(color = "#F8766D", lwd = 1.5) +
  theme_bw() +
  ggtitle("Average Global Land Temperature 1990-2021") +
  xlab("Year") +
  ylab("Average Temperature (F)")
temp_graph2 + geom_smooth() + theme(plot.title = element_text(hjust = 0.5))

############ Cubic Regression on Temp #############
cubic_temp <- glm(V2 ~ temp$V1 + I(temp$V1^2) + I(temp$V1^3), data = temp)
y <- rep(NA, 176)
y[1:148] <- temp$V2
x <- 1875:2060
yhat <- cubic_temp$coef[1] + cubic_temp$coef[2]*x + cubic_temp$coef[3]*x^2 + cubic_temp$coef[4]*x^3

plot(x=1875:2050, y=y, main = "Cubic Regression on Global AVG Temperature",
     xlab="Year", ylab="Temperature (F)", pch = 16, cex = .5, ylim = c(48, 62))
lines(x=1875:2060, y=yhat, col = 2, lwd = 2)
points(x=2050, y=yhat[176], pch = 16, col = "blue")
text(x=2020, y=yhat[176], "(2050, 61.1)")

############ Boxplot HDI Code and CO2 production #####################
low <- c()
med <- c()
high <- c()
vhigh <- c()
for(i in 1:nrow(co2_skinny)){
  if(co2_skinny$hdicode[i] == "Low"){
    low <- append(low, co2_skinny[i, 3:34])
  } else if(co2_skinny$hdicode[i] == "Medium"){
    med <- append(med, co2_skinny[i, 3:34])
  } else if(co2_skinny$hdicode[i] == "High"){
    high <- append(med, co2_skinny[i, 3:34])
  } else if(co2_skinny$hdicode[i] == "Very High"){
    vhigh <- append(vhigh, co2_skinny[i, 3:34])
  }
}
hdi <- rep("Low", length(low))
co2p <- as.numeric(low)

hdi <- append(hdi, rep("Medium", length(med)))
hdi <- append(hdi, rep("High", length(high)))
hdi <- append(hdi, rep("Very High", length(vhigh)))

co2p <- append(co2p, as.numeric(med))
co2p <- append(co2p, as.numeric(high))
co2p <- append(co2p, as.numeric(vhigh))

box_whisker_df <- data.frame(HDI = hdi,
                             co2p = co2p)

box_n_whisker <- ggplot(box_whisker_df, aes(x=HDI, y=co2p, fill=HDI)) +
                    geom_boxplot() +
                    theme_bw() +
                    ggtitle("Boxplot HDI Code vs CO2 Production") +
                    ylab("log(CO2 Production)") +
                    xlab("HDI Code") +
                    scale_x_discrete(limits=c("Low", "Medium", "High", "Very High")) +
                    scale_y_continuous(trans="log10") +
                    theme(plot.title = element_text(hjust = 0.5))
                    
box_n_whisker








########## Misc ##############
library(lubridate)
temp <- read.csv("temp.csv")
temp <- na.omit(temp)
temp <- temp %>% select("Country", "dt", "AverageTemperature")
colnames(temp)[1] <- "region"
temp <- temp %>% filter(dt >= 1990)

first <- c()
last <- c()
for(i in 1:nrow(temp)){
  if(temp$dt[i] == "1990-01-01"){
    first[i] <- temp$AverageTemperature[i]
  } else if(temp$dt[i] == "2013-01-01"){
    last[i] <- temp$AverageTemperature[i]
  }
}
first <- na.omit(first)
last <- na.omit(last)
roctempcalc <- round((last - first)/23, 4)
uniqueregions <- unique(temp$region)
temp.df <- data.frame(region = uniqueregions, 
                 roctemp = roctempcalc)


temp.df <- temp.df %>% mutate(region = recode(str_trim(region), "United States" = "USA",
                                                  "United Kingdom" = "UK",
                                                  "Korea (Republic of)" = "South Korea",
                                                  "Congo (Democratic Republic of the)" = "Democratic Republic of the Congo",
                                                  "Congo" = "Republic of Congo",
                                                  "Czechia" = "Czech Republic",
                                                  "Syrian Arab Republic" = "Syria",
                                                  "Saint Kitts and Nevis" = "Saint Kitts",
                                                  "Iran (Islamic Republic of)" = "Iran",
                                                  "Moldova (Republic of)" = "Moldova",
                                                  "Brunei Darussalam" = "Brunei",
                                                  "Antigua and Barbuda" = "Antigua",
                                                  "TÃ¼rkiye" = "Turkey",
                                                  "Trinidad and Tobago" = "Trinidad",
                                                  "Russian Federation" = "Russia",
                                                  "Tanzania (United Republic of)" = "Tanzania",
                                                  "Bolivia (Plurinational State of)" = "Bolivia",
                                                  "Viet Nam" = "Vietnam",
                                                  "Venezuela (Bolivarian Republic of)" = "Venezuela",
                                                  "Palestine, State of" = "Palestine",
                                                  "Saint Vincent and the Grenadines" = "Saint Vincent",
                                                  "Lao People's Democratic Republic" = "Laos"
))

joined_temp <- inner_join(world, temp.df, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)
temp_graph <- ggplot(joined_temp, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = roctemp)) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette ="RdBu", direction = -1) +
  plain +
  ggtitle("Annual Temperature Rate of Change 1990-2013")
temp_graph












