library(ggplot2)
library(dplyr)
library(stringr)

new <- read.csv("fuero-comun-estados.csv.gz")
old <- read.csv("fuero-comun-estados-2014-03-24 17:22:18.655226.csv.gz")

old <- subset(old, state_code == 30 & type == "DOLOSOS")
new <- subset(new, state_code == 30 & type == "DOLOSOS")

old <- old %.%
    group_by(type, date) %.%
    summarise(total = sum(count))
old$date <- as.Date(old$date)

new <- new %.%
    group_by(type, year, month) %.%
    summarise(total = sum(count))
new$date <- as.Date(str_c(new$year,"-",new$month,"-01"))
new$year <- NULL
new$month <- NULL

old$type <- "Datos al 24 de Marzo 2014"
new$type <- "Datos al 22 de Abril 2014"
hom <- rbind(old, new)

df <- data.frame(xmin=as.Date("2013-01-01"),
               xmax=as.Date("2013-12-01"),
               ymin=c(-Inf),
               ymax=c(Inf))

ggplot(hom, aes(date, total, group = type, color = type)) +
    ylim(0, 100) +
    xlim(as.Date("2013-01-01"), as.Date("2014-04-01")) +
    geom_rect(data = df, aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              inherit.aes=FALSE,
              color = "transparent", alpha = .2,
              fill = "red") +
    geom_line(size = 1) +
    ggtitle("Cambio en cifras de homicidio doloso del estado de Veracruz\ncorrespondientes al año 2013") +
    scale_color_manual("fecha", values = c("black", "#aaaaaa")) +
    theme_bw()

ggsave("cambio.png", dpi = 100, width = 9.60, height = 5.60)
