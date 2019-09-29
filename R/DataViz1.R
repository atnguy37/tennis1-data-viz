library(readr)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggimage)
library(magick)
library(cowplot)

aus_open <- read.csv("./data/10years_AusOpen_MaleFinalMatchesDetail.csv", header = TRUE)


aus_open$return1 <- as.numeric(gsub("[\\%,]", "", aus_open$return1))
aus_open$return2 <- as.numeric(gsub("[\\%,]", "", aus_open$return2))


aus_open$secPointWon1 <- as.numeric(gsub("[\\%,]", "", aus_open$secPointWon1))
aus_open$secPointWon2 <- as.numeric(gsub("[\\%,]", "", aus_open$secPointWon2))

victory <- rep(c(1,0),11)

victory

year <- rep(aus_open$year,each = 2)

year
as.vector(t(aus_open[c('player1','player2')]))

ass1 <- data.frame("year" = year,"player" = as.vector(t(aus_open[c('player1','player2')])),
"victory" = victory,
"total" = as.vector(t(aus_open[c('total1','total2')])),
"return" = as.vector(t(aus_open[c('return1','return2')])),
"secPointWon" = as.vector(t(aus_open[c('secPointWon1','secPointWon2')])))

image <- c(1:length(ass1$player))
player <- ass1$player
image 
for (i in 1:length(image)) {
    print(image[i])
    image[i] <- paste("./data/",player[i],".png",sep="")
}

image

ass1

ass1$image <- image
fun.1 <- function(x) (-0.7) * x + 70


label.x <- seq(25,85,by = 10)
label.y <- seq(10,50,by = 10)
# label <- paste(label,"%",sep="")

label.x
label.y
p <- ggplot(ass1, aes(secPointWon, return ))+ labs(title="Return and Second Point Won In\nAustralian Open Championship Final Matches", y="Return", x="Second Point Won")
p + geom_point(aes(color = factor(victory)), stroke = 1.5, shape = 1, size = 7)+ ylim(15, 50) +
geom_text_repel(aes(label=year), size = 3, direction = "both", box.padding = unit(0.75, "lines")) + stat_function(fun = fun.1) +
geom_image(aes(image=image), size=.03, show.legend = TRUE) +
# scale_color_discrete(name="Player",breaks=c("1", "0"),labels=c("Winner", "Loser")) +
scale_fill_discrete(name="Good") +
scale_color_manual (name="Player",breaks=c("1", "0"),labels=c("Winner", "Loser"), values=c("#C02F1D", "#107896")) +
guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) +
scale_x_continuous(limits=c(20,90),breaks = label.x, labels = paste(label.x,"%",sep="")) +
scale_y_continuous(limits=c(10,52), breaks = label.y, labels = paste(label.y,"%",sep="")) +
theme(
  panel.background = element_rect(fill = "white",
                                size = 0.5, linetype = "solid")
  )

ggsave("ass1.png", dpi=900)
# tiff("ass1.jpg", units="in", width=5, height=5, res=300)
# ggdraw() +
#   draw_image("australian_open_logo_new.png") +
#   draw_plot(p)

write.csv(ass1,'ass1.csv')
#https://stackoverflow.com/questions/10437442/place-a-border-around-points
#https://stackoverflow.com/questions/2181902/how-to-use-an-image-as-a-point-in-ggplot
#http://www.sthda.com/english/wiki/print.php?id=168
#https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want
#https://stackoverflow.com/questions/42949875/how-to-reduce-geom-text-overlap


#https://stackoverflow.com/questions/34847598/using-custom-images-instead-of-standard-shapes-for-r-line-chart-markers
#https://stackoverflow.com/questions/36133374/custom-legend-with-imported-images
#https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
#https://ggplot2.tidyverse.org/reference/geom_point.html
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
#http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software

#https://ausopen.com/match/2019-Novak-Djokovic-vs-Rafael-Nadal-MS701#!stats

#https://github.com/slowkow/ggrepel/issues/14

#https://stackoverflow.com/questions/40944239/removing-ggplot2-legend-removes-whole-data-from-the-plot


#https://www.viget.com/articles/color-contrast/

