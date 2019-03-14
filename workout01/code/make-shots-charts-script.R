#Title: Making charts
#Description: This is code for making shot charts for the GSW players.
#input: shots-data.csv, curry-1.csv, durant-1.csv, green-1.csv, thompson-1.csv, iguodala-1.csv
#output: ande-iguodala-shot-chart.pdf, draymond-green-shot-chart.pdf, gsw-shot-charts.pdf,
# gsw-shot-charts.pdf, kevin-durant-shot-chart.pdf, klay-thompson-shot-chart.pdf,
# stephen-curry-shot-chart.pdf
library(ggplot2)
library(jpeg)
library(grid)
options(max.print = 999999)
thompson = read.csv("../data/thompson-1.csv", stringsAsFactors = FALSE)
thompson
thompson_scatterplot = ggplot(data = thompson) + geom_point(aes(x = x, y = y, color =  shot_made_flag))
thompson_scatterplot
court_file = "../images/nba-court.jpg"
court_image = rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)
thompson_shot_chart = ggplot(data = thompson) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: Klay Thompson (2016 season)") + 
  theme_minimal()
thompson_shot_chart
help(ggsave)
ggsave(filename = "klay-thompson-shot-chart.pdf", plot = thompson_shot_chart, path = "../images", width = 6.5, height = 5)

curry = read.csv("../data/curry-1.csv", stringsAsFactors = FALSE)
curry
curry_shot_chart = ggplot(data = curry) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: Stephen Curry (2016 season)") + 
  theme_minimal()
ggsave(filename = "stephen-curry-shot-chart.pdf", plot = curry_shot_chart, path = "../images", width = 6.5, height = 5)

durant = read.csv("../data/durant-1.csv", stringsAsFactors = FALSE)
durant
durant_shot_chart = ggplot(data = durant) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: Kevin Durant (2016 season)") + 
  theme_minimal()
durant_shot_chart
ggsave(filename = "kevin-durant-shot-chart.pdf", plot = durant_shot_chart, path = "../images", width = 6.5, height = 5)

iguodala = read.csv("../data/iguodala-1.csv", stringsAsFactors = FALSE)
iguodala
iguodala_shot_chart = ggplot(data = iguodala) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: Andre Iguodala (2016 season)") + 
  theme_minimal()
iguodala_shot_chart
ggsave(filename = "andre-iguodala-shot-chart.pdf", plot = iguodala_shot_chart, path = "../images", width = 6.5, height = 5)

green = read.csv("green-1.csv", stringsAsFactors = FALSE)
green
green_shot_chart = ggplot(data = green) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: Draymond Green (2016 season)") + 
  theme_minimal()
ggsave(filename = "draymond-green-shot-chart.pdf", plot = green_shot_chart, path = "../images", width = 6.5, height = 5)

total = read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)
total
total_shot_chart = ggplot(data = total) + 
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50,420) + 
  ggtitle("Shot Chart: GSW (2016 season)") + 
  theme_minimal() +
  facet_wrap(~name)
total_shot_chart
ggsave(filename = "gsw-shot-charts.pdf", plot = total_shot_chart, path = "../images", width = 8, height = 7)
ggsave(filename = "gsw-shot-charts.png", plot = total_shot_chart, path = "../images", width = 8, height = 7)
