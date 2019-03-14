#Title: Data Preperation
#Description: This is the code used to prepare the raw data into clean data
#Input: stenphen-curry.csv, andre-iguodala.csv, kevin-durant.csv, klay-thompson.csv, draymond-green.csv
#Output: stenphen-curry-summary.txt, andre-iguodala-summary.txt, kevin-durant-summary.txt,
# klay-thompson-summary.txt, draymond-green-summary.txt, shots-data-summary.txt, shots-data.csv,
# curry-1.csv, durant-1.csv, green-1.csv, thompson-1.csv, iguodala-1.csv
options(max.print = 99999999)
curry = read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala = read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
durant = read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson = read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
green = read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
curry$name = "stephen curry"
iguodala$name = "andre iguodala"
durant$name = "kevin durant"
thompson$name = "klay thompson"
green$name = "draymond green"
curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes" 
curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] = "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] = "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] = "shot_yes"  
durant$shot_made_flag[durant$shot_made_flag == "n"] = "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] = "shot_yes" 
thompson$shot_made_flag[thompson$shot_made_flag == "n"] = "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] = "shot_yes" 
green$shot_made_flag[green$shot_made_flag == "n"] = "shot_no"
curry$minute = (curry$period - 1) * 12 + (12 - curry$minutes_remaining)
iguodala$minute = (iguodala$period - 1) * 12 + (12 - iguodala$minutes_remaining)
durant$minute = (durant$period - 1) * 12 + (12 - durant$minutes_remaining)
thompson$minute = (thompson$period - 1) * 12 + (12 - thompson$minutes_remaining)
green$minute = (green$period - 1) * 12 + (12 - green$minutes_remaining)

sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file = "../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()

shots_data = rbind(curry, iguodala, durant, thompson, green)
shots_data

write.csv(x = shots_data,
          file = "../data/shots-data.csv")

sink(file = "../output/shot-data-summary.txt")
summary(shots_data)
sink()

write.csv(x = thompson,
          file = "../data/thompson-1.csv")
write.csv(x = curry,
          file = "../data/curry-1.csv")
write.csv(x = durant,
          file = "../data/durant-1.csv")
write.csv(x = green,
          file = "../data/green-1.csv")
write.csv(x = iguodala,
          file = "../data/iguodala-1.csv")
