library(rvest)
library(dplyr)
library(xml2)

col_link = "https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/tables/table-22"
col_page = read_html(col_link)

col_table = col_page %>% html_nodes("table.data") %>%
  html_table() %>% . [[1]]

View(col_table)
write.csv(col_table, file = "Table_17.csv")

sum(is.na(Table_17$`Total aggravated assaults1`))
sum(is.na(Table_17$Firearms))
sum(is.na(Table_17$`Other weapons`))
sum(is.na(Table_17$`Personal weapons`))
sum(is.na(Table_17$`Agency count`))A
sum(is.na(Table_17$Population))

library(dplyr)
Table_17<-Table_17 %>% mutate(Type =
                            case_when(Population  <  86670 ~    "Small", 
                                      Population  <  866718 ~   "Medium",
                                      Population  <  9910658 ~  "Large",
                                      Population  >= 15000000 ~ "Extra Large")
)
View(Table_17)

boxplot(Table_17$`Total aggravated assaults1`)
out <- boxplot.stats(Table_17$`Total aggravated assaults1`)$out
mtext(paste("Total assault: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 102822] <- mean(Table_17$`Total aggravated assaults1`)
Table_17[Table_17 == 55333] <- mean(Table_17$`Total aggravated assaults1`)
Table_17[Table_17 == 43967] <- mean(Table_17$`Total aggravated assaults1`)
Table_17[Table_17 == 73258] <- mean(Table_17$`Total aggravated assaults1`)
Table_17[Table_17 == 29325] <- mean(Table_17$`Total aggravated assaults1`)
Table_17[Table_17 == 30687] <- mean(Table_17$`Total aggravated assaults1`)




boxplot(Table_17$Firearms)
out <- boxplot.stats(Table_17$Firearms)$out
mtext(paste("Firearms: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 8993] <- mean(Table_17$Firearms)
Table_17[Table_17 == 10813] <- mean(Table_17$Firearms)
Table_17[Table_17 == 13807] <- mean(Table_17$Firearms)
Table_17[Table_17 == 17196] <- mean(Table_17$Firearms)
Table_17[Table_17 == 17341] <- mean(Table_17$Firearms)
Table_17[Table_17 == 25970] <- mean(Table_17$Firearms)
Table_17[Table_17 == 7896] <- mean(Table_17$Firearms)
Table_17[Table_17 == 8598] <- mean(Table_17$Firearms)
Table_17[Table_17 == 6636] <- mean(Table_17$Firearms)


Table_17$`Total aggravated assaults1`=lapply(ceiling(Table_17$`Total aggravated assaults1`),as.integer)



boxplot(Table_17$`Knives or cutting instruments`)
out <- boxplot.stats(Table_17$`Knives or cutting instruments`)$out
mtext(paste("Knivers or cutting instruments: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 16098] <- mean(Table_17$`Knives or cutting instruments`)
Table_17[Table_17 == 10213] <- mean(Table_17$`Knives or cutting instruments`)
Table_17[Table_17 == 11626] <- mean(Table_17$`Knives or cutting instruments`)
Table_17[Table_17 == 14156] <- mean(Table_17$`Knives or cutting instruments`)
Table_17[Table_17 == 5580] <- mean(Table_17$`Knives or cutting instruments`)
Table_17[Table_17 == 5270] <- mean(Table_17$`Knives or cutting instruments`)


boxplot(Table_17$`Other weapons`)
out <- boxplot.stats(Table_17$`Other weapons`)$out
mtext(paste("Other weapons: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 34424] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 18648] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 12824] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 9587] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 20714] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 7502] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 8934] <- mean(Table_17$`Other weapons`)
Table_17[Table_17 == 12825] <- mean(Table_17$`Other weapons`)


boxplot(Table_17$`Personal weapons`)
out <- boxplot.stats(Table_17$`Personal weapons`)$out
mtext(paste("Personal weapons: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 34959] <- mean(Table_17$`Personal weapons`)
Table_17[Table_17 == 9276] <- mean(Table_17$`Personal weapons`)
Table_17[Table_17 == 14981] <- mean(Table_17$`Personal weapons`)
Table_17[Table_17 == 12418] <- mean(Table_17$`Personal weapons`)
Table_17[Table_17 == 7607] <- mean(Table_17$`Personal weapons`)

boxplot(Table_17$`Agency count`)
out <- boxplot.stats(Table_17$`Agency count`)$out
mtext(paste("Agency count: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 823] <- mean(Table_17$`Agency count`)

boxplot(Table_17$Population)
out <- boxplot.stats(Table_17$Population)$out
mtext(paste("Population: ", paste(out, collapse = ", ")))
Table_17[Table_17 == 38767853] <- mean(Table_17$Population)
Table_17[Table_17 == 21424937] <- mean(Table_17$Population)
Table_17[Table_17 == 18382202] <- mean(Table_17$Population)
Table_17[Table_17 == 27151685] <- mean(Table_17$Population)



#mean
mean(Table_17$Population)
mean(Table_17$Firearms)

#median
median(Table_17$`Agency count`)

#Range
max(Table_17$Population)-min(Table_17$Population)

#variance
var(Table_17$Population)
var(Table_17$`Agency count`)

#standard deviation
sd(Table_17$Population)
sd(Table_17$`Agency count`)

#Quantile 
quantile(Table_17$Population)
quantile(Table_17$`Agency count`)

#InterQuartile
IQR(Table_17$Firearms)
IQR(Table_17$`Other weapons`)
IQR(Table_17$`Knives or cutting instruments`)


library(ggplot2)
library(ggplot2)
library(mosaicData)

ggplot(Table_17)+geom_bar(aes(x = State, y = Firearms),stat="identity" ,fill = "green")+
  labs(title = "U.S. GUN usage comparison by state",caption = "Data source: FBI/UCR") + coord_flip()














