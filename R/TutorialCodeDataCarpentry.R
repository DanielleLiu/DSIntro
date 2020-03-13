# create directory; recommended directory structure for data,
# data output and figure output;
dir.create("data")
dir.create("data_output")
dir.create("fit_output")

# down load the data file from the given url to the provided
# directory.
download.file("https://ndownloader.figshare.com/files/11492171", 
    "data/SAFI_clean.csv", mode = "wb")
# R is friendly to next line; if enter at the proper space,
# i.d., if not interuptting the argument variable name will be
# fine; if interuptting the middle of the string will create a
# string with '\n'

# data science package.
install.packages("tidyverse")
# package for nice formatting of the code
install.packages("formatR", repos = "http://cran.rstudio.com")

# load the library into the current working env
library(tidyverse)
library(formatR)
tidy_dir()  #tidy all scripts under the current directory
# tidy the file with given name
tidy_file("script.R")

# assignment shortcut 'option' AND '-'
width <- 10
length <- 12
area <- width * length

args(round)  #to get help of the function
`?`(round)  #get documents in the help window
# or have cursor at a function, then press Fn+F1, to load doc in
# the help window
round(pi, 2)  #usually, optional arguments last.
round(digits = 2, x = pi)
round(5236.65265, digits = 4)
# can provide arguments in different orders, in this case,
# specify the arguments name

hh_members <- c(1, 2, 3, 4)  #c creates the vector: numerical vector
res_wall_type <- c("muddaub", "burntbricks", "sunbricks")  #string vector

length(res_wall_type)  #outputs the length of the vector
class(res_wall_type)  #outputs the class type of the objects in the vector

str(hh_members)  # string description of the struture, 
# doesn't quite outputs a sring rep of the elements like in
# java.
class(hh_members)

pos <- c("apple", "bike", "tv")  #character vector
pos <- c(pos, "banana")  #add to the end of the vector
pos <- c("celeery", pos)  #add to the beginning of the vector

# also can create logical, integer, complex, or raw (bitstream)
# vectors
typeof(pos)  #similar to class, gives the type of the elements in the vectors

numChar <- c(1, 2, 3, "a")  #auto converts to a string array
charLogical <- c("a", "b", "c", TRUE)
sumArray <- c(numChar, charLogical)  #string arrayr
typeof(sumArray)  #will be character
sumArray

sumArray[2]  #indexing the 2nd elements; 1 based index
sumArray[c(1, 2)]  #indexing the 1st and 2nd elements
sumArray[c(1, 2, 3, 1)]  #repeated indexing allowed.
sumArray[2:4]  #works too, index out from 2 to 4
# logical indexing, if not specified, default to true; in this
# case, will only ignore the 3rd element
sumArray[c(TRUE, TRUE, FALSE)]

cond <- hh_members > 3  #outputs a logical array with true at the ones satisfying the condition
hh_members[cond]
hh_members[hh_members >= 1 & hh_members < 3]  #index out[1,3)
hh_members[hh_members > 4 & hh_members < 3]  #empty, outputs numeric(0)
# check if pos is in the given array and outputs a logical array
# of size matching the given array, the one after %in% not pos,
# TRUE for elements in the given array that's also in pos.
pos %in% c("apple", "bicycle", "motorcycle")

# support missing data
rooms <- c(2, 1, 1, NA, 4)
mean(rooms)  #will give NA
mean(rooms, na.rm = TRUE)  # ignore na and compute the rest
rooms[!is.na(rooms)]  #index out the none NA terms
omitted <- na.omit(rooms)
typeof(omitted)  #the omitted now is an array of double
rooms[complete.cases(rooms)]  #same, index out non na terms
rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
median(rooms, na.rm = TRUE)
sum(rooms > 2, na.rm = TRUE)  #count the true output
# or create a new array of the true output and then length needs
# the &, otherwise the NA will also be indexed out
room_above_w <- rooms[rooms > 2 & !is.na(rooms)]
length(room_above_w)

# load data
library(tidyverse)
# load from CSV and conver the NULL entries to na
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")
# using read.csv will read in tibble format (tidyverse package),
# read.csv will read like normal format read_delim(file,
# delimeter) allow loading with different delimeter

# print the first 6 lines, in the case of tidyverse, will also
# only read the columns that fit into the window.
head(interviews)
# print out the column title
names(interviews)

# outputs the classes, here the tibble is an inherited object,
# so will print out all the polymorphisms
class(interviews)
length(interviews)  #column count
dim(interviews)  #row then col
nrow(interviews)  #row counts
ncol(interviews)  #col counts
str(interviews)  #detail of the table, class, length, content of the column
summary(interviews)  #summary stats of each var.

# indexing/subsetting data frames,
element13 <- interviews[1, 2]  #row, column
# the output is a list/a 1x1 tibble itself,
typeof(element13)
class(element13)

interviews[1]  #index the 1st column
interviews[1, ]  #first row, all columns
interviews[1:3, 7]  #row 1-3 and column 7
interviews["village"]  #index the column with the name
# all the above outputs a tibble

interviews[["village"]]  #outputs as a vector
interviews$village  #index out the village column, output a vector
interviews$village[1:3]  #row 1-3 of village column, as a vector output

# Factor, to deal with categorical data, stored as integers
# (levels) associated with labels createa a factory type, by
# default assign cement to level 1, and earth to level 2
# (alphabetical)
respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))  #a factor with 2 earth, 2 cement
class(respondent_floor_type)  #output: factor
levels(respondent_floor_type)  #the categories
nlevels(respondent_floor_type)  #number of categories
# if non alphabetical order is preferred, specify levels= when
# creating factor
respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", 
    "cement"))
respondent_floor_type
# change the level name easily, replace cement with brick
levels(respondent_floor_type)[2] <- "brick"
respondent_floor_type  #now the data will be replaced with brick
# convert to string vector
as.character(respondent_floor_type)
as.numeric(respondent_floor_type)

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(year_fct)  # will give the numerical rep of the levels
levels(year_fct)
# to get the original array back as a numeric vector, run below
temp <- as.numeric(levels(year_fct))
class(temp)
temp <- as.numeric(levels(year_fct))[year_fct]  #equivalent
class(temp)

# example of a possibly categorical variable
memb_assoc <- interviews$memb_assoc
# only have, yes, no or NA, since there is NA, clean up the NAs
# first as a recognizable category, otherwise the NA will not be
# recognized as a category
memb_assoc[is.na(memb_assoc)] <- "undetermined"
# now convert to facotr, should have 3 levels.
memb_assoc <- as.factor(memb_assoc)
levels(memb_assoc)

# plot, factor, default to histogram, follow the order of the
# levels
plot(memb_assoc)

# specify the categorical variable with the given order
memb_assoc <- factor(memb_assoc, levels = c("no", "yes", "undetermined"))
plot(memb_assoc)

class(interviews$interview_date)  #POSIXct, some standard thingy
dates <- interviews$interview_date
class(dates)
str(dates)
# load the lubridate package, part of tidyverse, but not loaded
# by default
library(lubridate)
# the output message indicates, some object is masked, which
# means that there is overwritting and the one provided in
# lubridate will be preferred
day(dates[1])
# extract date into 3 separate columns if the column doesn't
# exit, will create automatically
interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)
interviews$year2 <- year(dates)
interviews$year2 <- month(dates)
drops <- "year2"  #can use a vector, or just the string directly
# index out all rows, and columns not in drops, then assign back
# to interviews.
interviews <- interviews[, !(names(interviews) %in% drops)]

# select 3 columns out, output a tibble
select(interviews, village, no_membrs, years_liv)
# select data with God for village, output a tibble
interviews2 <- filter(interviews, village == "God")
interviews_god <- select(interviews2, no_membrs, years_liv)
# Equivalently, nested
interviews_god <- select(filter(interviews, village == "God"), no_membrs, 
    years_liv)
interviews_god

#pipe structure, filter first, then select
interviews %>% 
  filter(village == 'God') %>%  
  select(no_membrs, years_liv)

# create a new column using mutate, people_per_room, with the value computed as given
interviews %>%
  mutate(people_per_room = no_membrs/rooms)%>%
  View() # in this case, view what's being operated right now

interviews %>% 
  filter(!is.nan(memb_assoc)) %>%
  mutate(people_per_room = no_membrs/rooms)

#create a new column total_meal, select the village and the new column
#out with total_meal value above 20, and summarize per village
#save the results to a new table interviewsNew
interviewsNew <- interviews %>% 
  #operating off interviews
  mutate(total_meals=no_membrs*no_meals)%>%
  filter(total_meals > 20)%>%
  select(village, total_meals) %>%
  group_by(village) %>% #group by village, still 50 rows
  summarize(total=sum(total_meals)) #take only 1 sum per village

interviews %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs)) %>% 
  #summarize using mean and outputs the mean to the new variable 
  #mean_no_membrs and list in final output
  filter(memb_assoc != 'NA') %>% #remove the NA terms
  arrange(desc(mean_no_membrs)) #sort descending order by the mean

#count of different types of village, sort=TRUE, 
#sort output in descending order of n
interviews %>% count(village, sort=FALSE)

interviews %>% 
  select(village) %>% 
  group_by(village)%>% 
  count(village, sort=TRUE)

interviews_spread <- interviews %>%
  mutate(wall_logical = TRUE) %>%  
  #create a temp column 
  spread(key =respondent_wall_type, 
         value = wall_logical, fill=FALSE) 
#spread the respondent_wall_type into different columns, 
#spread the corresponding value from wall_logical to the new columns 

#gather columns from burntbricks to sunbricks to wall_type
#and fill in wall_logical, will result in a table 4xprevious Rows (4 different bricks)
#previous 1 item-1wall type, now will be 4 rows for each wall type with 3 false and 1 true for the corresponding wall type
interview_gather <- interviews_spread %>%
  gather(key="wall_type",value="wall_logical",
         burntbricks:sunbricks)
  
interview_items_owned <- interviews %>%
  separate_rows(items_owned, sep=";") %>% 
  #separate entries in the given using delimeter to diff rows
  #now will have many more rows, 1 for each item per house
  mutate(items_logical = TRUE) %>%
  spread(key=items_owned, value=items_logical, fill=FALSE)
  #will return back to original row count, as items listed as columns now

interview_items_owned %>%
  filter(bicycle == TRUE) %>%
  group_by(village)%>%
  count(bicycle == TRUE) 
#this seems to just count bicycles, regardless of true false
#without running filter, will output bicycle true and false for 
#each village, but we only care about the true ones

#get the mean item counts of each vilalge
interview_items_owned %>%
  #for each row, counts the trues from column bicycle to tv
  mutate(number_items = rowSums(select(., bicycle:television))) %>%
  group_by(village) %>%
  #group the results by vilalge, using summarize methods, mean
  summarise(mean_items = mean(number_items))
  
#get the mean item counts of each vilalge
itemCount <- interviews %>%
  separate_rows(items_owned, sep=";")%>%
  mutate(items_logical = TRUE)%>%
  spread(key = items_owned, value = items_logical, fill = FALSE)%>%
   #for each row, counts the trues from column bicycle to tv
  separate_rows(months_lack_food, sep=";")%>%
  mutate(lack_logical = TRUE) %>%
  spread(key = months_lack_food, value = lack_logical, fill=FALSE)%>%
  mutate(totalMonthLack = rowSums(select(.,Apr:Sept))) %>%
  mutate(number_items = rowSums(select(.,bicycle:television)))

#drop variables
rm(list = c("interviewNes","element13"))

# export
write_csv(itemCount, path="data_output/interviews_plotting.csv")

#scatter plots
ggplot(itemCount, aes(x = no_membrs, y=number_items)) +
  geom_jitter(aes(color = village), alpha=0.5) 
  #add jitter to allow better viewing
  #alpha: transparency of the points; aes(color=colName), color by column

#box plot
ggplot(data = interview_items_owned,  # what data to use
       aes(x=respondent_wall_type, y=rooms)) +  # what to use for x and y axis, then add
  geom_boxplot(alpha = 0.5) + #use box plot, then add
  geom_jitter(alpha = 0.5, aes(color = memb_assoc)) #also add the scatter points with jitter
#the plots layer will be in the order it was added.

#histogram
ggplot(data = itemCount, 
       aes(x=respondent_wall_type)) + geom_bar(aes(fill = village), position="dodge") 
#the aes, allows showing data for each histogram by village with diff colors,
#by adding position = dodge, will separate bars for each village for each wall type

# bar plot with diff grouping 
ggplot(data = itemCount, 
       aes(x=respondent_wall_type)) + 
  geom_bar(aes(fill = village), position="dodge") + #organized by village, then by respondent_wall_type
  facet_wrap(~village) #now will organize by village, then sub group by wall type
