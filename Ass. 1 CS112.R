install.packages("MASS")
#Install the relevant libraries - do this one time
install.packages("data.table")

### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)
# dimensions of the data set
dim(foo)
# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.
# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)
# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
{
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))
}

# Now R knows that these columns are comprised of dates
# for example...  Replicate this yourself...
foo[3,12]
foo[4,12]
foo[3,12] - foo[4,12]

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
new_foo <- foo[-which.have.NAs, ]
# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.

# --------------------
# SETUP
# Restrict foo data so that CirculationDate >= "2008-01-01" and CirculationDate has no NA values.
# Simplified single step:

filtered_CD <- new_foo[na.omit(which((new_foo$CirculationDate >= as.Date("2008-01-01")))),]
filtered_CD

# (1) When projects are approved, they are approved for a certain period of time (until the time of
# "original completion date"). While projects are active, this "original" completion date is 
# often pushed out (extended), and then there is a "revised" completion date.

# You have been told that project duration at approval is generally about 
# 2 years (24 months). In other words, (purportedly) when projects are approved, the difference 
# between the original project completion date and the the approval date is (supposedly) 
# approximately 24 months.

# (a) Is this claim true? Explain.
# Has project duration at approval changed over time (consider projects circulated earlier
# and circulated later). Be sure to discuss mean durations, median durations, and the
# interquartile range of durations (using the "quantile" function). 
# Approximate suggested length: 3-5 sentences
x <- na.omit(filtered_CD$ApprovalDate)
duration <- na.omit(filtered_CD$OriginalCompletionDate) - na.omit(filtered_CD$ApprovalDate)
duration
mean_duration <- mean(duration)
mean_duration
sd(duration)
mean_months <- round(mean_duration/30)
print(paste("The mean duration of months:", mean_months))

# (b) How does original planned project duration differ from actual duration (if actual duration is 
# measured as the duration between "ApprovalDate" and "RevisedCompletionDate"?)  Once again, use
# means, medians, and interquartile ranges to explain your results. 
# Approximate suggested length: 3-5 sentences
# 1.b) actual duration vs planned project duration
real_duration <- (na.omit(filtered_CD$RevisedCompletionDate) - na.omit(filtered_CD$ApprovalDate))
real_duration
real_mean_duration <- mean(real_duration)
real_mean_duration
real_mean_months <- round(real_mean_duration/30)
print(paste("The mean duration is ", real_mean_months, "months, therefore projects take on average ", 41-24, "months longer than anticipated."))
median(real_duration)
quantile(real_duration)
print("The interquartile range is also strongly skewed to the right, therefore we can infer that most projects take longer than expected.")

# (2) What % of projects that have ratings were rated 0?
# What % were rated 1? What % were rated 2? What % were rated 3? Answer these questions using a table
# or a figure. Provide a title AND an explanatory sentence or two that provides the numerical % results
# rounded to the nearest percentage-point.

Rating_full <- na.omit(filtered_CD$Rating)
Rating_full
prop.table(table(Rating_full))

# (3) Repeat problem 2, but this time exclude all PPTA projects. PPTA projects are more prone to 
# negative ratings, because after a certain point in time only the low-rated PPTA projects required
# ratings.  PPTA stands for "Project Preparatory Technical Assistance" and it is basically a project
# intended to set up a loan (often a very large multi-million-dollar loan). Only PPTAs that fail to 
# "eventuate" to a loan are rated, which is why they are usually rated negatively.

is_PPTA <- filtered_CD$Type == "PPTA"
which_PPTA <- which(is_PPTA == TRUE)
noPPTA <- filtered_CD[-which_PPTA, ]
noPPTA
new_Rating <- na.omit(noPPTA$Rating)
new_Rating
prop.table(table(new_Rating))

# (4) Identify the top 25% of projects by "Revised.Amount" and the bottom 25% of projects by 
# "RevisedAmount". ("RevisedAmount" shows the final project budget.)
# Compare the ratings of these projects. Can you draw a causal conclusion about the effect of 
# budget size on ratings? Why or why not? 
# Hint: Compare the characteristics of the two project groupings,
# e.g., "Dept", "Division", "Cluster", "Country"
# Approximate suggested length: 3-5 sentences.

filtered_CD$RevisedAmount
quantile(filtered_CD$RevisedAmount)

bottom_25 <- filtered_CD[na.omit(which(filtered_CD$RevisedAmount >= 0.009 & filtered_CD$RevisedAmount <= 0.400)),]
bottom_25
top_25 <- filtered_CD[na.omit(which(filtered_CD$RevisedAmount >= 1.000 & filtered_CD$RevisedAmount <= 29.860)),]
top_25
top_25$Rating

# This will be relevent later to compare how different Top and Bottom 25 are from the rest of the data
whats_left <- filtered_CD[na.omit(which(filtered_CD$RevisedAmount >= 0.400 & filtered_CD$RevisedAmount <= 1.000)),]
whats_left
mean(top_25$RevisedAmount)
mean(bottom_25$RevisedAmount)
mean(whats_left$RevisedAmount)

mean(top_25$Rating)
mean(bottom_25$Rating)

# __________
# SET UP FOR DATA VISUALIZATION (1):
# This function generates 4 datasets containing the Revised Project Amount 
# for the projects rated 0, 1, 2, and 3 in the Top 25 projects of the post-2008
# foo dataset.

top_box_data_generator <- function(x) {
  is_x <- top_25$Rating == deparse(substitute(x))
  which_top_x <- (which(is_x == TRUE))
  top_x_amount <- top_25$RevisedAmount[which_top_x]
  return(top_x_amount)
}

# Similarly, for the Bottom 25 projects in the dataset:

bottom_box_data_generator <- function(x) {
  is_x <- bottom_25$Rating == deparse(substitute(x)) #deparse used for including a character string as a functional argument
  which_bottom_x <- (which(is_x == TRUE))
  bottom_x_amount <- bottom_25$RevisedAmount[which_bottom_x]
  return(bottom_x_amount)
}

# These functions can now be used to create boxplots for Top 25 and Bottom 25
# Top 25 grouped Revised Amount data:
top_zero <- top_box_data_generator(0)
mean(top_zero)
sd(top_zero)
top_one <- top_box_data_generator(1)
mean(top_one)
sd(top_one)
top_two <- top_box_data_generator(2)
mean(top_two)
sd(top_two)
top_three <- top_box_data_generator(3)
mean(top_three)
sd(top_three)

# Bottom 25 grouped Revised Amount data:
bottom_zero <- bottom_box_data_generator(0)
mean(bottom_zero)
bottom_one <- bottom_box_data_generator(1)
mean(bottom_one)
bottom_two <- bottom_box_data_generator(2)
mean(bottom_two)
bottom_three <- bottom_box_data_generator(3)
mean(bottom_three)

# Create data visualization for Top 25% Boxplot:
boxplot(top_zero, top_one, top_two, top_three, xaxt = "n", col="gray", xlab="Rating Level", ylab="Revised Amount")
title(main = "Boxplot for Revised amount per Rating within Top 25% projects")
text(x= 1, y= 28, labels= "mean: 1.52", col="red")
text(x= 2, y= 28, labels= "mean: 2.05", col="purple")
text(x= 3, y= 28, labels= "mean: 2.20", col="blue")
text(x= 4, y= 28, labels= "mean: 2.40", col="dark green")

# Create data visualization for Bottom 25% Boxplot:
boxplot(bottom_zero, bottom_one, bottom_two, bottom_three, xaxt = "n", col="gray", xlab="Rating Level", ylab="Revised Amount")
title(main = "Boxplot for Revised amount per Rating within Bottom 25% projects")
text(x= 1, y= 0.28, labels= "mean: 0.28", col="red")
text(x= 2, y= 0.28, labels= "mean: 0.28", col="purple")
text(x= 3, y= 0.28, labels= "mean: 0.27", col="blue")
text(x= 4, y= 0.28, labels= "mean: 0.27", col="dark green")

# ----------
# Sort Top and Bottom 25 by Department:

sort(prop.table(table(top_25$Dept))) 
# Sorted table with ascending order of departments which appear the most
# Top three departments are "SERD" (29.8%), "SDCC" (16.39%), and "SARD" (13.44%)

sort(prop.table(table(bottom_25$Dept)))
# Top three departments are "SARD" (18.11%), "SERD" (17.92%), and "EARD" (15.09%)

# Sort Top and Bottom 25 by Country:
sort(prop.table(table(top_25$Country))) 
sort(prop.table(table(bottom_25$Country)))
