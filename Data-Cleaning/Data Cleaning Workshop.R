getwd()
setwd("C:/Users/Christopher/Desktop/Data Cleaning Examples")
library(readxl)
library(stringr)

##### Importing data #####
# header, col.names, na.strings, colClasses, stringsAsFactors
# nrow, ncol, length, head, tail, dim, class, which, unique, names, range, table, summary, typeof, str

#read.csv   Example_csv.txt
read.csv("Example_csv.txt")

#read.csv2  Example_csv2.txt
dat <- read.csv2("Example_csv2.txt")

#read.delim   Example_delim.txt
read.delim("Example_delim.txt")

#read.delim2  Example_delim2.txt
read.delim2("Example_delim2.txt")

#read_excel from "readxl"   Example.xlsx
read_excel("Example.xlsx")

##### Type conversion #####
x <- 1:16
mode(x)
class(x)

dim(x) <- c(4,4)
mode(x)
class(x)
is.numeric(x)

mode(x) <- "character"
mode(x)
class(x)

x <- factor(x)
class(x)
mode(x)

##### Recoding factors #####
set.seed(1234)
a <- sample(1:3, size = 10, replace = TRUE)
recode <- c("Red" = 1, "Green" = 2, "Blue" = 3)
factor(a, levels = recode, labels = names(recode))
# sample 1:3, recode colors, put into factor(data, levels, labels)


##### String normalization #####
#library(stringr)
#str_trim (side) and str_pad (width, side, pad)
ex.string <- "   Hello world  "
str_trim(ex.string, side = "left")
str_trim(ex.string, side = "right")
str_trim(ex.string, side = "both")
#Hello world string
#sample PINs

set.seed(123)
PINs <- sample.int(9999, 10)
str_pad(PINs, width = 4, side = "left", pad = "0")

##### String manipulation #####
#grep and grepl
## ignore.case, toupper/tolower, ^(pattern), fixed = T/F
## Regular expressions
#grepl "m", ignore.case, tolower, fixed with "^"
gender <- c("M", "male ", "Female", "fem.")
grepl("m", gender)
grepl("m", gender, ignore.case = TRUE)
grepl("m", tolower(gender))
grepl("^", gender, fixed = TRUE)


#readLines    Example1.txt
b <- readLines("Example1.txt")
comments <- grepl("%", b, fixed = TRUE)
b <- b[!comments]
b <- unlist(strsplit(b, split = ":"))
dim(b) <- c(2, 4)
#remove comments, strsplit, put into a data frame


##### Missing values #####
#RussianRoulette(dat), replace NA with rnorm
set.seed(123)
bad.dat <- RussianRoulette(dat)
bad.dat[which(is.na(bad.dat), arr.ind = TRUE)] <- rnorm(3)

##### Special values #####
#ToInfinityAndBeyond
set.seed(1234)
dat2 <- matrix(rnorm(100), nrow = 25)
infinite.dat <- ToInfinityAndBeyond(dat2)

##### Outliers #####
#ZimbabweInflation and OutlierFinder
set.seed(1234)
inflated.dat <- ZimbabweInflation(dat2)
inflated.dat.vector <- as.vector(inflated.dat)
subset(inflated.dat.vector, subset = (inflated.dat.vector < -2))
subset(inflated.dat.vector, subset = (inflated.dat.vector > 2))
OutlierFinder(inflated.dat, type = "vector")
OutlierFinder(inflated.dat, type = "data frame")

##### Inconsistencies #####
#Use your judgement
d <- c(1:5, "six", 7:20)
f <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
#What do you think f[14] is?