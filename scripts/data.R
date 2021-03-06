##### data.R #######################################################################################

##### Data Types -----------------------------------------------------------------------------------

x <- 6
y <- 6L
z <- TRUE

typeof(x)
typeof(y)
typeof(z)

is.logical(x)
is.double(x)


##### Data Conversion ------------------------------------------------------------------------------

# Implicit conversion:

w <- TRUE
x <- 4L
y <- 5.8
z <- w + x + y

typeof(z)

# Explicit conversion. Note loss of data:

as.integer(z)


##### Class ----------------------------------------------------------------------------------------

class(TRUE)
class(pi)
class(4L)
class(1:4)

x <- as.Date('2010-01-01')
class(x)
typeof(x)


##### Mode -----------------------------------------------------------------------------------------

# Ignore


##### Dates and times ------------------------------------------------------------------------------

x <- as.Date('06-30-2010')
# ambigious!

x <- as.Date('30-06-2010')
class(x)
typeof(x)

# Stick with this format:
x <- as.Date('2010-06-30')
class(x)
typeof(x)


##### More on dates --------------------------------------------------------------------------------

x <- Sys.Date()
class(x)

y <- Sys.time()
class(y)


##### Factors --------------------------------------------------------------------------------------

myColors <- c("Red", "Blue", "Green", "Red", "Blue", "Red")
typeof(myColors)
class(myColors)

myFactor <- factor(myColors)
typeof(myFactor)
class(myFactor)

is.character(myFactor)
is.character(myColors)


##### Altering Factors -----------------------------------------------------------------------------

# This probably won't give you what you expect:

myOtherFactor <- c(myFactor, "Orange")
myOtherFactor

# And this will give you an error:

myFactor[length(myFactor) + 1] <- "Orange"

# Must do things in two steps:

myOtherFactor <- factor(c(levels(myFactor), "Orange"))
myOtherFactor[length(myOtherFactor) + 1] <- "Orange"


##### Avoid factors --------------------------------------------------------------------------------


##### Questions ------------------------------------------------------------------------------------


# 1. Create a logical, integer, double and character variable.

# 2. Can you create a vector with both logical and character values?

# 3. What happens when you try to add a logical to an integer? An integer to a double?


##### Answers --------------------------------------------------------------------------------------

# 1. Create a logical, integer, double and character variable.

myLogical <- TRUE
typeof(myLogical)

myInteger <- 1:4
typeof(myInteger)

myDouble <- 3.14
typeof(myDouble)

myCharacter <- "Hello!"
typeof(myCharacter)

# 2. Can you create a vector with both logical and character values?

y <- c(myLogical, myCharacter)
typeof(y)

# 3. What happens when you try to add a logical to an integer? An integer to a double?

y <- myLogical + myInteger
typeof(y)

y <- myInteger + myDouble
typeof(y)


##### From vectors to matrices and lists -----------------------------------------------------------

##### A matrix -------------------------------------------------------------------------------------

myVector <- 1:100
myVector

myMatrix <- matrix(myVector, nrow = 10, ncol = 10)
myMatrix

myOtherMatrix <- myVector
myOtherMatrix

dim(myOtherMatrix)
length(myOtherMatrix)
dim(myOtherMatrix) <- c(10, 10)
dim(myOtherMatrix)

identical(myMatrix, myOtherMatrix)

myMatrix <- matrix(nrow = 10, ncol = 10)

library(rblocks)

block_grid(10, 10, type = "matrix")

dim(myMatrix) <- c(25, 4)
block_grid(25, 4, type = "matrix")


##### Matrix metadata ------------------------------------------------------------------------------

myMatrix <- matrix(nrow = 10, ncol = 10, data = sample(1:100))

colnames(myMatrix) <- letters[1:10]
head(myMatrix, 3)

rownames(myMatrix) <- tail(letters, 10)
head(myMatrix, 3)


##### Data access for a matrix ---------------------------------------------------------------------

myMatrix[2, ]

myMatrix[, 2]

myMatrix[2]

myMatrix[22]


##### Matrix summary -------------------------------------------------------------------------------

sum(myMatrix)

colSums(myMatrix)

rowSums(myMatrix)

colMeans(myMatrix)


##### Lists ----------------------------------------------------------------------------------------

x <- list()
typeof(x)

x[[1]] <- c("Hello", "there", "this", "is", "a", "list")
x[[2]] <- c(pi, exp(1))

summary(x)
str(x)
make_block(x)


##### [ vs. [[ -------------------------------------------------------------------------------------

y <- list()
y[[1]] <- "Lou Reed"
y[[2]] <- 45

x[[3]] <- y
make_block(x)


##### List metadata --------------------------------------------------------------------------------

y[[1]] <- c("Lou Reed", "Patti Smith")
y[[2]] <- c(45, 63)

names(y) <- c("Artist", "Age")
y

y$Artist
y$Age


##### lapply ---------------------------------------------------------------------------------------

myList <- list(firstVector = c(1:10),
               secondVector = c(89, 56, 84, 298, 56),
               thirdVector = c(7,3,5,6,2,4,2))

myList

lapply(myList, mean)
lapply(myList, median)
lapply(myList, sum)


##### Questions ------------------------------------------------------------------------------------

# 1. Create a list with two elements. Have the first element be a vector with 100 numbers. Have the
# second element be a vector with 100 dates. Give your list the names: “Claim” and “Accident Date”.

# 2. What is the average value of the first list element?


##### Answers --------------------------------------------------------------------------------------

# 1. Create a list with two elements. Have the first element be a vector with 100 numbers. Have the
# second element be a vector with 100 dates. Give your list the names: “Claim” and “Accident Date”.

myList <- list()

myList$Claims <- rlnorm(100, log(10000))

myList$AccidentDate <- sample(seq.Date(as.Date('2008-01-01'), as.Date('2017-12-31'), length.out = 1000), 100)

# 2. What is the average value of the first list element?

mean(myList$Claims)


##### Data frames ----------------------------------------------------------------------------------

##### Creating a data frame ------------------------------------------------------------------------

set.seed(1234)
State <- rep(c("TX", "NY", "CA"), 10)
State

EarnedPremium <- rlnorm(length(State), meanlog = log(50000), sdlog = 1)
EarnedPremium <- round(EarnedPremium, -3)

Losses <- EarnedPremium * runif(length(EarnedPremium), min = 0.4, max = 0.9)

df <- data.frame(State, EarnedPremium, Losses, stringsAsFactors = FALSE)


##### Basic properties of a data frame -------------------------------------------------------------

summary(df)

str(df)

names(df)

colnames(df)

length(df)

dim(df)

nrow(df)

ncol(df)

head(df)

head(df, 2)

tail(df)


##### Referencing ----------------------------------------------------------------------------------

df[2,3]

df[2]

df[2,]

df[2, -1]

df$EarnedPremium

# Columns of a data frame may be treated as vectors:

df$EarnedPremium[3]

df[2:4, 1:2]

df[, "EarnedPremium"]

df[, c("EarnedPremium", "State")]


##### Ordering -------------------------------------------------------------------------------------

order(df$EarnedPremium)

df <- df[order(df$EarnedPremium), ]


##### Altering and adding columns ------------------------------------------------------------------

df$LossRatio <- df$EarnedPremium / df$Losses

df$LossRatio <- 1 / df$LossRatio


##### Eliminating columns --------------------------------------------------------------------------

df$LossRatio <- NULL

df <- df[, 1:2]


##### rbind, cbind ---------------------------------------------------------------------------------

dfA <- df[1:10,]
dfB <- df[11:20, ]

rbind(dfA, dfB)

dfC <- dfA[, 1:2]

cbind(dfA, dfC)


##### Merging --------------------------------------------------------------------------------------

dfRateChange <- data.frame(State = c("TX", "CA", "NY"), RateChange = c(0.05, -0.1, 0.2))

df <- merge(df, dfRateChange)


##### Altering column names ------------------------------------------------------------------------

df$LossRatio <- with(df, Losses / EarnedPremium)
names(df)

colnames(df)[4] <- "Loss Ratio"
colnames(df)


##### Subsetting - The easy way --------------------------------------------------------------------

dfTX <- subset(df, State == "TX")

dfBigPolicies <- subset(df, EarnedPremium >= 50000)


##### Subsetting - The hard(ish) way ---------------------------------------------------------------

dfTX <- df[df$State == "TX", ]

dfBigPolicies <- df[df$EarnedPremium >= 50000, ]


##### Subsetting - Yet another way -----------------------------------------------------------------

whichState <- df$State == "TX"
dfTX <- df[whichState, ]
dfTX

whichEP <- df$EarnedPremium >= 50000
dfBigPolicies <- df[whichEP, ]


##### Summarizing ----------------------------------------------------------------------------------

sum(df$EarnedPremium)

sum(df$EarnedPremium[df$State == "TX"])

aggregate(df[, -1], list(df$State), sum)

aggregate(cbind(EarnedPremium, Losses, `Loss Ratio`, LossRatio) ~ State, data = df, sum)


##### Summarizing visually - 1 ---------------------------------------------------------------------

dfByState <- aggregate(df$EarnedPremium, list(df$State), sum)
colnames(dfByState) <- c("State", "EarnedPremium")

barplot(dfByState$EarnedPremium, names.arg = dfByState$State, col = "blue")


##### Summarizing visually - 2 ---------------------------------------------------------------------

dotchart(dfByState$EarnedPremium, dfByState$State, pch = 19)


##### Advanced data frame tools --------------------------------------------------------------------


##### Reading data ---------------------------------------------------------------------------------

getwd()
setwd("C:/Users/scott.sobel/Documents/Docs/CAS/2018-03-19 RPM/raw_rpm")

myData <- read.csv("SomeFile.csv", stringsAsFactors = FALSE)
myData
str(myData)
head(myData)


##### Reading from Excel ---------------------------------------------------------------------------

library(XLConnect)

wbk <- loadWorkbook("myWorkbook.xlsx")

df <- readWorksheet(wbk, "someSheet")


##### Reading from the web - 1 ---------------------------------------------------------------------

URL <- "http://www.casact.org/research/reserve_data/ppauto_pos.csv"

df <- read.csv(URL, stringsAsFactors = FALSE)


##### Reading from the web - 2 ---------------------------------------------------------------------

library(XML)

URL <- "http://www.pro-football-reference.com/teams/nyj/2012_games.htm"

games <- readHTMLTable(URL, stringsAsFactors = FALSE)


##### Reading from a database ----------------------------------------------------------------------

library(RODBC)

myChannel <- odbcConnect(dsn = "MyDSN_Name")

df <- sqlQuery(myChannel, "SELECT stuff FROM myTable")

View(df)


##### Questions ------------------------------------------------------------------------------------


# 1. Load the data from “StateData.csv” into a data frame.

# 2. Which state has the most premium?


##### Answers --------------------------------------------------------------------------------------


# 1. Load the data from “StateData.csv” into a data frame.

# 2. Which state has the most premium?


##### END CODE #####################################################################################