myFunction <- function(x, y){
  x + y
}
CircleArea <- function(radius){
  area <- (2 * pi * radius) ^ 2
  area
}
QuadRoot <- function(a, b, c){
  disc <- b^2 - 4 * a * c
  result <- -2 * b - sqrt(disc)
  result <- result / (2 * a)
  result
}
## if (something) {
## 
## } else {
## 
## }
## for (i in 1:5) {
##   print(i)
## }
## 
## for (i in seq_len(5)){ print(letters[i]) }
x <- 2005:2010
seq_along(x)

for (i in seq_along(x)) print(x[i])

for (i in seq_len(5)) print(paste("This is line #", i))
## while (x != 'q') {
## 
## }
myIndex <- 1
while (letters[myIndex] != 'q') {
  myIndex <- myIndex + 1
}
myOtherIndex <- which(letters == 'q')
all.equal(myIndex, myOtherIndex)
mySumSq <- function(x, y){
  result <- (x - y) ^ 2
  sum(result)
}

x <- 1:4
y <- 10:13
mySumSq(x, y)
