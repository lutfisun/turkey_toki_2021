bir <- c(0)
iki <- c(0)
uch <- c(0)
dor <- c(0)

for (t in 1:30) {
  x <- round((1.08^t)*(1000))
  bir[t] <- x
}

for (t in 1:25) {
  x <- round((1.08^t)*(10000))
  iki[t] <- x
}

for (t in 1:20) {
  x <- round((1.08^t)*(30000))
  uch[t] <- x
}

for (t in 1:15) {
  x <- round((1.08^t)*(50000))
  dor[t] <- x
}

sum(bir)
sum(iki)
sum(uch)
sum(dor)

sum(bir, iki, uch, dor)