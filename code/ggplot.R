library(ggplot2)
data <- data.frame(x = 1:20000, y = listre$k)
qplot(x, y, data = data, geom = "path", colour = y)

p <- ggplot(data = data, aes(x = y))
p + geom_bar(aes(fill = factor(y)))

data <- data.frame(x = 1:20000, y = listre$r[, 1])
p <- ggplot(data = data, aes(x = x, y = y))
p + geom_path(colour = "blue") + ylab("r1")

p <- ggplot(data = data, aes(x = y))
p + geom_histogram(aes(fill = ..count..)) + xlab("r1")+ scale_fill_gradient("Count", low = "green", high = "red")

data <- data.frame(x = 1:10, low.r =log(apply(re,2, mean) - 2 * apply(listre$r, 2, sd)),
  upper.r = log(apply(re,2, mean) + 2 * apply(listre$r, 2, sd)), 
  log.r = log(apply(re,2, mean)), 
  log.r.true = log(r)
)
p <- ggplot(data, aes(x = x, y = log.r.true)) 
p + geom_path(linetype = 1, colour = 2) + ylim(c(-4, -1.5)) + 
   geom_path(aes(y = log.r), colour = 3, linetype = 2) +
   geom_path(aes(y = low.r), colour = 3, linetype = 4) +
   geom_path(aes(y = upper.r), colour = 3, linetype = 4) +
   geom_point(aes(y = low.r), colour = 3, shape = 4) +
   geom_point(aes(y = upper.r), colour = 3, shape = 4) +
