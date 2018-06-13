######Plotting#####

### Graphical parameters
?par
opar <- par(mfrow = c(1, 1))
par(opar)

# library(help = "graphics")

## abline                  Add Straight Lines to a Plot
## arrows                  Add Arrows to a Plot
## axis                    Add an Axis to a Plot
## barplot                 Bar Plots
## boxplot                 Box Plots
## boxplot.matrix          Draw a Boxplot for each Column (Row) of a Matrix
## curve                   Draw Function Plots
## dotchart                Cleveland's Dot Plots
## frame()                 Create / Start a New Plot Frame
## grid                    Add Grid to a Plot
## hist                    Histograms
## legend                  Add Legends to Plots
## lines                   Add Connected Line Segments to a Plot
## matplot                 Plot Columns of Matrices
## pie                     Pie Charts
## plot                    Generic X-Y Plotting
## points                  Add Points to a Plot
## segments                Add Line Segments to a Plot
## stem                    Stem-and-Leaf Plots
## text                    Add Text to a Plot
## title                   Plot Annotation


### Plots
# plot(x, y, type, main, sub, xlab, ylab, xaxt, yaxt, col, pch, lty, xlim, ylim)
# axis(side, at, col)   "b, p, l, c, s, n"
# abline(a, b, h, v)
# arrows(x0, y0, x1, y1, length = 0.25, angle = 30, code = 2)
# segments(x0, y0, x1 = x0, y1 = y0)
# points(x, y, type)
# grid(nx = NULL, ny = nx)
# title(main, sub, xlab, ylab)
# women dataset
women
plot(women)
plot(women, xaxt = "n", yaxt = "n")
axis(3, at = seq(60, 80, by = 0.50))
abline(lm(women$weight~women$height))
title(main = "Here is our title", sub = "This is a subtitle")
plot(women, main = "Here is our title", xlab = "This is our x-axis", ylab = "This is our y-axis", type = "b", pch = 16, col = "orange")
arrows(58, 115, 72, 164, col = "green")
segments(63, 129, 67, 142, col = "red")
points(60, 150, pch = 16, col = "blue")

### Boxplots
# boxplot(x, range = 1.5, pch, col, add = T/F) and boxplot.matrix
# text(x, y, col, labels)
# women, as.matrix(trees)
boxplot(women)
boxplot(as.matrix(trees), col = rainbow(3), pch = 16)
text(3, max(trees$Volume)-10, col = "orange", "This is an outlier!")

### Matrix Plots
# matplot(x, y, type, lty, pch, col), matpoints, matlines
# legend(x, legend, col, pch, cex)
# as.matrix(trees)
matplot(as.matrix(trees), type = "l", lty = 1, col = rainbow(3))
legend(x = "left", legend = c("Girth", "Height", "Volume"), pch = 16, col = rainbow(3), cex = 1)

### Curves
# curve(expr, from, to, n, type, col)
# abline
curve(x^2)
abline(h = 0, v = 0, lty = 2, col = "grey40")
curve(x^2 - 5, from = -5, to = 5)
abline(h = 0, v = 0, lty = 2, col = "grey40")


### Barplots
# barplot(height, width = 1, density = T/F)
# women$weight
barplot(women$height, col = rainbow(15))

### Histogram
# hist(x, breaks, freq / prob = T/F)
# women$weight
hist(women$weight, breaks = 14, prob = TRUE)

### Dot charts
# dotchart(x, labels, group)
# women$weight
dotchart(women$weight)

### Stem-and-Leaf Plots
# stem(x, scale, width)
stem(women$weight)

### Pie Charts
# pie(x, labels, col)
d <- c(0.40, 0.10, 0.25, 0.10, 0.15)
d.names <- LETTERS[1:5]
pie(d, d.names, col = rainbow(5))

#chart of R colors
demo("colors")
?rainbow #demo at bottom
