# Normal Distribution




#-------------------------------------------------------------------#

# Chi-Squared Distribution

# plot the PDF
curve(dchisq(x, df = 3), 
      xlim = c(0, 10), 
      ylim = c(0, 1), 
      col = "blue",
      ylab = "",
      main = "p.d.f. and c.d.f of Chi-Squared Distribution, M = 3")

# add the CDF to the plot
curve(pchisq(x, df = 3), 
      xlim = c(0, 10), 
      add = TRUE, 
      col = "red")

# add a legend to the plot
legend("topleft", 
       c("PDF", "CDF"), 
       col = c("blue", "red"), 
       lty = c(1, 1))

# Increasing Degrees-of-Freedom

# plot the density for M=1
curve(dchisq(x, df = 1), 
      xlim = c(0, 15), 
      xlab = "x", 
      ylab = "Density", 
      main = "Chi-Square Distributed Random Variables")

# add densities for M=2,...,7 to the plot using a 'for()' loop 
for (M in 2:7) {
  curve(dchisq(x, df = M),
        xlim = c(0, 15), 
        add = T, 
        col = M)
}

# add a legend
legend("topright", 
       as.character(1:7), 
       col = 1:7 , 
       lty = 1, 
       title = "D.F.")

#-------------------------------------------------------------------#

# Student-t Distribution

#plot standard normal density
curve(dnorm(x),
      xlim = c(-4,4),
      xlab = "x",
      lty = 2,
      ylab = "Density",
      main = "Densities of t Distributions")

#plot the t density for M = 2
curve(dt(x, df=2),
      xlim = c(-4,4),
      col = 2,
      add = T)

#plot the t density for M = 4
curve(dt(x, df = 4),
      xlim = c(-4,4),
      col  = 3,
      add  = T)

#plot the t density for M = 25
curve(dt(x, df = 5),
      xlim = c(-4,4),
      col  = 4,
      add  = T)

#add a legend
legend("topright",
       c("N(0,1)", "M=2", "M=4", "M=25" ),
       col = 1:4,
       lty = c(2,1,1,1))

#-------------------------------------------------------------------#

# F - Distribution

pf(2, df1 = 3, df2 = 14, lower.tail = F)

# define coordinate vectors for vertices of the polygon
x <- c(2, seq(2, 10, 0.01), 10)
y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
curve(df(x ,3 ,14), 
      ylim = c(0, 0.8), 
      xlim = c(0, 10), 
      ylab = "Density",
      main = "Density Function")

# draw the polygon
polygon(x, y, col = "orange")
