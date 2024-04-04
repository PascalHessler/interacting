# Define the layout matrix
layout_matrix <- matrix(c(1, 2), nrow = 2, byrow = TRUE)
layout(layout_matrix)


  par(oma=c(1,1,1,1))
  par(mar=c(0,4.1,4.1,2.1))


# Plot 1: Lines of interest (top plot)
plot(x, y, type = 'l', main = "Lines of Interest",bty='n',xaxt='n') # replace x, y with your data vectors
# Add more plotting commands here as needed

  par(mar=c(5,4.1,4.1,2.1))

# Plot 2: Histogram (bottom plot)
# If you're using a histogram function
hist(x, main = "Sample Size")

rect(par('usr')[1:4])

rect(par('usr')[3],par('usr')[4],par('usr')[1],par('usr')[2])
     
     
     ,par('usr')[3] 0,0,1,1)

f=t1$frequencies

barplot(t(f))