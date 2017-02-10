setwd('F:/ytwang/Dropbox/USLab/pesticides/BioSim')

install.packages('scatterplot3d')
library(scatterplot3d)

scatter_plot_3d <- pca_center_distance
with(scatter_plot_3d, {
  scatterplot3d(pc1,   # x axis
                pc2,     # y axis
                pc3,    # z axis
                main="3-D distance of different dataset")
})
country <- c("USA", "Japan", "Europe", "China", "Overlap")

with(scatter_plot_3d, {
  s3d <- scatterplot3d(pc1, pc2, pc3,        # x y and z axis
                       color="blue", pch=19,        # filled blue circles
                       type="h",  lty.hplot=2,                  # vertical lines to the x-y plane
                       main="3-D Scatterplot of different dataset",
                       xlab="Center of PCA1",
                       ylab="Center of PCA2",
                       zlab="Center of PCA3")
  s3d.coords <- s3d$xyz.convert(pc1, pc2, pc3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=country,               # text to plot
       cex=1.2, pos=4)           # shrink text 50% and place to right of points)
})


# create column indicating point color
mtcars$pcolor[mtcars$cyl==4] <- "red"
mtcars$pcolor[mtcars$cyl==6] <- "blue"
mtcars$pcolor[mtcars$cyl==8] <- "darkgreen"
with(mtcars, {
  s3d <- scatterplot3d(disp, wt, mpg,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot Example 4",
                       xlab="Displacement (cu. in.)",
                       ylab="Weight (lb/1000)",
                       zlab="Miles/(US) Gallon")
  s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=row.names(mtcars),       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Number of Cylinders",
         c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
})