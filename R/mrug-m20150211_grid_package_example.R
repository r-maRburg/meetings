library(grid)
library(ggplot2)

?qplot

plt <- qplot(mpg, wt, data=mtcars)

vp1 <- viewport(x = 0, y = 1,
                height = 0.9, width = 1,
                just = c("left", "top"),
                name = "vp_plot")

grid.newpage()
pushViewport(vp1)
#grid.newpage()
print(plt, newpage = FALSE)
upViewport(0)

vp2 <- viewport(x = 0, y = 0,
                height = 0.1, width = 1,
                just = c("left", "bottom"),
                name = "vp_caption")

pushViewport(vp2)
#grid.newpage()
grid.text(label = "Fig 1: blaaaaaaaa blaaaaaaaaa",
          x = 0.1, just = c("left", "centre"))
upViewport(0)
