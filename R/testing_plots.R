
library(ggplot2)
# install.packages("ggforce")
# library(ggforce)


# population_structure 


# circles are drawn centered at x and y

# data(mpg)
# ggplot(mpg, aes(displ, hwy)) + geom_circle(radius=0.1) + geom_point()

rad <- 0.75
cells <- 6
mutant <- 2
cell_size <- 15

population_structure_df <- data.frame(color_reps=sample(c(rep("wild type",cells-mutant),rep("mutant",mutant)),replace = F),
                                      rate=rep(1,cells))
population_structure_df$angle <- seq(90,360+90,length.out = nrow(population_structure_df)+1)[1:nrow( population_structure_df)]

population_structure_df$x_pos <- rad * cos(population_structure_df$angle*pi/180)  
population_structure_df$y_pos <- rad * sin(population_structure_df$angle*pi/180)  

ggplot(data = population_structure_df, aes(x=x_pos,y=y_pos)) + geom_point(aes(fill=color_reps),size=cell_size,shape=21,stroke=2) + theme_no_axes() + coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + scale_color_discrete(name="Cell type")


