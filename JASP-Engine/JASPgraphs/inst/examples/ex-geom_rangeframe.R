library(ggplot2)
library(JASPgraphs)

# an example plot
graph <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

# add bty = 'n' for x and y-axes.
graph +
 geom_rangeframe() +
 themeJaspRaw()

# automatically done by themeJasp
themeJasp(graph)

# only draw the x-axis line
graph +
  geom_rangeframe(sides = "b") +
  themeJaspRaw()

# coord_flip does not interact with sides
graph + 
  coord_flip() +
  geom_rangeframe(sides = "b") + 
  themeJaspRaw()

# add axis lines right and above of plot:
graph + 
  scale_x_continuous(sec.axis = sec_axis(~ log10(.))) + 
  scale_y_continuous(sec.axis = sec_axis(~ log10(.))) + 
  geom_rangeframe(sides = "trbl") + 
  themeJaspRaw()

# set x-axis to above the plot and draw a line there
graph + 
  scale_x_continuous(position = "top") + 
  geom_rangeframe(sides = "tl", panelInfo = list(t = "x.major")) + 
  themeJaspRaw()
