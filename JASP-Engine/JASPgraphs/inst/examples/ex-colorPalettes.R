library(ggplot2)

# use colors directly
colors <- JASPcolors(10, "colorblind")
plot(rnorm(10), col = colors, pch = 16, cex = 2)

colFun <- JASPcolors(palette = "colorblind", asFunction = TRUE)
plot(rnorm(10), col = colFun(10), pch = 16, cex = 2)

# use colorscales
e0 <- 1:10
df <- data.frame(x = e0, y = e0, z = e0, g = factor(e0))
ggplot(df, aes(x = x, y = y, color = z, fill = z)) +
  geom_point() +
  scale_JASPcolor_continuous() +
  scale_JASPfill_continuous()

ggplot(df, aes(x = x, y = y, color = g, fill = g)) +
  geom_point() +
  scale_JASPcolor_discrete() +
  scale_JASPfill_discrete()
