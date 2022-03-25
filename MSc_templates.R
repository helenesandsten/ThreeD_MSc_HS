### FIGURE TEMPLATES

# SCATTER PLOT
plotname <- ggplot(data = DF, aes(x = , y = )) +
  geom_point(color = "blue", size = 3, alpha = 0.5) +
  labs( # if you put = waiver() it will fill it with default values 
    title = waiver(),
    subtitle = waiver(),
    caption = waiver(),
    tag = waiver(),
    alt = waiver(),
    alt_insight = waiver()
  )

xlab(label)

ylab(label)
