## FIGURES - ROOTS

source("ThreeD_MSc_roots_fixup.R")

names(roots.df2)
str(roots.df2)

# plot of rootmass and RIC depth
roots_plot1 <- ggplot(data = roots.df2, 
                      aes(x = root_mass_g, y = RIC_length_cm), 
                      color = warming) + 
  geom_point(size = 3, alpha = 0.5) +
  theme_bw(base_size = 20) +
  labs(title = "Root mass and RIC length", 
       x = "Root mass (g)",
       y = "RIC length (cm)")

roots_plot1
