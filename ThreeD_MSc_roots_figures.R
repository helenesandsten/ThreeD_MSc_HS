## FIGURES - ROOTS

source("ThreeD_MSc_roots_fixup.R")

names(roots.df)
str(roots.df)

# plot of rootmass and RIC depth
roots_plot1 <- ggplot(data = roots.df, 
                      aes(x = root_mass_g, y = RIC_length_cm), 
                      color = warming) + 
  geom_point(size = 3, alpha = 0.5) +
  theme_bw(base_size = 20) +
  labs(title = "Root mass and RIC length", 
       x = "Root mass (g)",
       y = "RIC length (cm)")
roots_plot1


# plot root biomass - warming 
plot_roots_w <- roots.df %>% 
  filter(grazing == "C") %>% # only non-grazed pltos
  filter(Nlevel == 1 & 4 & 10) %>% # zero N 
  ggplot(data = roots.df, mapping = aes(x = warming, y = root_mass_g, fill = warming)) +
  geom_boxplot(fill = c("#fecc5c", "#fd8d3c")) +
  theme_bw(base_size = 20) +
  labs(title = "Effect of warming on root growth", x = "warming", y = "Root mass (g)") 
plot_roots_w

# plot root biomass - nitrogen
plot_roots_n <- roots.df %>% 
  filter(warming == "A") %>% # only non-grazed pltos
  filter(grazing == "C") %>% # zero N 
  ggplot(data = roots.df, mapping = aes(x = Nlevel, y = root_mass_g, fill = Nlevel)) +
  geom_point(fill = Nlevel) +
  theme_bw(base_size = 20) +
  labs(title = "Effect of Nitrogen on root growth", x = "N level", y = "Root mass (g)") 
plot_roots_n


