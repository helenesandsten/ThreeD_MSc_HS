### figures plan 

source("R scripts/analysis_plan.R")

figure_plan <- list(
  
  ## import root biomass and fix it
  tar_target(
    name = ,
    command = {
      
    }),
  
  ### ABOVEGROUND BIOMASS -------------------------------------
  tar_target(
    name = plot_agb,
    command = {
      plot_agb <- agb.df %>% 
        group_by(Namount_kg_ha_y, origSiteID, warming, grazing, Nlevel, turfID) %>% 
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
        filter(!grazing == "Natural") %>%
        summarise(biomass_m2 = sum(biomass_m2)) %>% 
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                             y = biomass_m2, 
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 2) + 
        theme_minimal(base_size = 16) + 
        theme(legend.title = element_blank(),
              legend.position = "bottom", 
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) + 
        scale_fill_manual(values = colors_w) + 
        scale_shape_manual(values = c(21, 25)) + 
        #scale_linetype_manual(values = c("longdash", "solid")) + 
        labs(title = "Aboveground productivity", 
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
             y = bquote(Biomass~(g~m^-2))) + 
        facet_grid(origSiteID ~ grazing) +
        geom_smooth(method = "lm", size = 1)
    }),

  ### ROOTS / BELOWGROUND BIOMASS --------------------------------
  tar_target(
    name = plot_bgb,
    command = {
      plot_bgb <- roots.df %>%
        group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>%
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
        filter(!grazing == "Natural") %>%
        summarise(root_mass_cm3 = mean(root_mass_cm3)) %>%
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                             y = root_mass_cm3,
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 2) + 
        theme_minimal(base_size = 16) + 
        theme(legend.title = element_blank(),
              legend.position = "bottom", 
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) + 
        scale_fill_manual(values = colors_w) + 
        scale_shape_manual(values = c(21, 25)) + 
        #scale_linetype_manual(values = c("longdash", "solid")) + 
        labs(title = "Belowground productivity",
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = bquote(Root~mass~(g/cm^3))) +
        facet_grid(origSiteID ~ grazing) +
        geom_smooth(method = "lm", size = 1)
    }),
  
  
  ### TEABAG / DECOMPOSITION  --------------------------------
  
  tar_target(
    name = plot_decomp_k,
    command = {
      plot_decomp_k <- decomp.df %>% 
        group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
        #filter(!grazing == "Natural") %>%
        filter(!origSiteID == "Sub-alpine") %>%
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                             y = k, 
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 4) + 
        theme_minimal(base_size = 20) + 
        theme(legend.title = element_blank(),
              legend.position = "bottom", 
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) + 
        scale_fill_manual(values = colors_w) + 
        scale_shape_manual(values = c(21, 25)) + 
        #scale_linetype_manual(values = c("longdash", "solid")) + 
        labs(title = "Decomposition rate k", 
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
             y = bquote(k)) + 
        facet_grid(origSiteID ~ grazing) +
        geom_smooth(method = "lm", size = 1)
    }),
  
  tar_target(
    name = plot_teabags_green,
    command = {
      plot_teabags_green <- teabag.df %>%
        group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
        #filter(!turfID == "1 WN1M 84") %>%
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>%
        # filter(!grazing == "Natural") %>%
        mutate(tea_w_color = case_when(
          (tea_type == "green") ~ "Green tea",
          (tea_type == "red") ~ "Rooibos tea")) %>%
        # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>%
        filter(tea_type == "green") %>%
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                             y = mass_loss_proportion,
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 4) +
        theme_minimal(base_size = 20) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) +
        scale_fill_manual(values = colors_w) +
        scale_shape_manual(values = c(21, 25)) +
        ylim(0.25, 1) +
        labs(title = "Mass loss of green tea",
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = bquote(Mass~loss~proportion)) +
        # geom_line() +
        geom_smooth(method = "lm") +
        facet_grid(origSiteID ~ grazing)
    }),
  
  tar_target(
    name = plot_teabags_red,
    command = {
      plot_teabags_red <- teabag.df %>%
        group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
        #filter(!turfID == "1 WN1M 84") %>%
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>%
        #filter(!grazing == "Natural") %>%
        mutate(tea_w_color = case_when(
          (tea_type == "green") ~ "Green tea",
          (tea_type == "red") ~ "Rooibos tea")) %>%
        # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>% 
        filter(tea_type == "red") %>% 
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                             y = mass_loss_proportion,
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 4) + 
        theme_minimal(base_size = 20) + 
        theme(legend.title = element_blank(),
              legend.position = "bottom", 
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) + 
        scale_fill_manual(values = colors_w) + 
        scale_shape_manual(values = c(21, 25)) + 
        ylim(0.25, 1) +
        labs(title = "Mass loss of rooibos tea",
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
             y = bquote(Mass~loss~proportion)) + 
        # geom_line() +
        geom_smooth(method = "lm") +
        facet_grid(origSiteID ~ grazing) 
    }),
  
  ### SOIL  ---------------------------------------------
  
  tar_target(
    name = plot_soil,
    command = {
      plot_soil <- soil.df %>% 
        group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
        mutate(origSiteID = case_when(
          (origSiteID == "Lia") ~ "Alpine",
          (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
        # summarise(prop_org_mat = sum(prop_org_mat)) %>% 
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                             y = prop_org_mat, 
                             color = warming,
                             fill = warming,
                             #linetype = warming,
                             shape = warming)
        ) +
        geom_point(size = 4) + 
        theme_minimal(base_size = 20) + 
        theme(legend.title = element_blank(),
              legend.position = "bottom", 
              legend.box = "horizontal") +
        scale_color_manual(values = colors_w) + 
        scale_fill_manual(values = colors_w) + 
        scale_shape_manual(values = c(21, 25)) + 
        #scale_linetype_manual(values = c("longdash", "solid")) + 
        labs(title = "Proportion of soil organic matter", 
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
             y = bquote(Proportion~organic~material)) + 
        facet_grid(origSiteID ~ grazing) +
        geom_smooth(method = "lm", size = 1)
    })
)

# # 
# tar_target(
#   name = data_,
#   command = 
# )

