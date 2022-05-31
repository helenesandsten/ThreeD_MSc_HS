## transformation plan 



tranformation_plan <- list(
  

  ## ABOVEGROUND BIOMASS -------------------------------------
  # import biomass and fix it
  tar_target(
    name = agb.df,
    command = agb.df <- agb.raw.df %>% 
      mutate_if(is.character, as.factor) %>%
      mutate(origBlockID = as.factor(origBlockID),
             origPlotID = as.factor(origPlotID),
             destBlockID = as.factor(destBlockID),
             destPlotID = as.factor(destPlotID)) %>% 
      mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
      # calculating biomass per m2 to standardize amounts
      mutate(biomass_m2 = ((biomass / area_cm2) * 10000)) %>% 
      filter(year == 2021) %>%
      # removing plots with missing forbs biomass
      filter(turfID != "147 WN9C 194") %>%
      # removing plots with unknown area cut
      filter(!turfID == "147 WN9C 194") %>% # same plot as missing forbs biomass
      filter(!turfID == "103 WN5N 172") %>%
      filter(!turfID == "128 WN7N 184") %>%
      filter(!turfID == "87 WN1N 164") %>%
      filter(!turfID == "159 WN2N 200") %>%
      filter(!turfID == "112 WN3N 176") %>%
      # renaming variables
      mutate(grazing = recode(grazing, 
                              "C" = "Control", "I" = "Intensive", 
                              "M" = "Medium", "N" = "Natural"),
             warming = recode(warming, 
                              "A" = "Ambient", "W" = "Warming")) %>% 
      # reordering variables 
      mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa")),
             grazing = factor(grazing, 
                              levels = c("C" = "Control", 
                                         "M" = "Medium",
                                         "I" = "Intensive",
                                         "N" = "Natural"))) %>% 
      # changing grazing into cont. variable too reduce degrees of freedom in models
      mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                     (grazing == "Medium") ~ 2,
                                     (grazing == "Intensive") ~ 4)) 
    ),
  
    tar_target(
        name = agb.alp.df,
        command = {
          agb.alp.df <- agb.df %>% 
            # removing grazing level N because it is too different from other 
            # levels to be included in analysis
            filter(!grazing == "Natural") %>% 
            filter(origSiteID == "Lia")
        }),
    
    tar_target(
      name = agb.sub.df,
      command = {
        agb.sub.df <- agb.df %>% 
          # removing grazing level N because it is too different from other 
          # levels to be included in analysis
          filter(!grazing == "Natural") %>% 
          filter(origSiteID == "Joa")
      }),

### ROOTS / BELOWGROUND BIOMASS --------------------------------

# import root biomass and fix it
tar_target(
  name = roots.df,
  command = {
    roots.df <- roots.raw.df %>%
      mutate(dateRIC_washed = ymd(dateRIC_washed),  
             date_roots_dried = ymd(date_roots_dried), # change format to date
             root_mass_g = total_mass_g - alutray_mass_g, 
             root_mass_cm3 = (root_mass_g/(pi*(1.6)^2*RIC_length_cm))) %>% # calculate volume
      mutate_if(is.character, as.factor) %>%
      # deciding order of origSiteID, 1. Lia 2. Joa
      mutate(grazing = recode(grazing, 
                              "C" = "Control", "M" = "Medium",
                              "I" = "Intensive","N" = "Natural"),
             warming = recode(warming, 
                              "A" = "Ambient", "W" = "Warming")) %>% 
      mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa")),
             grazing = factor(grazing, 
                              levels = c("C" = "Control", 
                                         "M" = "Medium",
                                         "I" = "Intensive",
                                         "N" = "Natural"))) %>% 
      # changing grazing into cont. variable too reduce degrees of freedom in models
      mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                     (grazing == "Medium") ~ 2,
                                     (grazing == "Intensive") ~ 4)) %>% 
      left_join(NitrogenDictionary, by = "Nlevel") %>% 
      mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
      mutate(days_buried = recover_date_2021 - burial_date)  

  }), 

tar_target(
  name = roots.alp.df,
  command = {
    ## Making dataset for alpine site
    roots.alp.df <- roots.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Lia") 
  }
),

tar_target(
  name = roots.sub.df,
  command = {
    ## Making dataset for sub-alpine site 
    roots.sub.df <- roots.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Joa") 
  }
),


  ### TEABAG / DECOMPOSITION BIOMASS --------------------------------
  
  tar_target(
    name = mean_string_bag_mass,
    command = {
      ## fixup mini dataset - bag and string weight 
      mean_string_bag_mass <- mean(string.bag.weight.df$weight_bag_string)
    }
  ), 

tar_target(
  name = teabag.df,
  command = {
    ## fixup main dataset - teabags
    teabag.df <- teabag.raw.df %>%  
      mutate_if(is.character, as.factor) %>% 
      mutate(origBlockID = as.factor(origBlockID),
             origPlotID = as.factor(origPlotID),
             destBlockID = as.factor(destBlockID),
             destPlotID = as.factor(destPlotID)) %>% 
      #tea_type = as.logical(tea_type)) %>% 
      mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% # for better visualization
      # filter out damaged teabags 
      mutate(comment_3 = ifelse(is.na(comment_2), 1, 0)) %>% 
      filter(comment_3 == 1) %>% # do not run this line if you want to remove tea bag pairs
      # filter out all teabag pairs with 1 or 2 damaged teabags
      # group_by(teabag_ID) %>% 
      # mutate(comment_3 = sum(comment_3)) %>% 
      # filter(comment_3 == 2) %>% 
      # calculating days buried 
      mutate(days_buried = recover_date - burial_date, 
             days_overdue = days_buried - 90) %>% 
      # reordeing factors 
      mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
      mutate(grazing = recode(grazing, 
                              "C" = "Control", "I" = "Intensive", 
                              "M" = "Medium", "N" = "Natural"),
             warming = recode(warming, 
                              "A" = "Ambient", "W" = "Warming")) %>% 
      # changing grazing into cont. variable too reduce degrees of freedom in models
      mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                     (grazing == "Medium") ~ 2,
                                     (grazing == "Intensive") ~ 4)) %>% 
      # remove weight of bag and string from teabag weight 
      mutate(preburial_weight_g = preburial_weight_g - mean_string_bag_mass,
             post_burial_weight_g = post_burial_weight_g - mean_string_bag_mass,
             # renaming and recalculating mass loss 
             mass_loss_g = weight_loss_g, 
             mass_loss_g = preburial_weight_g - post_burial_weight_g,
             # calculating proportion of mass loss   
             mass_loss_proportion = 1 - (post_burial_weight_g / preburial_weight_g))  
  }
), 

tar_target(
  name = decomp.df,
  command = {
    Hydrolysable_fraction_green <- 0.842
    Hydrolysable_fraction_red <- 0.552
    
    decomp.df <- teabag.df %>% 
      select(teabag_ID, tea_type, post_burial_weight_g, preburial_weight_g, incubation_time,
             origSiteID, destSiteID, destBlockID, origBlockID, warming, grazing, grazing_lvl,
             Namount_kg_ha_y, recover_date, burial_date) %>%
      # rearranging data to have teabag pairs on same row for calculations
      pivot_wider(names_from = tea_type, 
                  values_from = c(post_burial_weight_g, preburial_weight_g)) %>%
      mutate(incubation_time = as.numeric(recover_date - burial_date), 
             # calculations for the teabag index
             fraction_decomposed_green = 1 - post_burial_weight_g_green/preburial_weight_g_green,
             fraction_remaining_green = post_burial_weight_g_green/preburial_weight_g_green,
             fraction_remaining_red = post_burial_weight_g_red/preburial_weight_g_red,
             # stabilisation rate 
             S = 1 - (fraction_decomposed_green / Hydrolysable_fraction_green),
             predicted_labile_fraction_red = Hydrolysable_fraction_red * (1 - S),
             # decomposition rate 
             k = log(predicted_labile_fraction_red / (fraction_remaining_red - (1 - predicted_labile_fraction_red))) / incubation_time)
  }
), 

tar_target(
  name = tea.green.alp.df,
  command = {
    ## Making dataset for alpine site
    tea.green.alp.df <- teabag.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Lia")
  }
), 

tar_target(
  name = tea.green.sub.df,
  command = {
    ## Making dataset for sub-alpine site 
    tea.green.sub.df <- teabag.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Joa") 
  }
),


tar_target(
  name = tea.red.alp.df,
  command = {
    tea.red.alp.df <- teabag.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Lia") 
  }
),


tar_target(
  name = tea.red.sub.df,
  command = {
    tea.red.sub.df <- teabag.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Joa")
  }
), 

tar_target(
  name = decomp.k.alp.df,
  command = {
    ## Making dataset for alpine site
    decomp.k.alp.df <- decomp.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(!is.na(k)) %>% 
      filter(origSiteID == "Lia") 
  }
), 

### SOIL DATA ---------------------------------------------
tar_target(
  name = decomp.s.alp.df,
  command = {
    ## Making dataset for alpine site
    decomp.s.alp.df <- decomp.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(!is.na(S)) %>% 
      filter(origSiteID == "Lia")
  }
), 

tar_target(
  name = decomp.s.sub.df,
  command = {
    ## Making dataset for sub-alpine site 
    decomp.s.sub.df <- decomp.df %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural") %>% 
      filter(!is.na(S)) %>% 
      filter(origSiteID == "Joa")
  }
), 

### SOIL ---------------------------------------------------
tar_target(
  name = soil.df,
  command = {
    soil.df <- soil.raw.df %>% 
      # removing unwanted columns and fixing weird ones 
      select(-c(...14, ...15, ...16, ...17, ...18)) %>%
      mutate(alutray_ID = str_replace(alutray_ID, "  ", " ")) %>% 
      mutate(burn_mass1 = if_else(
        alutray_ID == "Vik W B5 I" & burn_mass1 == 11.5771, # row and col you want to change
        12.5771, # what you change it to 
        burn_mass1)) %>% # this needs to be here, ask Aud
      separate(col = alutray_ID, # separate column into several
               into = c("destSiteID", "warming", "destBlockID", "grazing"), " ") %>% 
      mutate(destBlockID = as.numeric(str_remove(destBlockID, "B")), # remove letter B
             destSiteID) %>% 
      select(-turfID) %>% # remove column 'turfID' because it is empty
      mutate_if(is.character, as.factor) %>% 
      mutate(#destPlotID = as.numeric(destPlotID),
        destBlockID = as.numeric(destBlockID),
        destSiteID = as.character(destSiteID)) %>% 
      # adding nitrogen levels to blocks 
      mutate(Nlevel = case_when(destBlockID == 1 ~ 1, # new_col = (case_when(old_c == old, ~new))
                                destBlockID == 2 ~ 6, 
                                destBlockID == 3 ~ 5, 
                                destBlockID == 4 ~ 3,
                                destBlockID == 5 ~ 10,
                                destBlockID == 6 ~ 7, 
                                destBlockID == 7 ~ 4, 
                                destBlockID == 8 ~ 8, 
                                destBlockID == 9 ~ 9, 
                                destBlockID == 10~ 2)) %>% 
      # adding nitrogen amount to Nlevels
      left_join(NitrogenDictionary, by = "Nlevel") %>% 
      mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
      # adding column origin site
      mutate(origSiteID = case_when((destSiteID == "Lia" & warming == "A") ~ "Lia",
                                    (destSiteID == "Joa" & warming == "W") ~ "Lia", 
                                    (destSiteID == "Joa" & warming == "A") ~ "Joa",
                                    (destSiteID == "Vik" & warming == "W") ~ "Joa")) %>% 
      filter(!destSiteID == 'NA') %>% 
      # reordering factors to make plots less confusing 
      mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
      mutate(grazing = recode(grazing, 
                              "C" = "Control", "M" = "Medium",
                              "I" = "Intensive","N" = "Natural"),
             warming = recode(warming, 
                              "A" = "Ambient", "W" = "Warming")) %>% 
      mutate(grazing = factor(grazing, levels = c("C" = "Control", 
                                                  "M" = "Medium",
                                                  "I" = "Intensive",
                                                  "N" = "Natural"))) %>% 
      # changing grazing into numbered levels (Richard)
      mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                     (grazing == "Medium") ~ 2,
                                     (grazing == "Intensive") ~ 4)) %>% 
      # change column names to informative names
      rename(alutray_mass = alutray_weight, # new name = old name
             wetmass = wet_mass_g,
             drymass_1_55 = dry_mass1, 
             drymass_2_sieved = dry_mass2, 
             drymass_3_sieved_105 = dry_mass3, 
             drymass_4_87 = dry_mass4, 
             porcelain_mass = porcelain_weight,
             burnmass_1_550 = burn_mass1, 
             burnmass_2_950 = burn_mass2,
             root_stone_mass = total_cf_mass) %>%
      # removing container weight from mass weights 
      mutate(drymass_4_87 = drymass_4_87 - porcelain_mass,
             burnmass_1_550 = burnmass_1_550 - porcelain_mass,
             burnmass_2_950 = burnmass_2_950 - porcelain_mass) %>% 
      # removing impossible values i.e. typos from dataset 
      filter(!drymass_4_87 < burnmass_1_550,
             !burnmass_1_550 < burnmass_2_950) %>% 
      mutate(prop_sample_left = burnmass_1_550 / drymass_4_87) %>% 
      mutate(prop_org_mat = 1 - prop_sample_left) %>% 
      # removing grazing level N because it is too different from other 
      # levels to be included in analysis
      filter(!grazing == "Natural")
  }
), 

tar_target(
  name = soil.alp.df,
  command = {
    ## Making dataset for alpine site
    soil.alp.df <- soil.df %>% 
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Lia")
  }
), 

tar_target(
  name = soil.sub.df,
  command = {
    ## Making dataset for sub-alpine site 
    soil.sub.df <- soil.df %>% 
      filter(!grazing == "Natural") %>% 
      filter(origSiteID == "Joa")
  }
)
)

###############
# , 
# 
# tar_target(
#   name = ,
#   command = {
#     
#   }
# )


