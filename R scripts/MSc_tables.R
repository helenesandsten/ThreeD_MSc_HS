### tables

# table_predictions <- tibble(
#   "" = c("Aboveground productivity", 
#          "Belowground productivity", 
#          "Decomposition", 
#          "Soil organic matter"),
#   
#   "Warming" = 
#     c("Increase at both sites", 
#       "Decrease at alpine site, increase at sub-alpine site", 
#       "", 
#       ""),
#   
#   "Increased nitrogen deposition" = 
#     c("", 
#       "", 
#       "", 
#       ""),
#   
#   "Grazing" = 
#     c("", 
#       "", 
#       "", 
#       ""))












## table - nitrogen amount in blocks 
nitrogen_table <- tibble(
  "Block" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  "N level" = c(1,6,5,3,10,7,4,8,9,2),
  "N kg/ha/y" = c(0, 5, 1, 0, 150, 10, 0.5, 50, 100, 0))

## table - overview of parameters and response variables 
table_param_respvar_unit <- tibble(
  "Parameter investigated" = c("Aboveground productivity", 
                               "Belowground productivity", 
                               "Decomposition", 
                               "",
                               "",
                               "",
                               "Soil organic matter"),
  
  "Response variables" = c("biomass", 
                           "biomass",
                           "mass loss green tea",
                           "mass loss green tea",
                           "decomposition rate",
                           "stabilization factor",
                           "mass loss"),
  
  "Unit" = c("g/m2", 
             "g/cm3",
             "proportion",
             "proportion",
             "k",
             "S",
             "proportion")
)

table_best_models_all <-  tibble(
  "Site"    = c("Alpine", # 1
                "Alpine", # 2
                "Alpine", # 3
                "Alpine", # 4
                "Alpine", # 5
                "Alpine", # 6
                "Alpine", # 7
                "Alpine", # 8
                "Alpine", # 9
                "Alpine", # 10
                "Alpine", # 11
                "Alpine", # 12
                "Alpine", # 13
                "Alpine", # 14
                "Alpine", # 15
                "Sub-alpine", # 1
                "Sub-alpine", # 2
                "Sub-alpine", # 3
                "Sub-alpine", # 4
                "Sub-alpine", # 5
                "Sub-alpine", # 6
                "Sub-alpine", # 7
                "Sub-alpine", # 8
                "Sub-alpine", # 9
                "Sub-alpine", # 10
                "Sub-alpine", # 11
                "Sub-alpine", # 12
                "Sub-alpine", # 13
                "Sub-alpine", # 14
                "Sub-alpine"  # 15
  ),
  
  
  "Model"   = c("w + n + g",  "w + n * g", "w * n + g", "w * g + n",  "w * n * g",
                "w + n", "w * n", "w + g", "w * g", "n + g", "n * g",
                "w", "n", "g", 
                "no model",
                
                "w + n + g",  "w + n * g", "w * n + g", "w * g + n",  "w * n * g",
                "w + n", "w * n", "w + g", "w * g", "n + g", "n * g",
                "w", "n", "g",
                "no model"),
  "AGP" = c("-", "-", "-", "-", "1", 
            "-", "-", "-", "-", "-", "-",
            "-", "-", "-", 
            "-",
            
            "-", "-", "-", "-", "3", 
            "-", "-", "-", "2", "-", "-",
            "-", "-", "(1)", 
            "-"),
  
  "BGP" = c("-", "-", "-", "-", "-", 
            "-", "2", "-", "-", "-", "-",
            "-", "-", "(3)", 
            "1",
            
            "3", "-", "-", "-", "-", 
            "-", "-", "-", "-", "1", "-",
            "-", "2", "-",
            "-"),
  
  "GT" = c("-", "-", "-", "-", "-", 
           "-", "-", "-", "-", "-", "",
           "(2)", "(3)", "-", 
           "1",
           
           "-", "-", "-", "-", "-", 
           "-", "-", "(3)", "-", "-", "-",
           "(1)", "-", "-",
           "2"),
  
  "RT" = c("-", "-", "-", "-", "-", 
           "-", "-", "-", "-", "-", "-",
           "(2)", "(3)", "-", 
           "1",
           
           "-", "-", "-", "-", "-", 
           "-", "-", "(3)", "-", "-", "-",
           "(1)", "-", "-",
           "2"),
  
  "Dec k" = c("-", "-", "-", "-", "-", 
              "-", "-", "-", "-", "-", "(3)",
              "-", "-", "(2)",
              "1",
              
              "-", "-", "-", "-", "-", 
              "-", "-", "-", "-", "-", "-",
              "-", "-", "-",
              "-"),
  
  "Dec S" = c("-", "-", "-", "-", "-", 
              "2", "-", "3", "-", "-", "-",
              "-", "-", "(1)", 
              "-",
              
              "-", "-", "-", "-", "-", 
              "2", "-", "3", "-", "-", "-",
              "-", "-", "(1)", 
              "-"),
  
  "SOM" = c("-", "-", "-", "-", "-", 
            "2", "-", "(3)", "-", "-", "-",
            "-", "-", "(1)", 
            "-",
            
            "-", "-", "-", "-", "-", 
            "-", "-", "3", "2", "-", "-",
            "-", "-", "(1)", 
            "-")
)

## table - overview of how the top three models performed
table_best_models_top3 <-  tibble(
  "Response" = c("Aboveground productivity", "", "", "Aboveground productivity", "", "",
                 "Belowground productivity", "", "", "Belowground productivity", "", "",
                 "Mass loss green tea", "", "", "Mass loss green tea", "", "",
                 "Mass loss rooibos tea", "", "", "Mass loss rooibos tea", "", "", 
                 "Decomposistion rate k", "", "","Decomposistion rate k", "", "",
                 "Stabilisation factor S", "", "", "Stabilisation factor S", "", "",
                 "Proportion soil organic matter", "", "", 
                 "Proportion soil organic matter", "", ""),
  
  "Site"    = c("Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                "",  # 3
                
                "Alpine", # 1
                "", # 2
                "", # 3
                "Sub-alpine", # 1
                "", # 2
                ""  # 3
  ),
  
  "Rank" = c("1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3.",
             
             "1.", "2.", "3.", 
             "1.", "2.", "3."
  ),
  
  "Model" = c("w * n * g", # agp
              "w * n * g",
              "w * n * g",
              "g",
              "w * g",
              "w * n * g",
              
              "-", # bgp
              "w * n",
              "w",
              "n + g",
              "n",
              "w + n + g",
              
              "-", #green tea
              "w", # x
              "n", # x
              "w", # x
              "-", # 
              "w + g", # low aic
              
              "-", #rooibos tea
              "w", # x
              "n", # x
              "w", # x
              "-", 
              "w + g", # x, low aic
              
              "-", # decomp k
              "g", # x
              "n * g", 
              "", 
              "",
              "",
              
              "w", # decomp s - x
              "w + n", 
              "w + g", # low aic
              "w", # x
              "w + n", # low aic
              "w + g", # low aic
              
              "w", # som - x
              "w + n",
              "w + g", # low aic, x
              "w", # x
              "w * g",
              "w + g" # low aic
  ),
  "Comment" = c("Model output available", 
                "",
                "Delta AIC >2",
                "Model assumptions not met",
                "",
                "Model output available",
                
                "",
                "Model output available",
                "Model assumptions not met",
                "Model output available",
                "",
                "Delta AIC >2",
                
                "", #green tea
                "Model assumptions not met", # x
                "Model assumptions not met", # x
                "Model assumptions not met", # x
                "", # 
                "Delta AIC >2", # low aic
                
                "", #rooibos tea
                "Model assumptions not met", # x
                "Model assumptions not met", # x
                "Model assumptions not met", # x
                "", 
                "Delta AIC >2", # x, low aic
                
                "", # decomp k
                "Model assumptions not met", # x
                "Model output available", 
                "", 
                "",
                "",
                
                "Model assumptions not met", # decomp s - x
                "Model output available", 
                "Delta AIC >2", # low aic
                "Model assumptions not met", # x
                "Delta AIC >2, model output available", # low aic
                "Delta AIC >2", # low aic
                
                "Model assumptions not met", # som - x
                "Model output available",
                "Model assumptions not met", # low aic, x
                "Model assumptions not met", # x
                "Model output available",
                "Delta AIC >2" # low aic
  )
)


table_models_results_top3 <- tibble(
  "Site"    = c("Alpine", # 1
                "", # 2
                "", # 3
                
                "", # 4
                "", # 5
                "", # 6
                
                "", # 7
                "", # 8
                "", # 9
                
                "", # 10
                "", # 11
                "", # 12
                
                "", # 13
                "", # 14
                "", # 15
                
                "", # 16
                "", # 17
                "", # 18
                
                "", # 19
                "", # 20
                "", # 21
                
                "Sub-alpine", # 1
                "", # 2
                "", # 3
                
                "", # 4
                "", # 5
                "", # 6
                
                "", # 7
                "", # 8
                "", # 9
                
                "", # 10
                "", # 11
                "", # 12
                
                "", # 13
                "", # 14
                "", # 15
                
                "", # 16
                "", # 17
                "", # 18
                
                "", # 19
                "", # 20
                "" # 21
),

"Response"= c("AGP", # 1
              "", # 2
              "", # 3
              
              "BGP", # 4
              "", # 5
              "", # 6
              
              "Mass loss GT", # 7
              "", # 8
              "", # 9
              
              "Mass loss RT", # 10
              "", # 11
              "", # 12
              
              "Decomp. k", # 13
              "", # 14
              "", # 15
              
              "Stab. fact. S", # 16
              "", # 17
              "", # 18
              
              "SOM", # 19
              "", # 20
              "", # 21
              
              "AGP", # 1
              "", # 2
              "", # 3
              
              "BGP", # 4
              "", # 5
              "", # 6
              
              "Mass loss GT", # 7
              "", # 8
              "", # 9
              
              "Mass loss RT", # 10
              "", # 11
              "", # 12
              
              "Decomp. k", # 13
              "", # 14
              "", # 15
              
              "Stab. fact. S", # 16
              "", # 17
              "", # 18
              
              "SOM", # 19
              "", # 20
              ""  # 21
),

"Model"= c("w * n * g", # 1 - agp
           "w * n * g", # 2
           "w * n * g", # 3
              
           "null model", # 4 - bgb
           "w * n", # 5
           "(w)", # 6
              
           "null model", # 7 - gt
           "(w)", # 8
           "(n)", # 9
              
           "null model", # 10 - rt
           "(w)", # 11
           "(n)", # 12
           
           "null model", # 13 - k
           "(g)", # 14
           "n * g", # 15
              
           "(w)", # 16 - s
           "w + n", # 17
           "w + g", # 18
           
           "(w)", # 19 - som 
           "w + n", # 20
           "(w + g)", # 21
           
           "(g)", # 1 - agp
           "w * g", # 2
           "w * n * g", # 3
           
           "n + g", # 4 - bgp
           "n", # 5
           "w + n + g", # 6
           
           "(w)", # 7 - gt
           "null model", # 8
           "w + g", # 9
           
           "(w)", # 10 - rt
           "null model", # 11
           "w + g", # 12
           
           "-", # 13 - k
           "-", # 14
           "-", # 15
           
           "(w)",   # 16 - s
           "w + n", # 17
           "w + g", # 18
           
           "(w)",    # 19 - som 
           "w * g",   # 20
           "w + g" # 21
),

"AIC"= c("5716.1", # 1 - agp
        "5716.1", # 2
        "5718.1", # 3
        
        "-716.0", # 4 - bgp
        "-715.1", # 5
        "-715.0", # 6
        
        "-92.4", # 7 - gt
        "-91.2", # 8
        "-90.8", # 9
        
        "-92.4", # 10 - rt
        "-91.2", # 11
        "-90.8", # 12
        
        "-294.6", # 13 - k
        "-293.9", # 14
        "-292.8", # 15
        
        "-121.1", #  - s
        "-120.1", # 17
        "-119.0", # 18
        
        "-17.4", #  - som
        "-17.2", # 20
        "-15.1", # 21
        
        "6012.9", # 1 - agp
        "6013.5", # 2
        "6013.6", # 3
        
        "-635.0", # 4 - bgp
        "-633.9", # 5
        "-632.9", # 6
        
        "-67.6", # 7 - gt
        "-67.0", # 8
        "-65.5", # 9
        
        "-67.6", # 10 - rt
        "-67.0", # 11
        "-65.5", # 12
        
        "-", # 13 - k
        "-", # 14
        "-", # 15
        
        "-140.7", #  - s
        "-138.6", # 17
        "-138.4", # 18
        
        "145.5", #  - som
        "144.1", # 20
        "143.4" # 21
),

"DeltaAIC"= c("0.00", # 1 - agp
         "0.02", # 2
         "2.08", # 3
         
         "0.00", # 4 - bgp
         "0.83", # 5
         "0.98", # 6
         
         "0.00", # 7 - gt
         "1.19", # 8
         "1.60", # 9
         
         "0.00", # 10 - rt
         "1.19", # 11
         "1.60", # 12
         
         "0.00", # 13 - k
         "0.74", # 14
         "1.85", # 15
         
         "0.00", #  - s
         "1.08", # 17
         "2.13", # 18
         
         "0.00", #  - som
         "0.28", # 20
         "2.30", # 21
         
         "0.00", # 1 - agp
         "0.58", # 2
         "0.71", # 3
         
         "0.00", # 4 - bgp
         "1.18", # 5
         "2.17", # 6
         
         "0.00", # 7 - gt
         "0.67", # 8
         "2.14", # 9
         
         "0.00", # 10 - rt
         "0.67", # 11
         "2.14", # 12
         
         "-", # 13 - k
         "-", # 14
         "-", # 15
         
         "0.00", #  - s
         "2.10", # 17
         "2.34", # 18
         
         "0.00", #  - som
         "1.36", # 20
         "2.02" # 21
),

"AICweight"= c("0.373", # 1 - agp
              "0.368", # 2
              "0.132", # 3
              
              "0.226", # 4 - bgp
              "0.149", # 5
              "0.138", # 6
              
              "0.295", # 7 - gt
              "0.163", # 8
              "0.132", # 9
              
              "0.295", # 10 - rt
              "0.163", # 11
              "0.132", # 12
              
              "0.280", # 13 - k
              "0.193", # 14
              "0.111", # 15
              
              "0.372", #  - s
              "0.217", # 17
              "0.128", # 18
              
              "0.312", #  - som
              "0.271", # 20
              "0.099", # 21
              
              "0.186", # 1 - agp
              "0.139", # 2
              "0.130", # 3
              
              "0.305", # 4 - bgp
              "0.169", # 5
              "0.103", # 6
              
              "0.284", # 7 - gt
              "0.203", # 8
              "0.097", # 9
              
              "0.284", # 10 - rt
              "0.203", # 11
              "0.097", # 12
              
              "-", # 13 - k
              "-", # 14
              "-", # 15
              
              "0.469", #  - s
              "0.164", # 17
              "0.146", # 18
              
              "0.340", #  - som
              "0.172", # 20
              "0.124" # 21
),

"R^2"= c(# alpine
  "0.16020", # 1 - agp
  "0.16270", # 2
  "0.16270", # 3
  
  "0.00000",  # 4 - bgp
  "0.09620", # 5
  "0.02034", # 6
  
  "0.00000", # 7 - gt
  "0.01051", # 8
  "0.00597", # 9
  
  "0.00000", # 10 - rt
  "0.01051", # 11
  "0.00597", # 12
  
  "0.00000", # 13 - k
  "0.04634", # 14
  "0.15370", # 15
  
  "0.21670", # 16 - s
  "0.23770", # 17
  "0.22080", # 18
  
  "0.16800", # 19 - som
  "0.19600", # 20
  "0.16800", # 21
  
  # sub-alpine
  "0.10160", # 1 - agp
  "0.10680", # 2
  "0.11860", # 3
  
  "0.14190",  # 4 - bgp
  "0.09073", # 5
  "0.14500", # 6
  
  "0.03565", # 7 - gt
  "0.00000", # 8
  "0.03667", # 9
  
  "0.03565", # 19 - rt
  "0.00000", # 11
  "0.03667", # 12
  
  "-", # 13 - k
  "-", # 14
  "-", # 15
  
  "0.35080", # 16 - s
  "0.35690", # 17
  "0.35310", # 18
  
  "0.10260", # 19 - som
  "0.15180", # 20
  "0.10690"  # 21
),

"R^2 adj"= c(# alpine
  "0.16020", # 1 - agp
  "0.16280", # 2
  "0.16280", # 3
  
  "0.00000",  # 4 - bgp
  "-0.00000", # 5
  "-0.00000", # 6
  
  "0.00000",  # 7 - gt
  "-0.00546", # 8
  "-0.00310", # 9
  
  "0.00000",  # 10 - rt
  "-0.00546", # 11
  "-0.00310", # 12
  
  "0.00000", # 13 - k
  "-0.00000", # 14
  "-0.00002", # 15
  
  "-0.02124", # 16 - s
  "-0.02330", # 17
  "-0.02165", # 18
  
  "-0.67990", # 19 - som
  "-0.79320", # 20
  "-0.68000", # 21
  
  # sub-alpine
  "0.10160", # 1 - agp
  "0.10680", # 2
  "0.11860", # 3
  
  "-0.00000",  # 4 - bgp
  "-0.00000", # 5
  "-0.00000", # 6
  
  "-0.02394", # 7 - gt
  "0.00000", # 8
  "-0.02463", # 9
  
  "-0.02394", # 19 - rt
  "0.00000", # 11
  "-0.02463", # 12
  
  "-", # 13 - k
  "-", # 14
  "-", # 15
  
  "-0.01412", # 16 - s
  "-0.01437", # 17
  "-0.01422", # 18
  
  "-0.00095", # 19 - som
  "-0.01409", # 20
  "-0.00991"  # 21
),

"df"= c("7", # 1 - agp
        "8", # 2
        "9", # 3
        
        "2", # 4 - bgp
        "5", # 5
        "3", # 6
        
        "2", # 7 - gt
        "3", # 8
        "3", # 9
        
        "2", # 10 - rt
        "3", # 11
        "3", # 12
        
        "2", # 13 - k
        "3", # 14
        "5", # 15
        
        "3", #  - s
        "4", # 17
        "4", # 18
        
        "3", #  - som
        "4", # 20
        "4", # 21
        
        "3", # 1 - agp
        "5", # 2
        "9", # 3
        
        "4", # 4 - bgp
        "3", # 5
        "5", # 6
        
        "3", # 7 - gt
        "2", # 8
        "4", # 9
        
        "3", # 10 - rt
        "2", # 11
        "4", # 12
        
        "-", # 13 - k
        "-", # 14
        "-", # 15
        
        "3", #  - s
        "4", # 17
        "4", # 18
        
        "3", #  - som
        "5", # 20
        "4" # 21
)

)






