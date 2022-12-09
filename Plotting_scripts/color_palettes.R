######################
# Functions with the color palettes for plots
#
# monica.golumbeanu@unibas.ch
# created 15.03.2020
#####################
library(ggplot2)
library(RColorBrewer)

# Color palette for the sensitivity analysis
create_color_palette_sens = function() {
    p = c("Halflife", "Half-life", "PI Halflife", "TB Halflife", "Efficacy", "PI Efficacy", "TB Efficacy", "Coverage", "Access", "Access to treatment", "Drug halflife", "Drug efficacy", "Preprandial efficacy", "Postprandial efficacy")
    categories = factor(p, levels = p)
    # myColors = brewer.pal(length(categories),"Set1")
    myColors = c("#6CC3B9", "#6CC3B9", "#6CC3B9", "#c7e9b4", "#3792BF", "#3792BF", "#a6bddb", "#0D539C", "#fec44f", "#fec44f", "#efedf5", "#bcbddc", "#3792BF", "#3792BF") #"#20546F"
    names(myColors) = levels(categories)
    return(myColors)
    # colScale = scale_colour_manual(name = "grp",values = myColors)
}

# Color palette for the interventions
create_color_palette_int = function() {
    # p = c("Passive anti-infective", "Active anti-infective", 
    #       "Active transmission-blocking", "Blood stage clearance", 
    #       "Pre- and postprandial killing", "Preprandial killing", 
    #       "Postprandial killing", "MDA + Passive anti-infective", 
    #       "MDA + Active anti-infective", "MDA + Active transmission-blocking",
    #       "MDA + Active transmission-blocking + Active anti-infective",
    #       "Active anti-infective + Active transmission-blocking")
    p = c("Anti-infective monoclonal ", "Anti-infective monoclonal", "Anti-infective vaccine ", "Anti-infective vaccine",
          "Transmission-blocking vaccine ", "Transmission-blocking vaccine", "Blood stage clearance ", "Blood stage clearance",
          "Attractive targeted sugar baits ", "Attractive targeted sugar baits",
          "Eave tubes ", "Eave tubes", "Endectocide",
          "MDA + anti-infective monoclonal", "MDA + anti-infective vaccine",
          "MDA + transmission-blocking vaccine",
          "MDA + transmission-blocking vaccine + anti-infective vaccine",
          "Anti-infective vaccine + transmission-blocking vaccine")
    categories = factor(p, levels = p)
    myColors = c("#E78AC3", "#E78AC3", "#984EA3", "#984EA3",
                 "#7570B3", "#7570B3", "#CB181D", "#CB181D",
                 "#1B9E77", "#1B9E77", "#66C2A5", "#66C2A5",
                 "#A6D854", "#E7298A",
                 "#7A0177", "#4A1486",
                 "#91003F", "#2D004B")
    names(myColors) = levels(categories)
    return(myColors)
}

# Color palette for the decay functions
create_color_palette_decay = function() {
    p = c("Step", "Sigmoidal", "Exponential", "Biphasic")
    categories = factor(p, levels = p)
    myColors = c("#8C2D04", "#F16913", "#FDAE6B", "#FEE6CE")
    names(myColors) = levels(categories)
    return(myColors)
}

# Define experiment names
define_experiment_names = function()
{
    # "MDA + Passive anti-infective", "MDA + Active anti-infective",
    # "MDA + Active transmission-blocking",
    # "Active anti-infective + Active transmission-blocking",
    # "MDA + Active transmission-blocking + Active anti-infective"
    # "Passive anti-infective",
    # "MDA + Passive anti-infective",
    # "Pre- and postprandial killing"
    
    # For interventions deployed single
    list_folders_single = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",     
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/",               
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/",               
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/",              
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")   
    list_names_single = c("Anti-infective monoclonal ", "Anti-infective vaccine ",
                          "Transmission-blocking vaccine ",
                          "Attractive targeted sugar baits ", "Eave tubes ") #, "Endectocide")
    
    # For interventions deployed in combination
    list_folders_combi = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_once/",
                           "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/",
                           "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/")
    
    list_names_combi = c("MDA + anti-infective monoclonal", "MDA + anti-infective vaccine",
                         "MDA + transmission-blocking vaccine")
    
    # For interventions deployed twice
    list_folders_twice = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_twice_sync_3years/",
                           "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_twice/",
                           "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/")
    
    list_names_twice = c("Anti-infective monoclonal", 
                         "MDA + anti-infective monoclonal", 
                         "Attractive targeted sugar baits")
    
    list_folders_drugs = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",     
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/",               
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/")
    
    list_folders_vc = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/",              
                        "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")
    
    all_folders = c(list_folders_single, list_folders_combi, list_folders_twice)
    all_names = c(list_names_single, list_names_combi, list_names_twice)
    
    return(list(list_folders_single = list_folders_single,
                list_names_single = list_names_single,
                list_folders_combi = list_folders_combi,
                list_names_combi = list_names_combi,
                list_folders_twice = list_folders_twice,
                list_names_twice = list_names_twice,
                all_folders = all_folders,
                all_names = all_names,
                list_folders_drugs = list_folders_drugs,
                list_folders_vc = list_folders_vc))
}

define_deployment_names = function() {
    dep_exp_folders = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_twice_sync_3years/",
                            # "/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_once/",
                            # "/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_twice/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/",  
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/",
                            "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")
    dep_exp_names = c("Once per year", "Twice per year", "Once per year \n+ blood stage drug",
                      "Twice per year \n+ blood stage drug", "Once per year ", "Twice per year ", 
                      "Once per year  ", "Once per year \n+ blood stage drug ",
                      "Once per year   ", "Once per year \n+ blood stage drug  ", "Once per year\n")
    return(list(dep_exp_names = factor(dep_exp_names, levels = dep_exp_names), 
                dep_exp_folders = factor(dep_exp_folders, levels = dep_exp_folders)))
}

define_deployment_names_vacc = function() {
    dep_exp_folders = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/",
                        "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
                        "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/",
                        "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/")
    dep_exp_names = c("Once per year", "Once per year \n+ blood stage drug",
                      "Once per year", "Once per year \n+ blood stage drug")
    return(list(dep_exp_names = factor(dep_exp_names, levels = dep_exp_names), 
                dep_exp_folders = factor(dep_exp_folders, levels = dep_exp_folders)))
}

# Color palette for the decay functions
create_color_palette_deployment = function() {
    deployment_cols = define_deployment_names()
    p = deployment_cols$dep_exp_names
    myColors = c("#E78AC3", "#c994c7", "#E7298A", "#980043", "#1B9E77", "#276419", 
                 "#E78AC3", "#E7298A", "#E78AC3", "#E7298A", "#66C2A5")
    names(myColors) = levels(p)
    return(myColors)
}

