##############################
# Generate tables with optimisation setups. A setup consists of the 
# fixed values of all parameters other than the parameter to be optimized for 
# achieving maximum prevalence reduction
# 
# monica.golumbeanu@unibas.ch
# 04.02.2020
##############################

sim_dir = "~/MMC/TPP/simulations/MDA_MAB_twice_3years/"

param_ranges_file = paste0(sim_dir, "param_ranges.RData")
table_file = paste0(sim_dir, "opt_setup.txt")
load(param_ranges_file)

CM_name = c("LowCM", "MidCM", "HighCM")
Param_opt = c("Coverage", "Halflife", "Efficacy")
Param_vec = t(param_ranges[,1])

opt_setup = Reduce(merge, list(as.data.frame(CM_name), as.data.frame(Param_vec), as.data.frame(Param_opt)))

if(!is.null(opt_setup$Drug_halflife)) {
    opt_setup$Drug_halflife = 0.167 #(half a year)
    opt_setup$Drug_efficacy = 0.9 #(half a year)
}

# Define case management levels
opt_setup[which(opt_setup$CM_name == "LowCM"), "Access"] = 0.1
opt_setup[which(opt_setup$CM_name == "MidCM"), "Access"] = 0.25
opt_setup[which(opt_setup$CM_name == "HighCM"), "Access"] = 0.5

# Optimization of halflife
opt_setup[which(opt_setup$Param_opt == "Halflife"), "Efficacy"] = 0.85
opt_setup[which(opt_setup$Param_opt == "Halflife"), "Coverage"] = 0.6

# Optimization of efficacy
opt_setup[which(opt_setup$Param_opt == "Efficacy"), "Halflife"] = 0.33 #(4 months)
opt_setup[which(opt_setup$Param_opt == "Efficacy"), "Coverage"] = 0.6

# Optimization of coverage
opt_setup[which(opt_setup$Param_opt == "Coverage"), "Halflife"] = 0.33 #(4 months)
opt_setup[which(opt_setup$Param_opt == "Coverage"), "Efficacy"] = 0.85

write.table(opt_setup, table_file, sep = "\t", quote = FALSE, col.names = TRUE,
            row.names = FALSE)


