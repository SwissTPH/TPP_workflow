######################################
# Plot the distributions for prevalence reduction for all interventions
# and create the corresponding tables
#
# monica.golumbeanu@unibas.ch
######################################

source("~/MMC/TPP/scripts_v38/plotting/plot_prev_red_distr.R")
library(ggpubr)

# Retrieve all experiment names
exp_names = define_experiment_names()
# Retrieve the EIR to prevalence transformations
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)

# ## For single interventions
# p1 = plot_prev_red_distr(sim_folders = exp_names$list_folders_single, sim_names = exp_names$list_names_single, 
#                          follow_up = 4, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
# p2 = plot_prev_red_distr(sim_folders = exp_names$list_folders_single, sim_names = exp_names$list_names_single, 
#                          follow_up = 6, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
# figure = ggarrange(p1$p, p2$p, common.legend = TRUE, legend="top",
#                    labels = c("A", "B"), ncol = 1, nrow = 2)
# ggsave("~/MMC/TPP/figures/simulation_desc/prev_red/prev_red_distr_single.pdf",
#        plot = figure, width = 8, height = 7)
# write.table(p1$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/single_4.txt", quote = FALSE,
#             sep="\t", col.names = TRUE, row.names = FALSE)
# write.table(p2$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/single_6.txt", quote = FALSE,
#             sep="\t", col.names = TRUE, row.names = FALSE)

# For interventions deployed in combination
p1 = plot_prev_red_distr(sim_folders = exp_names$list_folders_combi, sim_names = exp_names$list_names_combi,
                         follow_up = 4, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
p2 = plot_prev_red_distr(sim_folders = exp_names$list_folders_combi, sim_names = exp_names$list_names_combi,
                         follow_up = 6, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
figure = ggarrange(p1$p, p2$p, common.legend = TRUE, legend="top",
                   labels = c("A", "B"), ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/simulation_desc/prev_red/prev_red_distr_combi_new.pdf",
       plot = figure, width = 8, height = 7)
write.table(p1$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/combi_4_new.txt", quote = FALSE,
            sep="\t", col.names = TRUE, row.names = FALSE)
write.table(p2$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/combi_6_new.txt", quote = FALSE,
            sep="\t", col.names = TRUE, row.names = FALSE)

# For interventions deployed twice
# p1 = plot_prev_red_distr(sim_folders = exp_names$list_folders_twice, sim_names = exp_names$list_names_twice, 
#                          follow_up = 4, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
# p2 = plot_prev_red_distr(sim_folders = exp_names$list_folders_twice, sim_names = exp_names$list_names_twice, 
#                          follow_up = 6, legend_rows = 3, EIR_prev_tab = EIR_prev_tab)
# figure = ggarrange(p1$p, p2$p, common.legend = TRUE, legend="top",
#                    labels = c("A", "B"), ncol = 1, nrow = 2)
# ggsave("~/MMC/TPP/figures/simulation_desc/prev_red/prev_red_twice.pdf",
#        plot = figure, width = 8, height = 7)
# write.table(p1$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/twice_4_new.txt", quote = FALSE,
#             sep="\t", col.names = TRUE, row.names = FALSE)
# write.table(p2$stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/twice_6_new.txt", quote = FALSE,
#             sep="\t", col.names = TRUE, row.names = FALSE)


