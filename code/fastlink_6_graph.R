#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-08-10    #
#########################

# Load libraries
library(here)
library(data.table)
library(ggplot2)
library(cowplot)
library(scales)

# Paths
result_path   <- here("result/fastlink/")
graph_path    <- here("data/fastlink/graph/")
dir.create(graph_path, showWarnings=FALSE)

# set seed
set.seed(1234)

########## passing, continuous threshold

passing <- fread(paste0(result_path, "passing_continuous.csv"))
passing_boot <- fread(paste0(result_path, "passing_continuous_boot.csv"))
passing <- merge(passing, passing_boot, by=c("q"))
passing_combined <- passing[, c("q", "r", "pass", "pass_l", "pass_u", "apass1", "apass1_l", "apass1_u", "apass3", "apass3_l", "apass3_u")]
colnames(passing_combined) <- c("q", "r", "pass", "pass_l", "pass_u", "apass1", "apass1_l", "apass1_u", "apass2", "apass2_l", "apass2_u")

gr <- ggplot(passing, aes(x=q)) +
  geom_line(aes(y=pass, color="Raw")) +
  geom_ribbon(aes(ymin=pass_l, ymax=pass_u, fill="Raw"), alpha=0.2) +
  geom_line(aes(y=apass1, color="Adjusted 1"), linetype="longdash") +
  geom_ribbon(aes(ymin=apass1_l, ymax=apass1_u, fill="Adjusted 1"), alpha=0.2) +
  geom_line(aes(y=apass3, color="Adjusted 2"), linetype="longdash") +
  geom_ribbon(aes(ymin=apass3_l, ymax=apass3_u, fill="Adjusted 2"), alpha=0.2) +
  labs(x = "Threshold", y = "Passing Rate") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  scale_color_manual(name="",
                     values=c("Raw"="#0072B2", "Adjusted 1"="#E69F00", "Adjusted 2"="#D55E00"),
                     breaks=c("Raw", "Adjusted 1", "Adjusted 2")) +
  scale_fill_manual(name="",
                    values=c("Raw"="#0072B2", "Adjusted 1"="#E69F00", "Adjusted 2"="#D55E00"),
                    breaks=c("Raw", "Adjusted 1", "Adjusted 2")) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme_minimal() +
  theme(legend.position="bottom", legend.justification="center")
ggsave(paste0(graph_path, "passing_continous.png"), plot=gr, width=7, height=5, bg="transparent")

rm(passing, passing_boot, gr)

########## passing, quartile

passing <- fread(paste0(result_path, "passing_quantile.csv"))
passing_boot <- fread(paste0(result_path, "passing_quantile_boot.csv"))
passing <- merge(passing, passing_boot, by=c("quant"))
passing_combined <- passing[, c("quant", "pass", "pass_l", "pass_u", "apass1", "apass1_l", "apass1_u", "apass3", "apass3_l", "apass3_u")]
colnames(passing_combined) <- c("quant", "pass", "pass_l", "pass_u", "apass1", "apass1_l", "apass1_u", "apass2", "apass2_l", "apass2_u")

gr <- ggplot(passing, aes(x=quant)) +
  geom_line(aes(y=pass, color="Raw")) +
  geom_ribbon(aes(ymin=pass_l, ymax=pass_u, fill="Raw"), alpha=0.2) +
  geom_line(aes(y=apass1, color="Adjusted 1"), linetype="longdash") +
  geom_ribbon(aes(ymin=apass1_l, ymax=apass1_u, fill="Adjusted 1"), alpha=0.2) +
  geom_line(aes(y=apass3, color="Adjusted 2"), linetype="longdash") +
  geom_ribbon(aes(ymin=apass3_l, ymax=apass3_u, fill="Adjusted 2"), alpha=0.2) +
  labs(x = "Quartile", y = "Passing Rate") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(name="",
                     values=c("Raw"="#0072B2", "Adjusted 1"="#E69F00", "Adjusted 2"="#D55E00"),
                     breaks=c("Raw", "Adjusted 1", "Adjusted 2")) +
  scale_fill_manual(name="",
                    values=c("Raw"="#0072B2", "Adjusted 1"="#E69F00", "Adjusted 2"="#D55E00"),
                    breaks=c("Raw", "Adjusted 1", "Adjusted 2")) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme_minimal() +
  theme(legend.position="bottom", legend.justification="center")
ggsave(paste0(graph_path, "passing_quantile.png"), plot=gr, width=7, height=5, bg="transparent")

rm(passing, passing_boot, gr)

# clean
rm(list=ls())

