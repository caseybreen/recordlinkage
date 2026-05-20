#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# Load libraries
library(here)
library(data.table)
library(ggplot2)
library(cowplot)
library(grid)

# Paths
result_path <- here("result/simulation/")
graph_path  <- here("result/simulation/graph/")
dir.create(graph_path, showWarnings=FALSE)

# set seed
set.seed(1234)

########## linkage

##### missed
### independent
res <- fread(paste0(result_path, "reg/mr05_1/2_result.csv"),
             select=c("rec", "pre", "betat1", "betat2",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w",
                      "beta2", "lb2", "ub2", "beta2_w", "lb2_w", "ub2_w"))
res[, false:=1-pre]
res[, miss:=1-rec]

yint <- res$betat1[1]
gr0 <- ggplot(res[pre==1], aes(x=miss)) +
  geom_line(aes(y=beta1), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb1, ymax=ub1), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("Missed Match Rate") + ylab("Regression Coefficient") +
  scale_y_continuous(limits = c(1.8, 2.2)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
ggsave(paste0(graph_path, "Figure_A1.png"), plot=gr0, width=5, height=5, bg="transparent")
rm(gr0)

yint <- res$betat2[1]
gr1 <- ggplot(res[pre==1], aes(x=miss)) +
  geom_line(aes(y=beta2, color="Baseline"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2, ymax=ub2, fill="Baseline"), alpha=0.2) +
  geom_line(aes(y=beta2_w, color="Weighted"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2_w, ymax=ub2_w, fill="Weighted"), alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("Missed Match Rate") + ylab("Regression Coefficient") +
  scale_color_manual(name="",
                     values=c("Baseline"="#0072B2",
                              "Weighted"="#E69F00"),
                     breaks=c("Baseline", "Weighted")) +
  scale_fill_manual(name="",
                    values=c("Baseline"="#0072B2",
                             "Weighted"="#E69F00"),
                    breaks=c("Baseline", "Weighted")) +
  scale_y_continuous(limits = c(1.5, 2.9)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="bottom", legend.justification="center")
rm(res, yint)

### confounder
res <- fread(paste0(result_path, "reg/mr05_1/4_result.csv"),
             select=c("rec", "pre", "betat2",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w",
                      "beta2", "lb2", "ub2", "beta2_w", "lb2_w", "ub2_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- res$betat2[1]
gr2 <- ggplot(res[pre==1], aes(x=miss)) +
  geom_line(aes(y=beta2, color="Baseline"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2, ymax=ub2, fill="Baseline"), alpha=0.2) +
  geom_line(aes(y=beta2_w, color="Weighted"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2_w, ymax=ub2_w, fill="Weighted"), alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("Missed Match Rate") + ylab("Regression Coefficient") +
  scale_color_manual(name="",
                     values=c("Baseline"="#0072B2",
                              "Weighted"="#E69F00"),
                     breaks=c("Baseline", "Weighted")) +
  scale_fill_manual(name="",
                    values=c("Baseline"="#0072B2",
                             "Weighted"="#E69F00"),
                    breaks=c("Baseline", "Weighted")) +
  scale_y_continuous(limits = c(1.5, 2.9)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="bottom", legend.justification="center")
rm(res, yint)

### moderator
res <- fread(paste0(result_path, "reg/mr05_1/6_result.csv"),
             select=c("rec", "pre", "betat2",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w",
                      "beta2", "lb2", "ub2", "beta2_w", "lb2_w", "ub2_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- res$betat2[1]
gr3 <- ggplot(res[pre==1], aes(x=miss)) +
  geom_line(aes(y=beta2, color="Baseline"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2, ymax=ub2, fill="Baseline"), alpha=0.2) +
  geom_line(aes(y=beta2_w, color="Weighted"), linewidth=1) +
  geom_ribbon(aes(ymin=lb2_w, ymax=ub2_w, fill="Weighted"), alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("Missed Match Rate") + ylab("Regression Coefficient") +
  scale_color_manual(name="",
                     values=c("Baseline"="#0072B2",
                              "Weighted"="#E69F00"),
                     breaks=c("Baseline", "Weighted")) +
  scale_fill_manual(name="",
                    values=c("Baseline"="#0072B2",
                             "Weighted"="#E69F00"),
                    breaks=c("Baseline", "Weighted")) +
  scale_y_continuous(limits = c(1.5, 2.9)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="bottom", legend.justification="center")
rm(res, yint)

### combined
gr <- plot_grid(gr1 + xlab(" ") +
                  theme(legend.text = element_text(color = "transparent"),
                        legend.key = element_rect(fill = "transparent")) +
                  guides(color=guide_legend(override.aes=list(alpha = 0, color ="transparent"))),
                gr2 + ylab(" ") +
                  theme(axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        legend.key.spacing.x=unit(20, "pt")),
                gr3 + xlab(" ") + ylab(" ") +
                  theme(axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        legend.text = element_text(color = "transparent"),
                        legend.key = element_rect(fill = "transparent")) +
                  guides(color=guide_legend(override.aes=list(alpha = 0, color ="transparent"))),
                labels = c("       a.", "     b.", "     c."),
                nrow=1, rel_widths = c(1.05, 1, 1))
ggsave(paste0(graph_path, "Figure_3.png"), plot=gr, width=12, height=5, bg="transparent")

gr1 <- gr1 + xlab(" ") +
  theme(legend.text = element_text(color = "transparent"),
        legend.key = element_rect(fill = "transparent")) +
  guides(color=guide_legend(override.aes=list(alpha = 0, color ="transparent")))
gr2 <- gr2 + ylab(" ") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.key.spacing.x=unit(20, "pt"))
gr3 <- gr3 + xlab(" ") + ylab(" ") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text = element_text(color = "transparent"),
        legend.key = element_rect(fill = "transparent")) +
  guides(color=guide_legend(override.aes=list(alpha = 0, color ="transparent")))
ggsave(paste0(graph_path, "Figure_3a.png"), plot=gr1, width=4, height=5, bg="transparent")
ggsave(paste0(graph_path, "Figure_3b.png"), plot=gr2, width=4, height=5, bg="transparent")
ggsave(paste0(graph_path, "Figure_3c.png"), plot=gr3, width=4, height=5, bg="transparent")
rm(gr, gr1, gr2, gr3)

##### false match

res <- fread(paste0(result_path, "reg/mr05_1/2_result.csv"),
             select=c("rec", "pre", "betat1",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- res$betat1[1]
gr1 <- ggplot(res[miss==0.5], aes(x=false)) +
  geom_line(aes(y=beta1), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb1, ymax=ub1), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab(" ") + ylab("Regression Coefficient") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(breaks = c(1, 1.5, 2)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
rm(res, yint)

res <- fread(paste0(result_path, "rate/rt10/mr05_1/2_result.csv"),
             select=c("rec", "pre",
                      "tr", "lb", "ub", "tr_w", "lb_w", "ub_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- 0.1
gr2 <- ggplot(res[miss==0.5], aes(x=false)) +
  geom_line(aes(y=tr), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb, ymax=ub), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab(" ") + ylab("Transition Rate") +
  scale_x_continuous(limits = c(0, 0.5)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
rm(res, yint)

gr <- plot_grid(gr1, NULL, gr2,
                labels = c("a.", "", "b."),
                nrow=1, rel_widths = c(1, 0.1, 1))
gr <- ggdraw(gr) + draw_label("     False Match Rate", x = 0.5, y = 0.02, vjust = 0)
ggsave(paste0(graph_path, "Figure_4.png"), plot=gr, width=8, height=5, bg="transparent")
rm(gr, gr1, gr2)

# clean
rm(list=ls())

