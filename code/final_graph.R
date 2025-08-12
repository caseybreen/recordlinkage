#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-08-10    #
#########################

# Load libraries
library(here)
library(data.table)
library(readxl)
library(ggplot2)
library(cowplot)
library(scales)
library(grid)

# Paths
wos_path      <- here("wos/")
sim_path      <- here("simulation/result/")
passing_path  <- here("fastlink/result/")
graph_path    <- here("graph/")
dir.create(graph_path, showWarnings=FALSE)

result_path   <- "/blue/wjoo/project/linkage/result/"
dir.create(result_path, showWarnings=FALSE)

# set seed
set.seed(1234)

########## WOS trends

wos1 <- read_excel(paste0(raw_path, "full_21.xls"))
wos2 <- read_excel(paste0(raw_path, "full_22.xls"))
wos3 <- read_excel(paste0(raw_path, "full_23.xls"))

wos <- as.data.table(rbind(wos1, wos2, wos3))
rm(wos1, wos2, wos3)

wos <- wos[!is.na(DOI)]

setnames(wos, c("WoS Categories", "Publication Year"), c("field", "year"))
wos <- wos[, c("year", "field")]
wos[, field:=tolower(field)]

fld <- c("sociology", "economics", "demography", "psychology", "political science", "geography", "education", "business")
for (i in fld){
  wos[, paste0(i):=as.numeric(grepl(paste0(i), field))]
}
wos[, field:=NULL]
wos[, dse:=0]
wos[sociology==1|economics==1|demography==1, dse:=1]
wos[, total:=1]

des <- wos[, lapply(.SD, sum), by=c("year")]
des <- des[order(year)]

fwrite(des, paste0(result_path, "wos.csv"))

gr1 <- ggplot(des[year>=1990 & year<=2024], aes(x=year)) +
  geom_col(aes(y=total, fill="Social Science")) +
  geom_col(aes(y=dse, fill="Dem/Soc/Econ")) +
  xlab("Year") + ylab("Article Count") +
  scale_fill_manual(name="",
                    values=c("Social Science"="#E69F00",
                             "Dem/Soc/Econ"="#0072B2"),
                    breaks=c("Social Science", "Dem/Soc/Econ")) +
  theme_cowplot() +
  theme(legend.position="bottom", legend.key.size=unit(0.5, "line"), legend.key.spacing.x=unit(15, "pt"))
ggsave(paste0(result_path, "1_wos.svg"), plot=gr1, width=7, height=5)
ggsave(paste0(result_path, "1_wos.png"), plot=gr1, width=7, height=5, bg="transparent")

gr2 <- ggplot(des[year>=1990 & year<=2024], aes(x=year)) +
  geom_col(aes(y=dse, fill="Dem/Soc/Econ")) +
  xlab("Year") + ylab("Article Count") +
  scale_fill_manual(name="",
                    values=c("Dem/Soc/Econ"="#0072B2")) +
  theme_cowplot() +
  theme(legend.position="bottom",
        legend.text = element_text(color = "transparent"),
        legend.key = element_rect(fill = "transparent")) +
  guides(fill=guide_legend(override.aes=list(alpha = 0, color ="transparent")))
gr <- plot_grid(gr1, gr2, ncol=2, nrow=1, labels = c("a", "b"))
ggsave(paste0(result_path, "1_combined.svg"), plot=gr, width=10, height=5)
ggsave(paste0(result_path, "1_combined.png"), plot=gr, width=10, height=5, bg="transparent")
rm(gr1, gr2, gr)

########## linkage

##### missed
### independent
res <- fread(paste0(result_path, "rnorm_250701/mr05_1/2_result.csv"),
             select=c("rec", "pre", "betat2",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w",
                      "beta2", "lb2", "ub2", "beta2_w", "lb2_w", "ub2_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- res$betat2[1]

gr1 <- ggplot(res[pre==1], aes(x=miss)) +
  geom_line(aes(y=beta2), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb2, ymax=ub2), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("Missed Match Rate") + ylab("Regression Coefficient") +
  scale_y_continuous(limits = c(1.8, 2.2)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
ggsave(paste0(result_path, "2_missed_independent_base.svg"), plot=gr1, width=5, height=5)
ggsave(paste0(result_path, "2_missed_independent_base.png"), plot=gr1, width=5, height=5, bg="transparent")
rm(gr1)

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
ggsave(paste0(result_path, "2_missed_independent.png"), plot=gr1, width=5, height=5, bg="transparent")
rm(res, yint)

### confounder
res <- fread(paste0(result_path, "rnorm_250701/mr05_1/6_result.csv"),
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
ggsave(paste0(result_path, "2_missed_confounder.png"), plot=gr2, width=5, height=5, bg="transparent")
rm(res, yint)

### moderator
res <- fread(paste0(result_path, "rnorm_250701/mr05_1/8_result.csv"),
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
ggsave(paste0(result_path, "2_missed_moderator.png"), plot=gr3, width=5, height=5, bg="transparent")
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
ggsave(paste0(result_path, "2_missed.svg"), plot=gr, width=12, height=5)
ggsave(paste0(result_path, "2_missed.png"), plot=gr, width=12, height=5, bg="transparent")

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
ggsave(paste0(result_path, "2_missed_1.svg"), plot=gr1, width=4, height=5)
ggsave(paste0(result_path, "2_missed_1.png"), plot=gr1, width=4, height=5, bg="transparent")
ggsave(paste0(result_path, "2_missed_2.svg"), plot=gr2, width=4, height=5)
ggsave(paste0(result_path, "2_missed_2.png"), plot=gr2, width=4, height=5, bg="transparent")
ggsave(paste0(result_path, "2_missed_3.svg"), plot=gr3, width=4, height=5)
ggsave(paste0(result_path, "2_missed_3.png"), plot=gr3, width=4, height=5, bg="transparent")
rm(gr, gr1, gr2, gr3)

##### false match

res <- fread(paste0(result_path, "rnorm_250701/mr05_1/1_result.csv"),
             select=c("rec", "pre", "betat2",
                      "beta1", "lb1", "ub1", "beta1_w", "lb1_w", "ub1_w",
                      "beta2", "lb2", "ub2", "beta2_w", "lb2_w", "ub2_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- res$betat2[1]
gr1 <- ggplot(res[miss==0.5], aes(x=false)) +
  geom_line(aes(y=beta2), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb2, ymax=ub2), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("False Match Rate") + ylab("Regression Coefficient") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(breaks = c(1, 1.5, 2)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
ggsave(paste0(result_path, "3_false_reg.png"), plot=gr1, width=5, height=5, bg="transparent")
rm(res, yint)

res <- fread(paste0(result_path, "rnorm_250730_passing/rt10/mr01_1/1_result.csv"),
             select=c("rec", "pre",
                      "tr", "lb", "ub", "tr_w", "lb_w", "ub_w"))
res[, false:=1-pre]
res[, miss:=1-rec]
yint <- 0.1
gr2 <- ggplot(res[miss==0.5], aes(x=false)) +
  geom_line(aes(y=tr), color="#0072B2", linewidth=1) +
  geom_ribbon(aes(ymin=lb, ymax=ub), fill="#0072B2", alpha=0.2) +
  geom_hline(yintercept=yint, linetype="dashed", color="black") +
  xlab("False Match Rate") + ylab("Transition Rate") +
  scale_x_continuous(limits = c(0, 0.5)) +
  guides(color=guide_legend(),
         fill="none") +
  theme_cowplot() +
  theme(legend.position="none")
ggsave(paste0(result_path, "3_false_tr.png"), plot=gr2, width=5, height=5, bg="transparent")
rm(res, yint)

gr <- plot_grid(gr1 + xlab(" "), NULL, gr2 + xlab(" "),
                labels = c("a.", "", "b."),
                nrow=1, rel_widths = c(1, 0.1, 1))
gr <- ggdraw(gr) + draw_label("     False Match Rate", x = 0.5, y = 0.02, vjust = 0)
ggsave(paste0(result_path, "3_false.svg"), plot=gr, width=8, height=5)
ggsave(paste0(result_path, "3_false.png"), plot=gr, width=8, height=5, bg="transparent")
rm(gr, gr1, gr2)

#### passing

passing <- fread(paste0(result_path, "passing/passing_continuous.csv"))
passing_boot <- fread(paste0(result_path, "passing/passing_continuous_boot.csv"))
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
ggsave(paste0(result_path, "passing_continous.svg"), plot=gr, width=7, height=5)
ggsave(paste0(result_path, "passing_continous.png"), plot=gr, width=7, height=5, bg="transparent")

rm(gr)

# clean
rm(list=ls())
