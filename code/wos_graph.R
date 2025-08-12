#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-08-10    #
#########################

# list of articles are retrieved on 6/7/2025 at the WOS website (https://www.webofscience.com/wos/woscc/basic-search)
# WOS files are located in the data path

# Load libraries
library(here)
library(data.table)
library(ggplot2)
library(cowplot)
library(grid)

# Paths
data_path   <- here("wos/data/")
graph_path    <- here("wos/graph/")
dir.create(graph_path, showWarnings=FALSE)

# set seed
set.seed(1234)

########## WOS trends

wos1 <- read_excel(paste0(data_path, "full_21.xls"))
wos2 <- read_excel(paste0(data_path, "full_22.xls"))
wos3 <- read_excel(paste0(data_path, "full_23.xls"))

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
ggsave(paste0(graph_path, "Figure_1.png"), plot=gr, width=10, height=5, bg="transparent")
rm(gr1, gr2, gr)

# clean
rm(list=ls())
