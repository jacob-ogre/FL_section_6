# Preliminary analysis of FL end. sp. data.
# Copyright © 2015 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

library(ggplot2)
library(FactoMineR)
source("multiplot.R")

# fil <- "data/FL_species_evaluation_for_analysis.csv"
fil <- "data/FL_species_evaluation_with_spend_for_analysis.tab"
dat <- read.table(fil, 
                  sep="\t", 
                  header=TRUE, 
                  na.strings="n/a")
bak <- dat
names(dat)

for (i in names(dat)[2:(length(dat)-1)]) {
    print(i)
    print(levels(dat[[i]]))
    print("")
}

dat$Status <- ifelse(dat$Status == "T", 0, 1)
dat$Crit_Hab <- ifelse(dat$Crit_Hab == "N", 0, 1)
dat$Rec_criteria <- ifelse(dat$Rec_criteria == "No", 0, 1)

dat$Obj_quant_crit <- ifelse(dat$Obj_quant_crit == "No", 0, 
                             ifelse(dat$Obj_quant_crit == "Mix", 0.5, 1))

dat$Revise_rec_plan <- ifelse(dat$Revise_rec_plan == "No", 0, 1)
dat$Impl_progress <- ifelse(dat$Impl_progress == "Most not started",
                            0,
                     ifelse(dat$Impl_progress == "Half not started",
                            1,
                     ifelse(dat$Impl_progress == "Most ongoing current or better",
                            2, 3)))

dat$Rec_plan_date <- as.Date(as.character(dat$Rec_plan_date), "%m/%d/%y")
dat$Review_date <- as.Date(as.character(dat$Review_date), "%m/%d/%y")
bak$Rec_plan_date <- as.Date(as.character(dat$Rec_plan_date), "%m/%d/%y")
bak$Review_date <- as.Date(as.character(dat$Review_date), "%m/%d/%y")

dat$Rec_plan_year <- as.numeric(format(dat$Rec_plan_date, "%Y"))
dat$Review_year <- as.numeric(format(dat$Review_date, "%Y"))
bak$Rec_plan_year <- as.numeric(format(dat$Rec_plan_date, "%Y"))
bak$Review_year <- as.numeric(format(dat$Review_date, "%Y"))

dat2 <- dat[ ,c(2:3, 5:8, 10:17, 19:20)]
row.names(dat2) <- dat$Species

# Fun with PCAs
pca1 <- PCA(dat2, ncp=10)
for_plotly <- data.frame(pca1$ind$coord[,1:2])

write.table(for_plotly,
            file="data/spp_coords_with_expend.tab",
            sep="\t",
            quote=FALSE)

weight1 <- c(1,2,2,1.5,1,1,2,2,1,1,1,1.5,1,1,1)
pca2 <- PCA(dat2, ncp=10, col.w=weight1)

hcpc1 <- HCPC(dat2)

pairwise <- cor(dat2, use="pairwise.complete.obs")
cor.test(dat2$N_lead_agencies, dat2$N_SHA)
cor.test(dat2$N_formal, dat2$Review_year)
write.table(pairwise, "pairwise_correls.tab", sep="\t", quote=FALSE)

plot(pca1$eig$`cumulative percentage of variance`, 
     ylim=c(0,100),
     xlab="Principal Component",
     ylab="Cumulative Pct. Variance")

stripchart(jitter(bak$Rec_plan_year, 0.8) ~ bak$Obj_quant_crit,
           pch=19,
           xlab="Objective and Quant. Criteria?",
           ylab="Recovery Plan Year")

boxplot(bak$Rec_plan_year ~ as.factor(bak$Impl_progress),
        xlab="Implementation progress",
        ylab="Year of recovery plan")

boxplot(bak$Demog_chg ~ as.factor(bak$Obj_quant_crit),
        xlab="Objective & Quant. Criteria?",
        ylab="Demographic change")

par(mfrow=c(1,2))
plot(dat$N_consult, jitter(dat$Demog_chg, 0.5),
     pch=19,
     xlab="# consultations",
     ylab="Demographic change")
plot(dat$N_consult, jitter(dat$Threat_chg, 0.5),
     pch=19,
     xlab="# consultations",
     ylab="Threat change")

par(mfrow=c(1,2))
plot(dat$fed_expend, jitter(dat$Demog_chg, 0.5),
     pch=19,
     xlab="Fed. expenditures ($)",
     ylab="Demographic change")
plot(dat$fed_expend, jitter(dat$Threat_chg, 0.5),
     pch=19,
     xlab="Fed. expenditures ($)",
     ylab="Threat change")

par(mfrow=c(1,2))
boxplot(dat$fed_expend ~ as.factor(dat$Demog_chg),
     pch=19,
     ylab="Fed. expenditures ($)",
     xlab="Demographic change")
boxplot(dat$fed_expend ~ as.factor(dat$Threat_chg),
     pch=19,
     ylab="Fed. expenditures ($)",
     xlab="Threat change")

par(mfrow=c(1,2))
boxplot(dat$Demog_chg ~ as.factor(dat$Impl_progress),
        las=2,
        xlab="Implementation progress",
        ylab="Demographic change")
boxplot(dat$Threat_chg ~ as.factor(dat$Impl_progress),
        las=2,
        xlab="Implementation progress",
        ylab="Threat change")

demog <- ggplot(dat, aes(factor(Impl_progress), Demog_chg)) +
         geom_boxplot() +
         geom_jitter(position=position_jitter(width=0.5))
demog

coords <- data.frame(pca1$ind$coord[,1:2])

labs <- strsplit(rownames(coords), split=" (", fixed=TRUE)
labs <- as.vector(sapply(labs, "[[", 1))

PCA_species <- ggplot(coords, aes(Dim.1, Dim.2, label=labs)) +
               geom_point(alpha=0.5, size=3) +
               geom_text(size=3, vjust=2)
PCA_species
