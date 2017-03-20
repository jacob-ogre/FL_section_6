# Quick analysis of Florida NLCD 2001-2011 land cover changes.
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

#############################################################################
# Load the data and do basic management
#############################################################################
fil <- "data/NLCD_change_upd.tab"
dat <- read.table(fil, sep="\t", stringsAsFactors=FALSE, header=TRUE)
head(dat)

dat$amt_x_idx <- dat$amt * dat$index

#############################################################################
# Analyses
#############################################################################
# by-county analysis
sum_idx_cty <- tapply(dat$amt_x_idx,
                      INDEX=dat$County,
                      FUN=sum, na.rm=TRUE)
sort(sum_idx_cty)
sum_idx_df <- data.frame(names(sum_idx_cty), as.vector(sum_idx_cty))
names(sum_idx_df) <- c("County", "idx_sum")
head(sum_idx_df)
dat[dat$County=="PALM_BEACH" & dat$amt_x_idx > 0,]
write.table(sum_idx_df, "data/sum_idx_NLCD_by_county.tab",
            sep="\t", quote=FALSE)

#############################################################################
# Bring in the NLCD-relevant time human population data, then analyze
#############################################################################
popf <- "data/FL_pop_2001-2011.tab"
pop <- read.table(popf, sep="\t", stringsAsFactors=FALSE, header=TRUE)
pop$X2001 <- as.numeric(gsub(",", "", pop$X2001, fixed=TRUE))
pop$X2011 <- as.numeric(gsub(",", "", pop$X2011, fixed=TRUE))

sum_idx_df <- data.frame(sum_idx_df, pop[,2:4])
head(sum_idx_df)
sum_idx_df$pct_pop_chg <- sum_idx_df$Change / sum_idx_df$X2001
head(sum_idx_df)
names(sum_idx_df) <- c("County", "Hab_chg", "Pop_2001", "Pop_2011", "Pop_chg",
                       "Pct_pop_chg")
sum_idx_df$Hab_chg_ac <- sum_idx_df$Hab_chg * 900 * 0.000247105
write.table(sum_idx_df, "data/sum_idx_NLCD_by_county_2.tab",
            sep="\t", quote=FALSE)

#############################################################################
# And some basic plots...
#############################################################################
plot(idx_sum ~ pct_pop_chg, data=sum_idx_df,
     pch=19,
     xlab="% Pop. Change, 2001-2011",
     ylab="Natural Habitat Change, 2001-2011")
abline(h=0)
abline(v=0)
abline(lm(idx_sum ~ pct_pop_chg, data=sum_idx_df), col="red")

plot(idx_sum ~ Change, data=sum_idx_df,
     pch=19,
     xlab="Human Pop. Change, 2001-2011",
     ylab="Natural Habitat Change, 2001-2011")
abline(h=0)
abline(v=0)
abline(lm(idx_sum ~ Change, data=sum_idx_df), col="red")

