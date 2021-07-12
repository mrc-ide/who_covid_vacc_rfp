################################################################################
# Plotting specifics
# Age bands
x1 <- seq(0,80,5)
y1 <- seq(4,84,5)
y2 <- seq(5, 85, 5)
z1 <- paste0(x1,"-",y1)
z2 <- paste0(x1, "-", y2)
z1[17] <- "80+"
z2[17] <- "80+"
a <- paste0(seq(0, 80, 5), "+")
a[3] <- "12+"
age_group_key <- data.frame(target_group_stop = 1:17, `Age target` = a)

age_group_10y <- c("0-10", "0-10", "10-20", "10-20","20-30", "20-30", "30-40", "30-40","40-50","40-50", "50-60","50-60", "60-70", "60-70", "70-80", "70-80", "80+")

age_group_key_2 <- data.frame(age_group = z2, age_group_10y = age_group_10y)

col1 <- "#6dc1db"
col2 <- "#d8b9a9"
col2b <- "khaki4"
col3 <- "#e5546c"
col4 <- "#c00000"