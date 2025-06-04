library(ggplot2)
library(tidyverse)
library(RMySQL)

# connecting to mysql
mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname=Sys.getenv('MYSQL_DB'),
                            host=Sys.getenv('MYSQLREMOTE_HOST'),
                            port=3306,
                            user=Sys.getenv('MYSQLREMOTE_USER'),
                            password=Sys.getenv('MYSQLREMOTE_PASS'))

# importing dataframe and summarizing
df <- dbGetQuery(mysqlconnection, "SELECT * FROM memorizer")
dbListTables(mysqlconnection)
df$free_jiffies[df$free_jiffies == -9223372036854775808] <- NA
df$free_ip[is.na(df$free_ip)] <- "NA"
summary(df)
head(df)

# hist of alloc jiffies
ggplot(data = df, mapping = aes(x = alloc_jiffies)) +
  geom_histogram(color = "white", fill = "lightgreen") +
  labs(title = "Distribution of Allocation Time", x = "Jiffies", y = "Count")

# hist of free jiffies
ggplot(data = df, mapping = aes(x = free_jiffies)) +
  geom_histogram(color = "white", fill = "lightblue") +
  labs(title = "Distribution of Freeing Time", x = "Jiffies", y = "Count")

# boxplot of alloc jiffies
ggplot(data = df, mapping = aes(y = alloc_jiffies)) +
  geom_boxplot(fill = 'lightgreen') +
  labs(title = "Distribution Boxplot of Allocation Time", y = "Jiffies")

# boxplot of free jiffies
ggplot(data = df, mapping = aes(y = free_jiffies)) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = "Distribution Boxplot of Freeing Time", y = "Jiffies")

# lineplot of alloc jiffies
ggplot(data = df, mapping = aes(x = 1:nrow(df), y = alloc_jiffies)) +
  geom_line(color = "darkgreen") +
  labs(title = "Lineplot of Allocation Time", y = "Alloc Jiffies")

# lineplot of free jiffies
ggplot(data = df, aes(x = 1:nrow(df), y = free_jiffies)) +
  geom_line(color = "lightblue") +
  labs(title = "Lineplot of Freeing Time", y = "Free Jiffies")

# lineplot of both
ggplot(data = df, mapping = aes(x = alloc_jiffies, y = free_jiffies)) +
  geom_line(color = 'lightblue') +
  labs(title = "Line Plot of Allocation x Freeing Time", x = "Alloc Jiffies", y= "Free Jiffies")

# hist of both
droppedna <- drop_na(df, free_jiffies)
hist(df$alloc_jiffies,
     main = "Histogram of Allocation and Freeing Time",
     xlab = "Jiffies",
     ylab = "Count",
     col = "lightgreen",
     breaks = 30,
     xlim = range(c(df$alloc_jiffies, droppedna$free_jiffies)),
     ylim = c(0, max(hist(df$alloc_jiffies, plot = FALSE)$counts, hist(droppedna$free_jiffies, plot = FALSE)$counts))
)
hist(droppedna$free_jiffies,
     col = "lightblue",
     breaks = 30,
     add = TRUE
)
legend("topright",
       legend = c("Alloc Jiffies", "Free Jiffies"),
       fill = c("lightgreen", "lightblue")
)

# point plot of both
ggplot(data = df, aes(x = alloc_jiffies, y = free_jiffies)) +
  geom_point(color = "lightgreen") +
  labs(x = "Alloc Jiffies",
       y = "Free Jiffies",
       title = "Comparing Trends Between Allocation and Freeing Time") +
  geom_smooth(method = "lm", color = "lightblue")

# trying out regression
regression <- lm(alloc_jiffies ~ free_jiffies, data = df)
summary(regression)

regression2 <- lm(alloc_jiffies ~ obj_size, data = df)
summary(regression2)
