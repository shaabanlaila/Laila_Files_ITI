library(ggplot2)
library(tidyverse)
library(RMySQL)

# connecting to mysql
mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname=Sys.getenv('MYSQL_DB'),
                            host=Sys.getenv('MYSQL_HOST'),
                            port=3306,
                            user=Sys.getenv('MYSQL_USER'),
                            password=Sys.getenv('MYSQL_PASSWORD'))

# importing dataframe and summarizing
df <- dbGetQuery(mysqlconnection, "SELECT * FROM memorizer")
dbListTables(mysqlconnection)
summary(df)

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
  geom_line(color = 'magenta') +
  labs(title = "Line Plot of Allocation x Freeing Time", x = "Alloc Jiffies", y= "Free Jiffies")

# hist of both
hist(df$alloc_jiffies, main = "Histogram of both Allocation and Freeing Time", xlab = "Jiffies", ylab = "Count", col = "lightgreen")
hist(df$free_jiffies, xlab = "", ylab = "", col = "lightblue", add = TRUE)
legend("topright", legend = c("Alloc Jiffies", "Free Jiffies"), fill = c("lightgreen","lightblue"))

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
