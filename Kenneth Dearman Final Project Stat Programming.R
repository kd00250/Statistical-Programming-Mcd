library(readr)
library(dplyr)
library(ggridges)
library(ggplot2)
library(tidyverse)
library(gridExtra)
install.packages("ggplot2")
install.packages("tidyverse")
India_Menu <- read_csv("C:/Users/kenny/OneDrive/Desktop/Stat Programming In class Practice/India_Menu.csv")
View(India_Menu)

pdf(file="final_india_menu_project.pdf",width=10, height=13)

data <- India_Menu

#barplot of top 10 calories items
top10_cals <- data %>%
  arrange(desc(`Energy (kCal)`)) %>%
  slice(1:10)

topTenCals <- ggplot(top10_cals, aes(x = reorder(`Menu Items`, `Energy (kCal)`), y = `Energy (kCal)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Calorie Items",
       x = "Menu Item",
       y = "Calories (kCal)") +
  theme_minimal()

print(topTenCals)

#barplot of top 10 protien 
top10_protein <- data %>%
  arrange(desc(`Protein (g)`)) %>%
  slice(1:10)

topTenProtein <- ggplot(top10_protein, aes(x = reorder(`Menu Items`, `Protein (g)`), y = `Protein (g)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Protein Items",
       x = "Menu Item",
       y = "Protein (g)") +
  theme_minimal()

print(topTenProtein)

#barplot of top 10 Cholesterol
top10_cholesterol <- data %>%
  arrange(desc(`Cholesterols (mg)`)) %>%
  slice(1:10)

topTenCholesterol <- ggplot(top10_cholesterol, aes(x = reorder(`Menu Items`, `Cholesterols (mg)`), y = `Cholesterols (mg)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Cholesterol Items",
       x = "Menu Item",
       y = "Cholesterols (g)") +
  theme_minimal()

print(topTenCholesterol)

#barplot of top 10 sugars
top10_sugar <- data %>% 
  arrange(desc(`Total Sugars (g)`)) %>%
  slice(1:10)

topTenSugars <- ggplot(top10_sugar, aes(x = reorder(`Menu Items`, `Total Sugars (g)`), y = `Total Sugars (g)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Total Sugars Items",
       x = "Menu Item",
       y = "Total Sugars (g)") +
  theme_minimal()

print(topTenSugars)

#barplot of top 10 sugars (taking out drinks)
data_take_out_drinks <- data %>%
  filter(`Menu Category` != "Beverages Menu") %>% 
  arrange(desc(`Total Sugars (g)`)) %>%
  slice(1:10)

topTenSugarsWithoutDrinks <- ggplot(data_take_out_drinks, aes(x = reorder(`Menu Items`, `Total Sugars (g)`), y = `Total Sugars (g)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Highest Total Sugars Items (Without drinks)",
       x = "Menu Item",
       y = "Total Sugars (g)") +
  theme_minimal()

print(topTenSugarsWithoutDrinks)

#pie chart (over/under daily limit)
total_items <- 141
items_over_sugar_limit <- sum(data$`Total Sugars (g)` > 50.00)
percentage_over_sugar_limit <- (items_over_sugar_limit/total_items)
print(percentage_over_sugar_limit)
over_display_percent <- paste(as.character(round(percentage_over_sugar_limit*100,0)),"%",sep = "")

items_under_sugar_limit <- sum(data$`Total Sugars (g)` < 50.00)
percentage_under_sugar_limit <- items_under_sugar_limit / total_items
under_display_percent <- paste0(round(percentage_under_sugar_limit * 100, 0), "%")

pie_data <- data.frame(
  category = c("Over 50g Sugar", "Under 50g Sugar"),
  count = c(items_over_sugar_limit, items_under_sugar_limit)
)

pie_data$percent <- round(pie_data$count / sum(pie_data$count) * 100, 1)

p0a <- ggplot(pie_data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Items Compared to Daily Sugar Limit") +
  theme_void() +
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

#print(p0a)

# Pie Chart 2 (1/2 daily sugar limit)
items_under_25 <- sum(data$`Total Sugars (g)` < 25)
items_over_25 <- sum(data$`Total Sugars (g)` >= 25)

pie_data <- data.frame(
  category = c("Under 25g Sugar", "Over 25g Sugar"),
  count = c(items_under_25, items_over_25)
)

pie_data$percent <- round(pie_data$count / sum(pie_data$count) * 100, 1)

p0b <- ggplot(pie_data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Items Under vs Over Half the Daily Sugar Limit (25g)") +
  theme_void() +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

#print(p0b)

# Pie chart 3 (1/4 daily limit)
items_under_12.5 <- sum(data$`Total Sugars (g)` < 12.5)
items_over_12.5 <- sum(data$`Total Sugars (g)` >= 12.5)

pie_data_12.5 <- data.frame(
  category = c("Under 12.5g Sugar", "Over 12.5g Sugar"),
  count = c(items_under_12.5, items_over_12.5)
)

pie_data_12.5$percent <- round(pie_data_12.5$count / sum(pie_data_12.5$count) * 100, 1)

p0c <- ggplot(pie_data_12.5, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Items Under vs Over 12.5g Sugar (Quarter of Daily Limit)") +
  theme_void() +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

#print(p0c)


# Pie chart 4 1/5 the daily sugar limit
items_under_10g <- sum(data$`Total Sugars (g)` < 10)
items_over_10g <- total_items - items_under_10g

percent_under_10g <- round((items_under_10g / total_items) * 100, 1)
percent_over_10g <- round((items_over_10g / total_items) * 100, 1)

pie_data_10g <- data.frame(
  category = c("Under 10g Sugar", "Over 10g Sugar"),
  count = c(items_under_10g, items_over_10g),
  percent = c(percent_under_10g, percent_over_10g)
)

p0d <- ggplot(pie_data_10g, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Items Compared to 10g Sugar Limit (1/5 Daily Limit)") +
  theme_void() +
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

#print(p0d)

fig4 <- grid.arrange(p0a,p0b, nrow = 2)
fig5 <- grid.arrange(p0c,p0d, nrow = 2)
print(fig4)
print(fig5)


#BoxPlot 1
boxplot1 <- ggplot(data, aes(x = `Menu Category`, y = `Energy (kCal)`)) +
  geom_boxplot(fill = "lightblue") +
  xlab("Menu Category") +
  ylab("Calories") +
  ggtitle("Calories by McDonald's Menu Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#BoxPlot 2
boxplot2 <- ggplot(data, aes(x = `Menu Category`, y = `Total Sugars (g)`)) +
  geom_boxplot(fill = "lightgreen") +
  xlab("Menu Category") +
  ylab("Total Sugars (g)") +
  ggtitle("Total Sugars by McDonald's Menu Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot 3
boxplot3 <- ggplot(data, aes(x = `Menu Category`, y = `Total fat (g)`)) +
  geom_boxplot(fill = "purple") +
  xlab("Menu Category") +
  ylab("Total Fat (g)") +
  ggtitle("Total Fat by McDonald's Menu Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#BoxPlot 4
boxplot4 <- ggplot(data, aes(x = `Menu Category`, y = `Total carbohydrate (g)`)) +
  geom_boxplot(fill = "lightyellow") +
  xlab("Menu Category") +
  ylab("Carbohydrates (g)") +
  ggtitle("Carbohydrates by McDonald's Menu Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplotGrid <- grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, ncol=2)
print(boxplotGrid)

#histogram 1
ggplot(data, aes(x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_histogram(binwidth = 50, position = "identity", alpha = 0.5) +
  xlab("Calories") +
  ylab("Count of Items") +
  ggtitle("Calories Distribution by Menu Category") +
  theme_minimal()

#histogram 2
ggplot(data, aes(x = `Total Sugars (g)`)) +
  geom_histogram(binwidth = 2, fill = "pink", color = "black") +
  xlab("Sugar (g)") +
  ylab("Count of Items") +
  ggtitle("Distribution of Sugar Content") +
  theme_minimal()

#RidgeLine graph 1
p <- data %>%
  ggplot(aes(y = `Menu Category`,x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_density_ridges(alpha = 0.6, stat = "binline", bins = 20) +
  theme_ridges() +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  xlab("Calories") +
  ylab("Menu Category") +
  ggtitle("Distribution of Calories by Menu Category")

print(p)

#RidgeLine graph 2
p_sugar <- data %>%
  ggplot(aes(y = `Menu Category`, x = `Total Sugars (g)`, fill = `Menu Category`)) +
  geom_density_ridges(alpha = 0.6, stat = "binline", bins = 20) +
  theme_ridges() +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  xlab("Total Sugars (g)") +
  ylab("Menu Category") +
  ggtitle("Distribution of Sugar Content by Menu Category")

print(p_sugar)

p_chol <- data %>%
  ggplot(aes(y = `Menu Category`, x = `Cholesterols (mg)`, fill = `Menu Category`)) +
  geom_density_ridges(alpha = 0.6, stat = "binline", bins = 20) +
  theme_ridges() +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  xlab("Cholesterol (mg)") +
  ylab("Menu Category") +
  ggtitle("Distribution of Cholesterol by Menu Category")

print(p_chol)

#pairs chart 
test_data <- na.omit(data[,c(4:13)])
pairs(test_data)

#Heatmap
mt <- cor(testdata)
heatmap(mt, scale = 'none', Rowv = NA,
        Colv = NA, margins = c(10, 10))

#linear regression 1
data_no_drinks <- data %>%
  filter(`Menu Category` != "Beverages Menu") %>% filter(`Menu Category` != "Condiments Menu") %>% filter(`Menu Category` != "McCafe Menu")

mod <- lm(`Total fat (g)` ~ `Energy (kCal)`, data = data_no_drinks)
#data_no_drinks$fitted <- fitted(mod)

fig1 <- ggplot(data_no_drinks, aes(y = `Total fat (g)`, x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) + 
  geom_line(aes(y = mod$fitted.values), color = "blue") + 
  xlab("Calories") +
  ylab("Total Fat") + 
  ggtitle("Total Fat vs Calories (excluding drinks, condiments, and cafe menu)") + 
  theme_minimal()

print(fig1)


#linear regression 1.5
mod1_5 <- lm(`Total fat (g)` ~ `Energy (kCal)`, data = data)
fig1_5 <- ggplot(data, aes(y = `Total fat (g)`, x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) + 
  geom_line(aes(y = mod1_5$fitted.values), color = "blue") + 
  xlab("Calories") +
  ylab("Total Fat") + 
  ggtitle("Total Fat vs Calories (Every Menu Item)") + 
  theme_minimal()

print(fig1_5)

#linear regression 2

mod2 <- lm(`Sat Fat (g)` ~ `Energy (kCal)`, data = data_no_drinks)
fig2 <- ggplot(data_no_drinks, aes(y = `Sat Fat (g)`, x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) + 
  geom_line(aes(y = mod2$fitted.values), color = "blue") + 
  xlab("Calories") +
  ylab("Saturated Fat") + 
  ggtitle("Saturated Fat vs Carbs (excluding drinks, condiments, and cafe menu)") + 
  theme_minimal()

print(fig2)

#linear regression 2.5
mod2_5 <- lm(`Sat Fat (g)` ~ `Energy (kCal)`, data = data)
fig2_5 <- ggplot(data, aes(y = `Sat Fat (g)`, x = `Energy (kCal)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) + 
  geom_line(aes(y = mod2_5$fitted.values), color = "blue") + 
  xlab("Calories") +
  ylab("Saturated Fat") + 
  ggtitle("Saturated Fat vs Carbs (Every Menu Item)") + 
  theme_minimal()

print(fig2_5)

#linear regression 3
clean <- data %>% drop_na(`Sodium (mg)`, `Energy (kCal)`) %>% filter(`Menu Category` != "Beverages Menu") %>% filter(`Menu Category` != "Condiments Menu") %>% filter(`Menu Category` != "McCafe Menu")
mod3 <- lm(`Sodium (mg)` ~ `Energy (kCal)`, data = clean)
clean$pred_sodium <- mod3$fitted.values
fig3 <- ggplot(clean, aes(x = `Energy (kCal)`, y = `Sodium (mg)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = pred_sodium), color = "blue") +
  xlab("Calories") +
  ylab("Sodium (mg)") +
  ggtitle("Sodium (mg) vs Energy (Calories) (excluding drinks, condiments, and cafe menu)") +
  theme_minimal()

print(fig3)

#linear regression 3.5
clean_all_data <- data %>% drop_na(`Sodium (mg)`, `Energy (kCal)`)
mod3_5 <- lm(`Sodium (mg)` ~ `Energy (kCal)`, data = clean_all_data)
clean_all_data$pred_sodium <- mod3_5$fitted.values
fig3_5 <- ggplot(clean_all_data, aes(x = `Energy (kCal)`, y = `Sodium (mg)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = pred_sodium), color = "blue") +
  xlab("Calories") +
  ylab("Sodium (mg)") +
  ggtitle("Sodium (mg) vs Energy (Calories) (Every Menu Item)") +
  theme_minimal()

print(fig3_5)


#linear regression 4
data_no_drinks_clean <- data_no_drinks %>% filter(`Added Sugars (g)` != 0)
mod_sugar <- lm(`Total Sugars (g)` ~ `Added Sugars (g)`, data = data_no_drinks_clean)

fig4 <- ggplot(data_no_drinks_clean, aes(x = `Added Sugars (g)`, y = `Total Sugars (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_sugar$fitted.values), color = "blue") +
  xlab("Added Sugars (g)") +
  ylab("Total Sugars (g)") +
  ggtitle("Total Sugars vs Added Sugars (excluding drinks, condiments, and cafe menu)") +
  theme_minimal()

print(fig4)

#linear regression 4.5
data_clean <- data %>% filter(`Added Sugars (g)` != 0)
mod_sugar_4_5 <- lm(`Total Sugars (g)` ~ `Added Sugars (g)`, data = data_clean)

fig4_5 <- ggplot(data_clean, aes(x = `Added Sugars (g)`, y = `Total Sugars (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_sugar_4_5$fitted.values), color = "blue") +
  xlab("Added Sugars (g)") +
  ylab("Total Sugars (g)") +
  ggtitle("Total Sugars vs Added Sugars (Every Menu Item)") +
  theme_minimal()

print(fig4_5)

#linear regression 5
mod_fat <- lm(`Total fat (g)` ~ `Sat Fat (g)`, data = data_no_drinks)

fig5 <- ggplot(data_no_drinks, aes(x = `Sat Fat (g)`, y = `Total fat (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_fat$fitted.values), color = "blue") +
  xlab("Saturated Fat (g)") +
  ylab("Total Fat (g)") +
  ggtitle("Total Fat vs Saturated Fat (excluding drinks, condiments, and cafe menu)") +
  theme_minimal()

print(fig5)

#linear regression 5.5
mod_fat_5_5 <- lm(`Total fat (g)` ~ `Sat Fat (g)`, data = data)

fig5_5 <- ggplot(data, aes(x = `Sat Fat (g)`, y = `Total fat (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_fat_5_5$fitted.values), color = "blue") +
  xlab("Saturated Fat (g)") +
  ylab("Total Fat (g)") +
  ggtitle("Total Fat vs Saturated Fat (Every Menu Item)") +
  theme_minimal()

print(fig5_5)

#linear regression 6
clean <- data %>% filter(!is.na(`Protein (g)`), !is.na(`Sodium (mg)`)) %>% filter(`Menu Category` != "Beverages Menu") %>% filter(`Menu Category` != "Condiments Menu")
mod_protein <- lm(`Protein (g)` ~ `Sodium (mg)`, data = clean)

fig6 <- ggplot(clean, aes(x = `Sodium (mg)`, y = `Protein (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_protein$fitted.values), color = "blue") +
  xlab("Sodium (mg)") +
  ylab("Protein (g)") +
  ggtitle("Protein vs Sodium (excluding drinks, and condiments)") +
  theme_minimal()

print(fig6)

#linear regression 6.5
clean_6_5 <- data %>% filter(!is.na(`Protein (g)`), !is.na(`Sodium (mg)`))
mod_protein_6_5 <- lm(`Protein (g)` ~ `Sodium (mg)`, data = clean_6_5)

fig6_5 <- ggplot(clean_6_5, aes(x = `Sodium (mg)`, y = `Protein (g)`, fill = `Menu Category`)) +
  geom_point(shape = 21, size = 3) +
  geom_line(aes(y = mod_protein_6_5$fitted.values), color = "blue") +
  xlab("Sodium (mg)") +
  ylab("Protein (g)") +
  ggtitle("Protein vs Sodium (Every Menu Item)") +
  theme_minimal()

print(fig6_5)

dev.off()
getwd()

