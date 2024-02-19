library(readr)
layoff <- read_csv("/Users/parul/OneDrive/LAYOFF1.csv")
layoff
str(layoff)

# Load necessary libraries
library(ggplot2)
library(dplyr)



#UNIVARIATE ANALYSIS

#QUESTION : How does the distribution of key company metrics, such as company size before and after layoffs, vary across different stages and industries of companies in the dataset?

boxplot(layoff[,7:10])

#INSIGHTS : The boxplot visualizes the distribution of four numeric attributes (Laid_Off, Percentage, Company_Size_before_Layoffs, 
#Company_Size_after_layoffs) in the layoff dataset. Each box represents the interquartile range (IQR) of the respective attribute, 
#with the median marked by the horizontal line inside the box.


# bar chart 
# QUESTION : How many unique industries are represented in the dataset?

ggplot(layoff, aes(x=Industry)) + geom_bar(position="stack")+
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
labs(x = "Industry", y = "Count", title = "Distribution of layoffs among Industries")

#INSIGHTS : From the provided Bar chart visualization of the layoff dataset, we can observe 
#the distribution of layoffs among different industries. By analyzing the plot, 
#we can determine the number of unique industries represented in the dataset. 
#Additionally, it appears that the Finance industry has the highest number of layoffs 
#compared to other industries, as indicated by the tallest bar in the plot. 


#Density Plot
# QUESTION : What is the density distribution of the percentage of layoffs compared to the total company size?

ggplot(layoff, aes(x = Percentage)) +
geom_density(fill = "skyblue", color = "black") +
labs(title = "Density Plot of Percentage of Layoffs",
x = "Percentage of Layoffs",y = "Density")


#INSIGHT : The density plot reveals that the majority of companies in the dataset 
#exhibit a concentration on the X-axis, particularly within the range of 0 to 25 
#on the percentage scale. Simultaneously, the majority of density is observed on the Y-axis,
# specifically around 0.04 on the density scale. This indicates a pronounced clustering of 
#companies with lower percentages of layoffs compared to their total company size, 
#emphasizing a prevalent trend within the dataset.



# BIVARIATE


#QUESTION : How does the percentage of layoffs vary over time?

layoff.company <- data.frame(ID = as.numeric(rownames(layoff)), layoff[, 7:10])
labs.diagonal <- c("Layoff", "Date Layoffs", "Percentage", "Company Size before Layoffs", "Company Size after layoffs")
plot(layoff$Date_layoffs, layoff$Percentage, xlab = "Date Layoffs", ylab = "Percentage")


#INSIGHTS : The provided plot visualizes the relationship between the date of layoffs (Date_layoffs) and the corresponding percentage of layoffs (Percentage). 


#ggplot
# QUESTION: What is the distribution of Layoff among Countries?


ggplot(layoff, aes(x=Laid_Off,y=Country)) + geom_point(aes(color=Country))+ 
  labs(x="Number of Employees Laid Off", y="Country Name", title="Distribution of layoff among Countries")


# INSIGHTS: From the provided ggplot visualization, it is evident that
#the distribution of layoffs among countries is notably skewed. The scatter plot shows 
#that the number of employees laid off (on the x-axis) across different countries (on the y-axis), 
#with each country represented by a distinct color. The data points reveal that the USA has the highest number of layoffs, 
#followed by India, Canada, Israel, and Germany. This insight suggests a significant variation in layoff magnitudes among countries, 
#with the USA exhibiting the highest impact on employee layoffs.


# QUESTION : How does the distribution of years vary across different continents?


ggplot(layoff, aes(x = Continent, y = Year, fill = Continent)) + geom_violin()


#INSIGHTS : The violin plot of the layoff dataset visualizes the distribution of layoff years 
#across different continents. Each violin represents the distribution of layoff years for a 
#specific continent. The width of the violins indicates the density of layoff years, with wider 
#sections representing higher densities. By examining the violins for each continent, we can gain 
#insights into how the distribution of layoff years differs geographically. 



#QUESTION : How does the distribution of laid-off employees vary across different stages of companies?

ggplot(layoff, aes(x = Stage, y = Laid_Off)) +
  geom_boxplot() + labs(title = "Boxplot of Laid Off Employees by Stage",
                        x = "Stage of Company",y = "Number of Laid Off Employees")

#INSIGHTS : The boxplot illustrates the distribution of laid-off employees across different stages of 
#companies. Each box represents a stage of the company, and the height of the box indicates the interquartile 
#range of laid-off employees within that stage. The median number of laid-off employees is represented by the horizontal line inside each box.



#MULTIVARIATE

#QUESTION: What is the structure and summary statistics of the layoff dataset when visualized as a star plot?

stars(layoff)

#INSIGHTS : provides a visual representation of the dataset's structure and key summary statistics for each variable.
# Each variable is represented by a spoke radiating outward from the center, with the length of the spoke indicating the magnitude of the variable. 


#QUESTION : How do the numeric attributes (Laid_Off, Percentage, Company_Size_before_Layoffs, Company_Size_after_layoffs) correlate with each other in the layoff dataset?

pairs(layoff[,7:10])

#INSIGHTS : The scatterplot matrix that visualizes the pairwise relationships between numeric attributes in the layoff dataset. 
#Each scatterplot represents the relationship between two attributes, with one attribute plotted on the x-axis and the other on the y-axis. 
#By examining the scatterplot matrix, we can observe any patterns or trends in the relationships between the attributes. 


#QUESTION : How do different variables in the layoff dataset relate to each other when visualized using scatterplots and boxplots along the diagonal?

library(SciViews)
pairs(layoff.company, diag.panel = panel.boxplot, labels=labs.diagonal,pch=c(1,16),font.labels=2)

#INSIGHTS :  Each variable is plotted against every other variable, resulting in a grid of scatterplots and boxplots. Along the diagonal of the grid, boxplots are displayed for each individual variable, showing the distribution of values. Scatterplots in the off-diagonal cells show the relationship between pairs of variables. 


#QUESTION : How does the distribution of laid-off employees vary across different stages of companies and years?

ggplot(layoff, aes(x = Laid_Off)) + geom_histogram(fill = "skyblue", color = "black") +
facet_grid(Stage ~ Year) + labs(title = "Distribution of Laid Off Employees by Stage and Year",
x = "Number of Employees Laid Off",y = "Frequency")

#INSIGHTS: The provided visualization, a histogram faceted by the stage of companies and years, 
#reveals insights into the distribution of laid-off employees across different stages of companies and years.
# By examining the histograms within each facet, we can observe how the frequency of laid-off employees varies 
#within each combination of company stage and year. 



#QUESTION : What is the distribution of company sizes before and after layoffs?

ggplot(layoff, aes(x = Company_Size_before_Layoffs, y = Company_Size_after_layoffs, color = Industry)) +
geom_point() + labs(title = "Distribution of Company Sizes Before and After Layoffs by Industry",
x = "Company Size Before Layoffs",y = "Company Size After Layoffs",color = "Industry") +scale_color_discrete(name = "Industry")

#INSIGHTS : The multivariate scatter plot illustrates the distribution of company sizes before 
#and after layoffs across various industries. Each point represents a company, with its position 
#indicating the company size before and after layoffs, and its color representing the industry. 
#The plot allows for a visual examination of how company sizes have changed post-layoffs 
#within different industries. 



ggplot(layoff, aes(x = Continent, y = Percentage, fill = Industry)) +
  geom_violin() +
  facet_grid(Stage ~ Location_HQ) +
  labs(title = "Percentage of Layoffs by Continent, Industry, Stage, and Location of Headquarters",
       x = "Continent",
       y = "Percentage of Layoffs",
       fill = "Industry")


