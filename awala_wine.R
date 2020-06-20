######################################
#title: "Wine Data Analysis"
#author: "Dr. Awala Fortune"
#date: "`r format(Sys.Date())`"
#output:
#  pdf_document: 
#  citation_package: natbib
#df_print: kable
#highlight: tango
#keep_tex: yes
#number_sections: yes
#toc: yes
#html_document:
#  df_print: kable
#number_sections: yes
#theme: cerulean
#toc: yes
#toc_depth: 3
#toc_float: yes
#geometry:
#  - top=25mm
#- bottom=25mm
#- left=25mm
#- right=25mm
#- heightrounded
#highlight-style: pygments 
#linkcolor: blue
#urlcolor: blue
#mainfont: Arial
#fontsize: 12pt
#sansfont: Verdana
#documentclass: report
#####################################
#$$/pagebreak
### I. Introduction###
##1.1. General description of Dataset:##
### II. Methods and Analysis###
#load libraries
library("ggplot2")
library("dplyr")
library("gridExtra")
library(Simpsons)
library(GGally)
library(memisc)
library(pander)
library(corrplot)
library(RCurl)
##2.1. Data Preparation:##
library(tidyverse)
# Define F1 score function
F1_score <- function(prec, rec) {
  2 * (prec * rec) / (prec + rec)
}

# Create a dataframe with precision and recall values
w <- expand.grid(precision = seq(0, 1, .01), 
                 recall = seq(0, 1, .01))

tib <- tibble(precision = w$precision, 
              recall = w$recall, 
              F1 = F1_score(w$precision, w$recall))

# Create the plot
tib %>% ggplot(aes(precision, recall, z = F1, fill = F1)) +
  geom_raster() +
  labs(title = "F1 Score") +
  xlab("Precision") +
  ylab("Recall") +
  scale_fill_gradientn(colors=c("#F70D0D", "white", "#005DFF")) +
  # Draw countour lines
  stat_contour(breaks=c(0.1), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.2), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.3), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.4), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.5), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.6), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.7), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.8), color="black", na.rm = TRUE) +
  stat_contour(breaks=c(0.9), color="black", na.rm = TRUE) +
  # Write the line levels
  geom_text(aes(x = 0.15, y = 0.1, label = "0.1")) +
  geom_text(aes(x = 0.25, y = 0.2, label = "0.2")) +
  geom_text(aes(x = 0.35, y = 0.3, label = "0.3")) +
  geom_text(aes(x = 0.45, y = 0.4, label = "0.4")) +
  geom_text(aes(x = 0.55, y = 0.5, label = "0.5")) +
  geom_text(aes(x = 0.65, y = 0.6, label = "0.6")) +
  geom_text(aes(x = 0.75, y = 0.7, label = "0.7")) +
  geom_text(aes(x = 0.85, y = 0.8, label = "0.8")) +
  geom_text(aes(x = 0.95, y = 0.9, label = "0.9"))
options(digits = 3)
load_lib <- function(libs) {
  sapply(libs, function(lib) {
    # Load the package. If it doesn't exists, install and load.
    if(!require(lib, character.only = TRUE)) {
      
      # Install the package
      install.packages(lib)
      
      # Load the package
      library(lib, character.only = TRUE)
    }
  })}

# Load the libraries used in this section
libs <- c("tidyverse", "icesTAF", "readr", 
          "lubridate", "caret")

load_lib(libs)

# Download the datasets from UCI repository
if(!dir.exists("data")) mkdir("data")
if(!file.exists("data/winequality-red.csv")) 
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", "data/winequality-red.csv")
if(!file.exists("data/winequality-white.csv")) 
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", "data/winequality-white.csv")
if(!file.exists("data/winequality.names")) 
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality.names", "data/winequality.names")

# Import the datasets.
# 'red' is the red wine dataset
# 'white' is the white wine dataset.
red   <- read_delim("data/winequality-red.csv", 
                    delim = ";", 
                    locale = locale(decimal_mark = ".", 
                                    grouping_mark = ","), 
                    col_names = TRUE)
white <- read_delim("data/winequality-white.csv", 
                    delim = ";", 
                    locale = locale(decimal_mark = ".", 
                                    grouping_mark = ","), 
                    col_names = TRUE)

# Set column names
cnames <- c("fixed_acidity", "volatile_acidity", "citric_acid",
            "residual_sugar", "chlorides", "free_sulfur_dioxide",
            "total_sulfur_dioxide", "density", "pH",
            "sulphates", "alcohol", "quality")

# Columns used for prediction are all columns
# except 'quality'.
xcol <- c("fixed_acidity", "volatile_acidity", "citric_acid",
          "residual_sugar", "chlorides", "free_sulfur_dioxide",
          "total_sulfur_dioxide", "density", "pH",
          "sulphates", "alcohol")

colnames(red)   <- cnames
colnames(white) <- cnames
# Add the column 'type' to define the type of wine
red   <- mutate(red,   type = "red")
white <- mutate(white, type = "white")

# Join 'red' and 'white' datasets
wine <- rbind(red, white)
wine <- mutate(wine, 
               quality = as.factor(quality),
               type = as.factor(type))
levels(wine$quality) <- paste0("Q", levels(wine$quality))
# Test set will be 10% of the entire dataset
set.seed(2020, sample.kind = "Rounding")
test_index <- createDataPartition(y = wine$type, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)

# Train and test sets for wine type
train_set <- wine[-test_index,]
test_set  <- wine[test_index,]

# Train and test sets for red wine quality
train_set_r <- train_set[which(train_set$type == "red"),]
test_set_r  <- test_set[which(test_set$type == "red"),]

train_set_r$quality <- factor(train_set_r$quality)
test_set_r$quality  <- factor(test_set_r$quality)
#========================================
# Data Explorations
#========================================
# After importing the dataset, it's good practice to check the data.
# Here, we make some basic data checking.


# Check for empty values (NAs) in the dataset
sum(is.na(wine))
# Identification of near zero variance predictors
nearZeroVar(train_set[, xcol], saveMetrics = TRUE)
# Compactly Display the Structure of an Arbitrary R Object
str(train_set)
head(train_set)
tail(train_set)
# Statistics summary
summary(train_set)
glimpse(train_set)
##3.2. Red and White Wine Ubiquity or Prevalence:##
# Distribution of red and white wines
ggplot(data = train_set) + 
  geom_bar(aes(type, fill = type)) +
  labs(title = "Ubiquity of red and white wines",
       caption = "Source: train_set dataset.") +
  theme(legend.position = 'none')
# Install and load the libraries used for visualization
# The 'load_lib' function was defined earlier.
load_lib(c("gridExtra", "ggridges", "ggplot2",
           "gtable", "grid", "egg"))


# The 'grid_arrange_shared_legend' function creates a grid of 
# plots with one legend for all plots.
# Reference: Baptiste Auguié - 2019
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }
load_lib(c("readxl", "huxtable", "viridis", "ggthemes"))

# The 'huxtable' package creates beautiful tables.
# 'viridis' and 'ggthemes' have color paletes for color blind people

# Download stats file from vinho verde official portal
if(!file.exists("data/vv-stats.xls")) 
  download.file("https://portal.vinhoverde.pt/pt/file/c/1614",
                "data/vv-stats.xls",
                cacheOK = FALSE,
                method = "auto",
                mode = "wb")
# Import stats file
vv_stats <- read_excel(path = "data/vv-stats.xls",
                       sheet = "vinho",
                       range = "A6:D16")

# Calculate the prevalence of red wine
vv_stats <- vv_stats[2:nrow(vv_stats),c(1,3:4)] %>% 
  mutate(Prevalence = 100 * TINTO / BRANCO)

# Create a table with the values.
# Change column names
colnames(vv_stats) <- c("Year", "White", "Red", "Red Prevalence (%)")
vv_stats <- as_hux(vv_stats)

vv_stats <- huxtable::add_colnames(vv_stats)

vv_stats <- vv_stats %>%
  # Format header row
  set_bold(row = 1, col = 1:ncol(vv_stats), value = TRUE)        %>%
  set_top_border(row = 1, col = 1:ncol(vv_stats), value = 1)     %>%
  set_bottom_border(row = c(1,10), col = 1:ncol(vv_stats), value = 1)  %>%
  # Format cells
  set_align(row = 1:4, col = 2, value = 'right')                 %>%
  set_number_format(row = 1:nrow(vv_stats), col = c(2,3), 
                    value = list(function(x)
                      prettyNum(x, big.mark = ",",
                                scientific = FALSE)))            %>% 
  set_number_format(row = 1:nrow(vv_stats), col = 4, value = 2)  %>% 
  # Format table
  set_width(value = 0.6) %>%
  set_caption("Vinho Verde Annual Production 1999-2008")         %>%
  set_position(value = "center")

# Show the table
vv_stats
##3.3. Quality distribution:##
# Create a plot with the downloaded data.
# The plot is easier to see the values than in the table
# Distribution of quality values
ggplot(data = train_set_r, 
       aes(x = quality, fill ='red')) +
  geom_bar() +
  theme(legend.position="none") +
  labs(title = "Distribution of quality of red wine",
       caption = "Source: train_set_r dataset")
##3.4. Important Variables:##
#---------------------------------------
# Important Variables 
#---------------------------------------
# The important variable gives an estimate of the predictive power
# of each feature. 
# Check the help file for 'filterVarImp' for more information.

# Variable importance for wine type
hux(Feature = rownames(filterVarImp(x = train_set[,xcol], 
                                    y = train_set$type)),
    Red   = filterVarImp(x = train_set[,xcol], 
                         y = train_set$type)$red,
    White = filterVarImp(x = train_set[,xcol],
                         y = train_set$type)$white,
    add_colnames = TRUE) %>%
  arrange(desc(Red)) %>% 
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)    %>%
  set_bottom_border(row = c(1,12), everywhere, value = 1)    %>%
  set_align(row = everywhere, col = 2:3, value = 'right') %>%
  set_caption('Important Variables of Wine Type') %>%
  set_position(value = "center")
ggplot(data = train_set, aes(x = volatile_acidity, y = total_sulfur_dioxide )) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")
ggplot(data = train_set, aes(x = volatile_acidity, y = chlorides )) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")
ggplot(data = train_set, aes(x = chlorides, y = total_sulfur_dioxide )) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("chlorides in Log Scale") +
  geom_smooth(method="lm")
#------------------
#Important Variables  for red wine quality
#------------------
x <- train_set_r[,xcol]
y <- train_set_r$quality

hux(Feature = rownames(filterVarImp(x = x, y = y)),
    filterVarImp(x = x, y = y),
    add_colnames = TRUE) %>%
  # Format header row
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)       %>%
  set_bottom_border(row = c(1,12), everywhere, value = 1)    %>%
  # Format numbers
  set_number_format(row = 2:12, col = 2:7, value = 3)  %>%
  
  
  map_text_color(row = everywhere, col = 2:7, 
                 by_ranges(seq(0.6, 0.9, 0.1), colorblind_pal()(5))) %>%
  # Format alignment
  set_align(row = everywhere, col = 1,   value = 'left')  %>%
  set_align(row = everywhere, col = 2:7, value = 'right') %>%
  # Title
  set_caption('Important variables of red Wine quality') %>%
  set_position(value = "center")
# Here we create a plot of variable information of wine quality.
# The same info as in the table above.
# Variable importance for red wine quality
x <- train_set_r[,xcol]
y <- train_set_r$quality
y <- factor(y)

data.frame(Feature = rownames(filterVarImp(x = x, y = y)),
           filterVarImp(x = x, y = y)) %>%
  pivot_longer(col = 2:7, names_to = "Quality",
               values_to = "Value", values_drop_na = TRUE) %>%
  ggplot(aes(x = Feature, y = Value)) +
  geom_col(fill = "blue") +
  coord_flip() +
  ggtitle("Important Variables of red wine quality") +
  theme(legend.position = "none") +
  ylab("Relative Importance") +
  geom_hline(yintercept = seq(0.5, 0.9, 0.1), color = "red") +
  facet_wrap("Quality")
#========================================
# Data visualization
#========================================
# In this section we create several stats plots
# to check the distribution of variables.

# Install and load the libraries used for visualization
# The 'load_lib' function was defined earlier.
load_lib(c("gridExtra", "ggridges", "ggplot2",
           "gtable", "grid", "egg"))


# The 'grid_arrange_shared_legend' function creates a grid of 
# plots with one legend for all plots.
# There's no commentaries because I use the code from the source below.
# Reference: Baptiste Auguié - 2019
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }
##3.5 Density Plot##
# Density grid
dens_grid <- lapply(xcol, FUN=function(var) {
  # Build the plots
  ggplot(train_set) + 
    geom_density(aes_string(x = var, fill = "type"), alpha = 0.5) +
    ggtitle(var)
})
do.call(grid_arrange_shared_legend, args=c(dens_grid, nrow = 4, ncol = 3))
# The distribution of predictors overlap for all quality levels.
# Maybe if we group the quality levels there's less overlap.
# Here we create 2 datasets to predict wine quality on the new levels.
train_set_r <- train_set_r %>% 
  mutate(quality2 = factor(case_when(
    quality %in% c("Q3", "Q4") ~ "low",
    quality %in% c("Q5", "Q6") ~ "medium",
    quality %in% c("Q7", "Q8") ~ "high"),
    levels = c("low", "medium", "high")))

test_set_r <- test_set_r %>% 
  mutate(quality2 = factor(case_when(
    quality %in% c("Q3", "Q4") ~ "low",
    quality %in% c("Q5", "Q6") ~ "medium",
    quality %in% c("Q7", "Q8") ~ "high"),
    levels = c("low", "medium", "high")))

# Plot the distribution of new quality levels
train_set_r %>% ggplot(aes(quality2, fill = quality2)) + geom_bar()
##3.6  Box Plot##
# Now we try to find the relationship between 'quality' and 
# the features.
#
# Another boxplot to check if the grouping improved the overlaps.
# Arrange the dataset
train_set_r[,c(cnames, "quality2")] %>% 
  pivot_longer(cols = -c(12:13), 
               names_to = "Feature", 
               values_to = "Value") %>%
  # Create the box plot
  ggplot(aes(x = quality2, y= Value, fill = quality2)) +
  geom_boxplot() +
  # Format labels
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ggtitle("Quality of Red Wine by feature") +
  # Create grid by feature
  facet_wrap(. ~ Feature, scales = "free", ncol = 3, shrink = FALSE)
##3.7.  Quantile-Quantile plot:##
#------------------
# Density ridge plots
#------------------
# Predict red wine quality
# It's easier to see the distribution with density ridge plots.
# It plots each quality level in a different row.
lapply(xcol, FUN=function(var) {
  
  train_set_r %>% 
    ggplot(data = ., aes_string(x = var, 
                                y = "quality", 
                                fill = "quality", 
                                alpha = 0.5)) + 
    geom_density_ridges() +
    theme_ridges() +
    theme(axis.text.x = element_text(hjust = 1)) +
    scale_fill_brewer(palette = 4) +
    ggtitle(paste0("Red wine quality by ", var))
})

#------------------
# QQ plots
#------------------
# QQ Plots help identify if the feature is normally distributed.
#
# Create a grid for each wine type
# 'rw' = red / white
qq_plot_grid <- lapply(xcol, FUN=function(var) {
  train_set_r %>% 
    dplyr::select(var) %>%
    ggplot(data = ., aes(sample = scale(.))) + 
    stat_qq() +
    stat_qq_line(colour = "red") +
    theme(axis.text.x = element_text(hjust = 1)) +
    ggtitle(var)
})
do.call(grid.arrange, args=c(qq_plot_grid, list(ncol=3)))
##3.8.  Density Ridge##  
# Density ridge plots
lapply(xcol, FUN=function(var) {
  
  train_set_r %>% 
    ggplot(data = ., aes_string(x = var, 
                                y = "quality2", 
                                fill = "quality2", 
                                alpha = 0.5)) + 
    geom_density_ridges() +
    # Format chart
    theme_ridges() +
    scale_fill_brewer(palette = 4) +
    # Format labels
    theme(axis.text.x = element_text(hjust = 1)) +
    ggtitle(paste0("Red wine quality by ", var)) +
    ylab("Quality")
})
##3.9.  Correlation:## 
#------------------
# Correleation
#------------------
# We want features with low correlation with each other.
# Load the "corrgram" package to draw a correlogram
load_lib("corrgram")

# Draw a correlogram
corrgram(train_set[,xcol], order=TRUE, 
         lower.panel = panel.shade, 
         upper.panel = panel.cor, 
         text.panel  = panel.txt,
         main = "Correlogram: Wine Physicochemical Properties",
         col.regions=colorRampPalette(c("red", "burlywood1", "blue",
                                        "darkgreen")))

# The correlogram has many information, so we filter only
# the features with high correlation and show in a table.
# 'High' correlation here is above 0.5 and lower than -0.5.
#
# Load the 'huxtable' package to format tables
load_lib("huxtable")
options(huxtable.knit_print_df = FALSE)

# Column names, same as 'xcol' but more beautiful
variable_names <- c("Fixed acidity", "Volatile acidity", "Citric acid",
                    "Residual sugar", "Chlorides", "Free sulfur dioxide",
                    "Total sulfur dioxide", "Density", "pH", "Sulphates",
                    "Alcohol")
# Calculate the correlation of all predictors
my_cor <- as.data.frame(cor(train_set[,xcol]))

# Row (r) and column (c) numbers of the correlation matrix
# filtered by high correlated features (cor <= -0.5 or cor >= 0.5)
r <- which(my_cor <=-0.5 | my_cor >= 0.5 & my_cor != 1, arr.ind=TRUE)[,"row"]
c <- which(my_cor <=-0.5 | my_cor >= 0.5 & my_cor != 1, arr.ind=TRUE)[,"col"]

# Create a table with high correlations features only
my_cor_hux <- hux(`Feature 1` = variable_names[r], 
                  `Feature 2` = variable_names[c], 
                  Correlation = sapply(1:length(r), function(x)
                    my_cor[r[x],c[x]]),
                  add_colnames = TRUE) %>%
  # Format the table
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)    %>%
  set_bottom_border(row = c(1,7), everywhere, value = 1)    %>%
  set_align(row = 1, col = 2:3, value = 'center') %>%
  set_number_format(row = 2:7, col = 3, value = 3)              %>% 
  set_caption('High Correlated Features') %>%
  set_position(value = "center")

# Show the table
my_cor_hux
# Previously, we identified 3 features that may be used in prediction
# of wine type. We create a table to check if the correlation 
# between each pair is low.


# Calculate the correlations for volatile acid, chlorides and total sulfur dioxide

# Variable names
pred1 <- c("volatile_acidity", "chlorides", "total_sulfur_dioxide")

# Nice variable names
xpred2 <- c("Volatile acidity", "Chlorides", "Total sulfur dioxide")

# Calculate the correlation of all predictors
my_cor2 <- as.data.frame(cor(train_set[,pred1]))

# Create a table with high correlations features only
cor_hux2 <- hux(cor(train_set[,pred1]),
                add_colnames  = FALSE, 
                add_rownames = FALSE) 

# Set row and column names
rownames(cor_hux2) <- xpred2
colnames(cor_hux2) <- xpred2
cor_hux2 <- add_rownames(cor_hux2, colname = "Feature")
cor_hux2 <- add_colnames(cor_hux2, value = TRUE)

# Format the table
cor_hux2 <- cor_hux2 %>%
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)    %>%
  set_bottom_border(row = c(1,4), everywhere, value = 1)    %>%
  set_align(row = 1, col = 2:ncol(my_cor_hux), value = 'center') %>%
  set_number_format(everywhere, everywhere, value = 3)              %>% 
  set_caption('Correlation Matrix - Selected Features') %>%
  set_position(value = "center") %>%
  set_width(value = 0.6)

# Show the table
cor_hux2
##3.10.  Modeling##
#========================================
# Modeling and results
#========================================
# Here we make predictions with information gained from
# data exploration and visualization.
#

# Formula used in predictions
pml <- as.formula(paste("type", "~", 
                        paste(xcol, collapse=' + ')))
#------------------
# Single predictor
#------------------
# Predict wine type with total_sulfur_dioxide + chlorides + volatile_acidity
# The first prediction is very simple. We predict 'red' if
# the feature value is above a certain cutoff value, and 'white' 
# otherwise. 
# We do this for the 3 best features discorevered in data exploration.
# Then we combine the results in a single ensemble.
#
# Create a list with variable names and cutoff decision rule.
# If the predicted value is lower than the cutoff value, the first color
# is chosen, otherwise the second. To understand this, look at the
# density plots in data visualization.
t_var <- list( c("white", "red"), c("white", "red"), c("red", "white"))
names(t_var) <- c("volatile_acidity", "chlorides", "total_sulfur_dioxide")


# Create an empty results table. The first row
# contains NAs and will be removed after the predictions.
t_results <<- data.frame(Feature = NA,
                         Accuracy = NA,
                         Sensitivity = NA,
                         Specificity = NA,
                         stringsAsFactors = FALSE)

# Prediction function
preds <- sapply(1:length(t_var), function(x){
  var <- names(t_var[x])
  
  # Cutoff value is the distribution range divided by 500
  cutoff <- seq(min(train_set[,var]), 
                max(train_set[,var]), 
                length.out = 500)
  
  # Calculate accuracy
  acc <- map_dbl(cutoff, function(y){
    type <- ifelse(train_set[,var] < y, t_var[[x]][1], 
                   t_var[[x]][2]) %>% 
      factor(levels = levels(train_set$type))
    
    # Accuracy
    mean(type == train_set$type)
  })
  
  # Build the accuracy vs cutoff curve
  acc_plot <- data.frame(cutoff = cutoff, Accuracy = acc) %>%
    ggplot(aes(x = cutoff, y = Accuracy)) + 
    geom_point() +
    ggtitle(paste0("Accuracy curve for ", var))
  
  # Print the plot
  print(acc_plot)
  # Predict new values in the test set
  # The model uses the cutoff value with the best accuracy.
  max_cutoff <- cutoff[which.max(acc)]
  p_hat <- ifelse(test_set[,var] < max_cutoff,
                  t_var[[x]][1], t_var[[x]][2]) %>% 
    factor(levels = levels(test_set$type))
  
  # Calculate accuracy, specificity and sensitivity
  acc <- max(acc)
  sens <- sensitivity(p_hat, test_set$type)
  spec <- specificity(p_hat, test_set$type)
  
  # Update results table
  t_results <<- rbind(t_results,
                      data.frame(Feature = names(t_var[x]),
                                 Accuracy = acc,
                                 Sensitivity = sens,
                                 Specificity = spec,
                                 stringsAsFactors = FALSE))
  
  # The prediction will be used in the ensemble
  return(p_hat)
})  

# Remove first row with NA
t_results <- t_results[2:nrow(t_results),]

# Combine the results using majority of votes
p_hat_ens <-as_factor(data.frame(preds) %>%
                        mutate(x = as.numeric(preds[,1] == "red") + 
                                 as.numeric(preds[,2] == "red") + 
                                 as.numeric(preds[,3]  == "red"),
                               p_hat = ifelse(x >=2, "red", "white")) %>%
                        pull(p_hat))

# Update results table
t_results <<- rbind(t_results,
                    data.frame(Feature = "Ensemble",
                               Accuracy = mean(p_hat_ens == test_set$type),
                               Sensitivity = sensitivity(p_hat_ens, test_set$type),
                               Specificity = specificity(p_hat_ens, test_set$type),
                               stringsAsFactors = FALSE))
as_hux(t_results,
       add_colnames = TRUE) %>%
  # Format header row
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)       %>%
  set_bottom_border(row = c(1,5), everywhere, value = 1)    %>%
  # Format numbers
  set_number_format(row = everywhere, col = 2:4, value = 3)  %>%
  # Format alignment
  set_align(row = everywhere, col = 1,   value = 'left')  %>%
  set_align(row = everywhere, col = 1:4, value = 'right') %>%
  set_caption("Best Performance for Combined Predictions")  %>%
  set_position(value = "center")
#------------------
# Linear Regression
#------------------
# Predict wine type with total_sulfur_dioxide + chlorides + volatile_acidity

# Train the linear regression model
ft_lm <- train_set %>% 
  # Convert the outcome to numeric
  mutate(type = ifelse(type == "red", 1, 0)) %>%
  # Fit the model
  lm(type ~ total_sulfur_dioxide + chlorides + volatile_acidity, data = .)

# Predict
f_hat_lm <- predict(ft_lm, newdata = test_set)

# Convert the predicted value to factor
p_hat_lm <- factor(ifelse(f_hat_lm > 0.5, "red", "white"))

# Evaluate the results
caret::confusionMatrix(p_hat_lm, test_set$type)


#------------------
# Knn
#------------------
# Predict wine type with all features
# Train
ft_knn <- knn3(formula = pml, data = train_set, k = 5)

# Predict
p_knn <- predict(object = ft_knn, 
                 newdata = test_set, 
                 type ="class")

# Compare the results: confusion matrix
caret::confusionMatrix(data = p_knn, 
                       reference = test_set$type, 
                       positive = "red")

# F1 score
F_meas(data = p_knn, reference = test_set$type)
##3.12.  Regression tree - rpart:##
#------------------
# Regression tree
#------------------
# Predict wine type with all features

# The "rpart" package trains regression trees and 
# "rpart.plot" plots the tree
load_lib(c("rpart", "rpart.plot"))

# Train the model
ft_rpart <- rpart::rpart(formula = pml, 
                         method = "class", 
                         data = train_set)
# Predict
p_rpart <- predict(object = ft_rpart, 
                   newdata = test_set, 
                   type = "class")

# Compare the results: confusion matrix
caret::confusionMatrix(data = p_rpart, 
                       reference = test_set$type, 
                       positive = "red")

# Plot the result
rpart.plot(ft_rpart)

# F1 score
F_meas(data = p_rpart, reference = test_set$type)

# Variable importance
caret::varImp(ft_rpart)
##3.13. Random Forest:##
#------------------
# Random Forest
#------------------
# Predict wine type with all features

# The "randomForest" package trains classification and regression
# with Random Forest
load_lib("randomForest")

# Train the model
ft_rforest <- randomForest(formula = pml, data = train_set)

# Predict
p_rf <- predict(object = ft_rforest, newdata = test_set)

# Compare the results: confusion matrix
caret::confusionMatrix(data = p_rf, 
                       reference = test_set$type, 
                       positive = "red")
# F1 score
F_meas(data = p_rf, reference = test_set$type)
# Plot the error curve
data.frame(ft_rforest$err.rate) %>% mutate(x = 1:500 ) %>% 
  ggplot(aes(x = x)) + 
  #  geom_line(aes(y = OOB)) +
  geom_line(aes(y = red),   col = "red") +
  geom_line(aes(y = white), col = "purple") +
  ggtitle("Random Forest Error Curve") +
  ylab("Error") +
  xlab("Number of trees") +
  geom_text(aes(x = 70,  y = 0.02), label = "Red wine", col = "red") + 
  #  geom_text(aes(x = 100, y = 0.01), label = "Error") +
  geom_text(aes(x = 100, y = 0), label = "White wine", col = "purple")
# Variable importance plot
varImpPlot(ft_rforest, main = "Random Forest Important Variables")
##3.14 Linear Discriminant Analysis - LDA:##
#------------------
# LDA
#------------------
# Predict wine type with all features
load_lib("MASS")

# Train the model
ft_lda <- lda(formula = pml, data = train_set)
# Predict
p_lda <- predict(object = ft_lda, newdata = test_set)

# Compare the results: confusion matrix
caret::confusionMatrix(data = p_lda[[1]], 
                       reference = test_set$type, 
                       positive = "red")

# F1 score
F_meas(data = p_lda[[1]], reference = test_set$type)

# Plot the result
plot(ft_lda)
##3.15  Quadratic Discriminant Analysis - QDA:##
#------------------
# QDA
#------------------
# Predict wine type with all features
load_lib(c("MASS", "scales"))

# Train the model
ft_qda <- qda(formula = pml, data = train_set)

# Predict
p_qda <- predict(object = ft_qda, newdata = test_set)

# Compare the results: confusion matrix
caret::confusionMatrix(data = p_qda[[1]], 
                       reference = test_set$type, 
                       positive = "red")
# F1 score
F_meas(data = p_qda[[1]], reference = test_set$type)
##3.16 Prediction Results Evaluation:##
data.frame(Model = c("Single predictor", "Linear Regression", "Knn", 
                     "Regression trees", "Random forest",
                     "LDA", "QDA"),
       Accuracy = c(percent(mean(p_hat_ens  == test_set$type), accuracy = 0.1),
                        percent(mean(p_hat_lm   == test_set$type), accuracy = 0.1),
                        percent(mean(p_knn      == test_set$type), accuracy = 0.1),
                        percent(mean(p_rpart    == test_set$type), accuracy = 0.1),
                        percent(mean(p_rf       == test_set$type), accuracy = 0.1),
                        percent(mean(p_lda[[1]] == test_set$type), accuracy = 0.1),
                        percent(mean(p_qda[[1]] == test_set$type), accuracy = 0.1)),
           
      Sensitivity = c(percent(sensitivity(p_hat_ens,  test_set$type), accuracy = 0.1),
                       percent(sensitivity(p_hat_lm,   test_set$type), accuracy = 0.1),
                       percent(sensitivity(p_knn,      test_set$type), accuracy = 0.1), 
                       percent(sensitivity(p_rpart,    test_set$type), accuracy = 0.1), 
                       percent(sensitivity(p_rf,       test_set$type), accuracy = 0.1), 
                       percent(sensitivity(p_lda[[1]], test_set$type), accuracy = 0.1), 
                       percent(sensitivity(p_qda[[1]], test_set$type), accuracy = 0.1)), 
           
      Specificity = c(percent(specificity(p_hat_ens,  test_set$type), accuracy = 0.1),
                       percent(specificity(p_hat_lm,   test_set$type), accuracy = 0.1),
                       percent(specificity(p_knn,      test_set$type), accuracy = 0.1),
                       percent(specificity(p_rpart,    test_set$type), accuracy = 0.1),
                       percent(specificity(p_rf,       test_set$type), accuracy = 0.1),
                       percent(specificity(p_lda[[1]], test_set$type), accuracy = 0.1),
                       percent(specificity(p_qda[[1]], test_set$type), accuracy = 0.1)))
##3.17. Cross Validation and Ensemble:##
# Now we are going to do several things:
# 1. train 10 classification models,
# 2. make the predictions for each model
# 3. calculate some statistics and store in the 'results' table
# 4. plot the ROC and precision-recall curves
# Then, we're going to plot the values in the 'results' table
# and make the ensemble of all models together.

# Load the packages used in this section
# Package "pROC" creates ROC and precision-recall plots
load_lib(c("pROC", "plotROC"))

# Several machine learning libraries
load_lib(c("e1071", "dplyr", "fastAdaboost", "gam", 
           "gbm", "import", "kernlab", "kknn", "klaR", 
           "MASS", "mboost", "mgcv", "monmlp", "naivebayes", "nnet", "plyr", 
           "ranger", "randomForest", "Rborist", "RSNNS", "wsrf"))


# Define models
models <- c("glm", "lda", "naive_bayes", "svmLinear", "rpart",
            "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# We run cross validation in 10 folds, training with 90% of the data.
# We save the prediction to calculate the ROC and precision-recall curves
# and we use twoClassSummary to compute the sensitivity, specificity and 
# area under the ROC curve
contrl <- trainControl(method = "cv", number = 10, p = .9,
                       summaryFunction = twoClassSummary, 
                       classProbs = TRUE,
                       savePredictions = TRUE)

contrl <- trainControl(method = "cv", number = 10, p = .9,
                       classProbs = TRUE,
                       savePredictions = TRUE)

# Create 'results' table. The first row
# contains NAs and will be removed after
# the training
results <- tibble(Model = NA,
                  Accuracy = NA,
                  Sensitivity = NA,
                  Specificity = NA,
                  F1_Score = NA,
                  AUC = NA)
#-------------------------------
# Start parallel processing
#-------------------------------
# The 'train' function in the 'caret' package allows the use of
# parallel processing. Here we enable this before training the models.
# See this link for details:
# http://topepo.github.io/caret/parallel-processing.html
cores <- 4    # Number of CPU cores to use
# Load 'doParallel' package for parallel processing
load_lib("doParallel")
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
set.seed(1234, sample.kind = "Rounding")
# Formula used in predictions
pml <- as.formula(paste("type", "~", 
                        paste(xcol, collapse=' + ')))

# Run predictions
preds <- sapply(models, function(model){ 
  
  if (model == "knn") {
    # knn use custom tuning parameters
    grid <- data.frame(k = seq(3, 50, 2))
    ft <- caret::train(form = pml, 
                       method = model, 
                       data = train_set, 
                       trControl = contrl,
                       tuneGrid = grid)
  } else if (model == "rforest") {
    # Random forest use custom tuning parameters
    t_grid <- data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100))
    
    ft <- caret::train(form = pml,
                       method = "rforest", 
                       data = train_set,
                       trControl = contrl,
                       ntree = 150,
                       tuneGrid = t_grid,
                       nSamp = 5000)
  } else {
    # Other models use standard parameters (no tuning)
    ft <- caret::train(form = pml, 
                       method = model, 
                       data = train_set, 
                       trControl = contrl)
  }
  
  # Predictions
  pred <- predict(object = ft, newdata = test_set)
  
  # Accuracy
  acc <- mean(pred == test_set$type)
  
  # Sensitivity
  sen <- sensitivity(data = pred, 
                     reference = test_set$type, 
                     positive = "red")
  # Specificity
  spe <- specificity(data = pred, 
                     reference = test_set$type, 
                     positive = "red")
  
  # F1 score
  f1 <- F_meas(data = factor(pred), reference = test_set$type)
  
  # AUC
  auc_val <- auc(ft$pred$obs, ft$pred$red)
  
  # Store stats in 'results' table
  results <<- rbind(results,
                    tibble(
                      Model = model,
                      Accuracy = acc,
                      Sensitivity = sen,
                      Specificity = spe,
                      AUC = auc_val,
                      F1_Score = f1))
  
  # The predictions will be used for ensemble
  return(pred)
}) 

# Remove the first row of 'results' that contains NAs
results <- results[2:(nrow(results)),]
#-------------------------------
# Combine all models
#-------------------------------
# Use votes method to ensemble the predictions
votes <- rowMeans(preds == "red")
p_hat <- factor(ifelse(votes > 0.5, "red", "white"))

# Update the 'results' table
results <<- rbind(results,
                  tibble(
                    Model = "Ensemble",
                    Accuracy = mean(p_hat == test_set$type),
                    Sensitivity = sensitivity(p_hat, test_set$type),
                    Specificity = specificity(p_hat, test_set$type),
                    AUC = auc(p_hat, as.numeric(test_set$type)),
                    F1_Score = F_meas(p_hat, test_set$type)))

as_hux(results,
       add_colnames = TRUE) %>%
  # Format header row
  set_bold(row = 1, everywhere, value = TRUE)          %>%
  set_top_border(row = 1, everywhere, value = 1)       %>%
  set_bottom_border(row = c(1,13), everywhere, value = 1)    %>%
  # Format numbers
  set_number_format(row = -1, col = 2:6, value = 3)  %>%
  # Format alignment
  set_align(row = everywhere, col = 1,   value = 'left')  %>%
  set_align(row = everywhere, col = 2:6, value = 'right') %>%
  # Title
  set_caption('Model Performance With Cross Validation') %>%
  set_position(value = "center")
hux(Accuracy  = results[which.max(results$Accuracy),1]$Model,
    Sensitivity = results[which.max(results$Sensitivity),1]$Model,
    Specificity = results[which.max(results$Specificity),1]$Model,
    F_1   = results[which.max(results$F1_Score),1]$Model,
    AUC  = results[which.max(results$AUC),1]$Model,
    add_colnames = TRUE) %>%
  # Format header row
  set_bold(row = 1, col = everywhere, value = TRUE)        %>%
  set_top_border(row = 1, col = everywhere, value = 1)     %>%
  set_bottom_border(row = c(1,2), col = everywhere, value = 1)  %>%
  # Format table
  set_width(value = 0.6)                                   %>%
  set_caption("Best model")                                %>%
  set_position(value = "center")
#-------------------------------
# Plot the 'results' table
#-------------------------------
# We create a grid with all plots together.
# Each plot is simple Model vs Stats
results %>% 
  # Convert columns to lines
  pivot_longer(cols = 2:6, names_to = "Metric", values_drop_na = TRUE) %>%
  ggplot(aes(x = Model, y = value, group = 1)) + 
  geom_line() +
  geom_point() +
  # Y axis scale
  ylim(0.75, 1) +
  # Format labels
  ggtitle("Model performance") + 
  ylab("") +
  theme(legend.position="none" ,
        axis.text.x = element_text(angle = 90)) +
  # Arrange in grid
  facet_wrap(~Metric)
results
##3.10 Predicting Quality:##
set.seed(1234, sample.kind = "Rounding")
# Formula used in predictions
pml_qual <- as.formula(paste("quality2", "~", 
                             paste(xcol, collapse=' + ')))

# Define models
#"glm",gamLoess, qda, adaboost
models <- c( "lda", "naive_bayes", "svmLinear", "rpart",
             "knn", "multinom", "rf")

# Create 'results' table. The first row
# contains NAs and will be removed after
# the training
q_results <- tibble(Model = NA,
                    Quality = NA,
                    Accuracy = NA,
                    Sensitivity = NA,
                    Specificity = NA,
                    F1_Score = NA)

preds_q <- sapply(models, function(model){ 
  
  print(model)
  if (model == "knn") {
    # knn use custom tuning parameters
    grid <- data.frame(k = seq(3, 50, 2))
    ft <- caret::train(form = pml_qual, 
                       method = model, 
                       data = train_set_r, 
                       trControl = contrl,
                       tuneGrid = grid)
  } else if (model == "rforest") {
    # Random forest use custom tuning parameters
    grid <- data.frame(mtry = c(1, 2, 3, 4, 5, 10, 25, 50, 100))
    
    ft <- caret::train(form = pml_qual,
                       method = "rforest", 
                       data = train_set_r,
                       trControl = contrl,
                       ntree = 150,
                       tuneGrid = grid,
                       nSamp = 5000)
  } else {
    # Other models use standard parameters (no tuning)
    ft <- caret::train(form = pml_qual, 
                       method = model, 
                       data = train_set_r, 
                       trControl = contrl)
  }
  
  # Predictions
  pred <- predict(object = ft, newdata = test_set_r)
  
  # Accuracy
  acc <- mean(pred == test_set_r$quality2)
  
  # Sensitivity
  sen <- caret::confusionMatrix(pred,
                                test_set_r$quality2)$byClass[,"Sensitivity"]
  # Specificity
  spe <- caret::confusionMatrix(pred,
                                test_set_r$quality2)$byClass[,"Specificity"]
  
  # F1 score
  f1 <- caret::confusionMatrix(pred,
                               test_set_r$quality2)$byClass[,"F1"]
  
  # Store stats in 'results' table
  q_results <<- rbind(q_results,
                      tibble(Model = model, 
                             Quality = levels(test_set_r$quality2),
                             Accuracy = acc,
                             Sensitivity = sen,
                             Specificity = spe,
                             F1_Score = f1))
  
  # The predictions will be used for ensemble
  return(pred)
}) 

# Remove the first row of 'results' that contains NAs
q_results <- q_results[2:(nrow(q_results)),]

#-------------------------------
# Combine all models
#-------------------------------
# Use votes method to ensemble the predictions
votes <- data.frame(low    = rowSums(preds_q =="low"),
                    medium = rowSums(preds_q =="medium"),
                    high   = rowSums(preds_q =="high"))

p_hat <- factor(sapply(1:nrow(votes), function(x)
  colnames(votes[which.max(votes[x,])])))

p_hat <- relevel(p_hat, "medium")

# Accuracy
acc <- caret::confusionMatrix(p_hat,
                              test_set_r$quality2)$overall["Accuracy"]

# Sensitivity
sen <- caret::confusionMatrix(p_hat,
                              test_set_r$quality2)$byClass[,"Sensitivity"]
# Specificity
spe <- caret::confusionMatrix(p_hat,
                              test_set_r$quality2)$byClass[,"Specificity"]

# F1 score
f1 <- caret::confusionMatrix(p_hat,
                             test_set_r$quality2)$byClass[,"F1"]


q_results <<- rbind(q_results,
                    tibble(Model = "Ensemble",
                           Quality = levels(test_set_r$quality2),
                           Accuracy = acc,
                           Sensitivity = sen,
                           Specificity = spe,
                           F1_Score = f1))
#-------------------------------
# Plot the 'results' table
#-------------------------------
# Make a plot for each metric with the 3 quality levels
# The plots are grouped using the chunk options.

# Metric names
metrics <- names(q_results)[3:6]

res <- sapply(metrics, function(var){
  
  # Plot stored in 'p'
  z <- q_results %>% 
    # Convert columns to lines
    pivot_longer(cols = 3:6, names_to = "Metric", 
                 values_drop_na = TRUE) %>%
    
    pivot_wider(names_from = Quality) %>%
    filter(Metric == var) %>%
    
    ggplot(aes(x = Model, group = 1)) + 
    # Draw lines
    geom_line(aes(y = low,     col = "low")) +
    geom_line(aes(y = medium,  col = "medium")) +
    geom_line(aes(y = high,    col = "high")) +
    # Draw points
    geom_point(aes(y = low,    col = "low")) +
    geom_point(aes(y = medium, col = "medium")) +
    geom_point(aes(y = high,   col = "high")) +
    # Format labels
    ggtitle(var) + 
    ylab("Model") +
    theme(axis.text.x = element_text(angle = 90))
  
  # Show the plot
  print(z)
})
#-------------------------------
# Stop parallel processing used in 'train'
#-------------------------------
stopCluster(cl)
##3.11 Clustering:##
#-------------------------------
# k-means
#-------------------------------
# Reference:
# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

# Load 'factoextra' to visualize clusters
load_lib("factoextra")

# Determine and visualize the optimal number of clusters 
# using total within sum of square (method = "wss")
train_set %>% filter(type == "red") %>% .[,xcol] %>%
  fviz_nbclust(x = ., FUNcluster = kmeans, method = "wss") + 
  geom_vline(xintercept = 4, linetype =2) +
  scale_y_continuous(labels = comma)
# We use 25 random starts for the clusters
k <- train_set %>% filter(type == "red") %>% .[,xcol] %>%
  kmeans(x = ., centers = 4, nstart = 25)

# Calculate cluster means
clus_m <- as_hux(data.frame(t(k$centers)), add_rownames = TRUE)
colnames(clus_m) <- c("Feature", paste("Cluster", 1:4))
clus_m <- add_colnames(clus_m)  
clus_m %>%
  # Format header row
  set_bold(row = 1, col = everywhere, value = TRUE)        %>%
  set_top_border(row = 1, col = everywhere, value = 1)     %>%
  set_bottom_border(row = c(1,12), col = everywhere, value = 1)  %>%
  # Format cells
  set_align(row = everywhere, col = 1,   value = 'left')   %>%
  set_align(row = everywhere, col = 2:5, value = 'right')  %>%
  set_number_format(row = 2:nrow(clus_m), 
                    col = everywhere, value = 3)           %>% 
  # Format table
  set_width(value = 0.7)                                   %>%
  set_position(value = "center")                           %>%
  set_caption("Features of Cluster Center")
# Plot the cluster
train_set %>% filter(type == "red") %>% .[,xcol] %>%
  fviz_cluster(object = k, 
               choose.vars = c("chlorides", "total_sulfur_dioxide"),
               geom   = "point", 
               repel  = TRUE, 
               main   = "Cluster plot with selected features",
               xlab   = "Chlorides",
               ylab   = "Total Sulfur Dioxide")
###IV. Conclusion:###
###V. Recommendation:###

###References###
results <- results[2:(nrow(results)),]