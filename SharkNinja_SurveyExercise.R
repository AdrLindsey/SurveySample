#### Setting the stage ####
rm(list=ls(all=TRUE))

## INSTALL AND LOAD MISSING PACKAGES 
List_Of_Packages <- c("ggplot2","dplyr","tidyr","scales","lubridate","lavaan")

New_Packages <- List_Of_Packages[!(List_Of_Packages %in% installed.packages()[,"Package"])]
if(length(New_Packages)) install.packages(New_Packages)

invisible(lapply(List_Of_Packages, library, character.only = TRUE))

# LOAD AND CLEAN DATA
my_data <- read.csv("~/Desktop/ExerciseFiles/SharkNinja Sample Data (V2).csv", stringsAsFactors = FALSE) %>%
  # exclude responses with missing answers and customers who don't own a product (Q3 != 0)
  filter(complete.cases(.), Q3 != "0") 

# format date and time variables
my_data <- my_data %>% 
  mutate(Date = as.Date(Survey.Date, "%m/%d/%y")) %>% 
  select(-Time, -Survey.Date) # exclude Time

# scale (Q1-4,8), bool (Q5,6), and factor (Q7) responses
my_data <- my_data %>%
  mutate(adjQ1 = if_else(Q1 < 4, 0, if_else(Q1 < 9, 1, 2)), # assume that 9,10 are recommends
         targetQ1 = if_else(Q1 < 9, 0, 1), # assume that 9,10 are recommends
         adjQ4 = if_else(Q4 < 4, 0, 1), # assume that only 4 is future purchase
         Q5 = as.numeric(as.character(Q5)), Q5 = if_else(Q5 == 2, 0, Q5),
         Q6 = as.numeric(as.character(Q6)), Q6 = if_else(Q5 == 2, 0, Q5),
         Q7 = if_else(Q7 == "1", "Direct from SharkNinja", "Retail"))
        
# summarize data
my_data %>% str()
my_data %>% head()
my_data %>% summary()
my_data %>% dim()

# subset train and test datasets (80/20 split)
smp_size <- floor(0.80 * nrow(my_data))
my_data_trimmed <- my_data %>% select(-Date)
set.seed(123)
train_dat <- sample(seq_len(nrow(my_data_trimmed)), size = smp_size)
train <- my_data_trimmed[train_dat, ]
test <- my_data_trimmed[-train_dat, ]

#### Exploratory Data Analysis ####
prop.table(table(train$Survey.DID))

ggplot(data = train, aes(x = Q3, fill = factor(Q4))) + 
  geom_bar(width = 0.5) +
  xlab("Service") + 
  ylab("Total Count") + 
  labs(fill = "Recommend?") +
  facet_grid(Q7~Survey.DID)

# Examine respondents who had negative experiences



#### Quick Deep Dives ####
# correlation of terms
table(train$Q4,train$Q3)
cor(train$Q4,train$Q3)
plot(train$Q4,train$Q3, main = "scatter plot")


lm_prod_purch <- lm(train$Q4 ~ train$Q3)
summary(lm_prod_purch)
plot(lm_prod_purch)
attributes(lm_prod_purch)
abline(lm_prod_purch)


table(train$Q4,train$Q2)
cor(train$Q1,train$Q2)
plot(train$Q4,train$Q2)

lm_serv_rec<- lm(train$Q1 ~ train$Q2)
summary(lm_serv_rec)
plot(lm_serv_rec)

mytable1 <- table(train$Q2,train$Q3)
mytable1 %>% prop.table() %>% round(., 3)

ggplot(data = train, aes(x = Q2, y = Q3, fill = Q4)) +
  geom_tile() +
  xlab("Service Rating") + 
  ylab("Product Rating") +
  labs(fill = "Future Purchase") +
  ggtitle("What is the Customer Propensity to Make Future Purchase?")+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_grid(.~Q7)
 
ggplot(data = train, aes(x = Q2, y = Q3, fill = Q1)) +
  geom_tile() +
  xlab("Service Rating") + 
  ylab("Product Rating") +
  labs(fill = "Recommend SharkNinja") +
  ggtitle("What is the Customer Propensity to Recommend SharkNinja?") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(.~Q7)

# plot in response to tree
train$Q8_split <- if_else(train$Q8 < 6, "CES < 6", "CES >= 6")
ggplot(data = train %>%
         # window view
         filter(Q3 %in% c(2,3)), 
       aes(x =  Q3, fill = factor(adjQ1))) +
  geom_bar() +
  xlab("Product Rating") + 
  ylab("Count") +
  labs(fill = "Recommend SharkNinja") +
  ggtitle("How does Custome Effort impact Propensity to Recommend \nSharkNinja for lower Product Ratings?")+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_grid(.~Q8_split)

# Classification Model
library(party)
tree <- ctree(as.factor(adjQ1) ~ Q2 + Q3 + Q5 + Q6  + Q8, data = train,
              controls = ctree_control(mincriterion = 0.99, minsplit = 500))
plot(tree)
predict(tree, test, type = "prob")

library(rpart)
library(rpart.plot)
tree1 <- rpart(as.factor(adjQ1) ~ Q2 + Q3 + Q5 + Q6  + Q8, data = train,
               control = rpart.control(0.01))
rpart.plot(tree1)
predict(tree1,test)
#### Consider Latient Variables
# PCA
train_pca <- train %>% select(Q1, Q2, Q5, Q6, Q8)
train.pca1 <- princomp(train_pca)
summary(train.pca1)
plot(train.pca1, main = "Feature Reduction") # -- There appear to be 2 latent factors in this data -- #


# SEM via Lavaan
model <- "
  latExperience =~ Q2 + Q3 + Q5
  latRecommend =~ Q1 + Q4
  latExperience ~ latRecommend
"

fit <- cfa(model, data=train)
summary(fit, standardized=TRUE)  



