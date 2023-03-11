library(tidyverse)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(leaflet)
library(kableExtra)
library(RColorBrewer)
library(plotly)
library(lubridate)
th <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text()) # global theme for ggplot2 objects
set.seed(500)
Airbnb<- read.csv("C:\\Users\\HP\\Downloads\\new_nyc_data.csv",encoding="UTF-8", stringsAsFactors = F, na.strings = c(""))

#head of the data
head(Airbnb)
#summary of the data
summary(Airbnb)

#delecting id and host_id not usefull.
names_to_delete <- c("id", "host_id")
Airbnb[names_to_delete] <- NULL

#convert character columns to factor columns 
names_to_factor <- c("host_name", "neighbourhood_group", "neighbourhood", "room_type")
Airbnb[names_to_factor] <- map(Airbnb[names_to_factor], as.factor)

#using function ymd from lubridate package
Airbnb[c("last_review")] <- Airbnb[c("last_review")] %>% map(~lubridate::ymd(.x))
#just checking everything is ok or not
glimpse(Airbnb)

#Missing Data
missing_airbnb <- Airbnb %>% summarise_all(~(sum(is.na(.))/n()))
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")
missing_airbnb <- missing_airbnb[missing_airbnb$percent_missing > 0.0, ] 
ggplot(missing_airbnb, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "pink", aes(color = I('green')), size = 0.3)+
  xlab('variables')+
  coord_flip() + 
  th  +
  ggtitle("Missing Data") +
  xlab("Oberservation") +
  ylab("Percentage missing") +
  annotate("text", x = 1.5, y = 0.1,label = "\n\n \t\t\t\t\t\t\t\t\t\t\t\t       host_name and name have less than 0.001 \n \t\t\t percentage missing", color = "orange", size = 3)


#histogram
ggplot(Airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "yellow") + 
  geom_density(alpha = 0.2, fill = "blue") +
  th +
  ggtitle("Distribution of price",
          subtitle = "The distribution is highly skewed") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(Airbnb$price), 2), size = 2, linetype = 3)

#hist
ggplot(Airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "red") + 
  geom_density(alpha = 0.2, fill = "red") +
  th +
  ggtitle("Transformed distribution of price",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(Airbnb$price), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Mean price = ", paste0(round(mean(Airbnb$price), 2), "$")),
           color =  "#32CD32", size = 8)



#hist3
airbnb_nh <- Airbnb %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price), 2))


ggplot(Airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  th +
  ggtitle("Transformed distribution of price\n by neighbourhood groups",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group) +
  scale_x_log10() 

#above the avg
Airbnb %>% filter(price >= mean(price)) %>% group_by(neighbourhood_group, room_type) %>% tally %>% 
  ggplot(aes(reorder(neighbourhood_group,desc(n)), n, fill = room_type)) +
  th +
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of above average price objects",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity")


#boxplot for price & room type

ggplot(Airbnb, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  th + 
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(Airbnb$price), color = "purple", linetype = 2)
#summary of price distribution

Airbnb %>% arrange(desc(price)) %>% top_n(10) %>% select(- host_name, -name) %>%  
  ggplot(aes(x = price, fill = neighbourhood_group)) +
  geom_histogram(bins = 50) +
  scale_x_log10() + 
  ggtitle("Summary of price distributions") +
  facet_wrap(~room_type + neighbourhood_group)

#price & availability

ggplot(Airbnb, aes(availability_365, price)) +
  th +
  geom_point(alpha = 0.2, color = "slateblue") +
  geom_density(stat = "identity", alpha = 0.2) +
  xlab("Availability during year") +
  ylab("Price") +
  ggtitle("Relationship between availability",
          subtitle = "there is not clear relationship")

#Price & Number Of Reviews
ggplot(Airbnb, aes(number_of_reviews, price)) +
  th + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(aes(size = price), alpha = 0.05, color = "orange") +
  xlab("Number of reviews") +
  ylab("Price") +
  ggtitle("Relationship between number of reviews",
          subtitle = "The most expensive objects have small number of reviews (or 0)")


#Number of objects by neighbourhood areas
Airbnb %>% group_by(neighbourhood_group) %>% tally() %>% 
  ggplot(aes(x = reorder(neighbourhood_group, n), n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_fivethirtyeight() +
  ggtitle("Number of objects by neighbourhood group") +
  geom_text(aes(x = neighbourhood_group, y = 1, label = paste0(n),
                colour = ifelse(neighbourhood_group %in%
                                  c("Manhattan", "Brooklyn", 
                                    "Queens"), '1', '2')),
            hjust=-1.5, vjust=.5, size = 4, 
            fontface = 'bold') +
  coord_flip() +
  scale_color_manual(values=c("white","black"), guide = F)

#Leaflet map
pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = Airbnb$neighbourhood_group)

leaflet(data = Airbnb) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,
                                                                                                        label = paste("Name:", Airbnb$name)) %>% 
  addLegend("bottomright", pal = pal, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )


#Correlation Matrix
airbnb_cor <- Airbnb[, sapply(Airbnb, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")

#Data Splitting
Airbnb <- Airbnb %>% mutate(id = row_number())
airbnb_train <- Airbnb %>% sample_frac(.7) %>% filter(price > 0)
airbnb_test  <- anti_join(Airbnb, airbnb_train, by = 'id') %>% filter(price > 0)

# sanity check
nrow(airbnb_train) + nrow(airbnb_test) == nrow(Airbnb %>% filter(price > 0))


#1st Linear Regression model
first_model <- train(price ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, data = airbnb_train, method = "lm")
summary(first_model)


#plot

plot(first_model$finalModel)

#2nd Linear Regression Model
learn <- airbnb_train %>% filter(price < quantile(airbnb_train$price, 0.9) & price > quantile(airbnb_train$price, 0.1)) %>% tidyr::drop_na()
second_model <- lm(log(price) ~ room_type + neighbourhood_group + latitude + longitude 
                   + number_of_reviews + availability_365
                   + reviews_per_month + 
                     calculated_host_listings_count + minimum_nights, data = learn)
# Summarize the results
summary(second_model)


#plot 2nd model
plot(second_model)


#Predict prices for training set
airbnb_test <- airbnb_test %>% filter(price <= quantile(airbnb_train$price, 0.9) & price >= quantile(airbnb_train$price, 0.1)) %>% tidyr::drop_na()
pred_regression <- predict(second_model, newdata = airbnb_test)
pred_regression <- exp(pred_regression)

RMSE_regression <- sqrt(mean( (airbnb_test$price - pred_regression)**2 ))
RMSE_regression

SSE <- sum((airbnb_test$price - pred_regression)**2)
SSR <- sum((pred_regression - mean(airbnb_test$price)) ** 2)
R2 <- 1 - SSE/(SSE + SSR)
R2

regression_results <- tibble(
  obs = airbnb_test$price,
  pred = pred_regression,
  diff = pred - obs,
  abs_diff = abs(pred - obs),
  neighbourhood = airbnb_test$neighbourhood,
  name = airbnb_test$name,
  group = airbnb_test$neighbourhood_group,
  type = airbnb_test$room_type
  
)

regression_plot <- regression_results %>% 
  ggplot(aes(obs, pred)) +
  geom_point(alpha = 0.1, aes(text = paste("Name:", name, "\nGroup:", group, "\nType:", type,
                                           "\nPrice diff = ", diff))) +
  th +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Observed vs predicted",
          subtitle = "Linear regression model") + 
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = 2)  +
  facet_wrap(~type)

#plot
ggplotly(regression_plot)

#End of R code.

