
# Loading the needed libraries
library(tidyverse)
library(readxl)
library(broom)
library(car)


# import file path
# file.choose()
data = utils::read.csv("/Users/nikosmatsavelas/Desktop/spitogatos assignment/assignment_rev2.csv")
df  = dplyr::tibble(data);df


# Cleaning the dataset 
# selecting the needed columns 
df_clean=df%>%select(id,ranking_score,agent_id,geography_name,sq_meters,
         price,year_of_construction,subtype,ad_type)%>%
  # filter out from the construction year,the zero year and years that are below 1900
  dplyr::filter(year_of_construction != 2155 & year_of_construction !=0 & year_of_construction > 1900)%>%
  # filter out the values of ranking score that have more than 5 digits
  dplyr::filter(nchar(ranking_score)<=5)%>%
  # convert back the ranking_score column as double and 
  # convert as double the price columns from integer
  dplyr::mutate(ranking_score = as.double(ranking_score),price = as.double(price))%>%
  # remove duplicated listings from different id 
  dplyr::distinct(agent_id,geography_name,sq_meters,year_of_construction,subtype,ad_type,.keep_all = TRUE)
df_clean


# ----- Question 1 
#
#
#
# Calculate metrics for property prices per house type 
metrics_by_subtype = df_clean %>%
  dplyr::group_by(subtype)%>%
  dplyr::summarise(mean_price = mean(price),
            median_price = median(price),
            std_price = sd(price))

# per area 
metrics_by_area = df_clean%>%
  dplyr::group_by(geography_name)%>%
  dplyr::summarise(mean_price = mean(price),
            median_price = median(price),
            std_price = sd(price))

# per house type per geography area
metrics_by_sub_type_area = df_clean%>%
  dplyr::group_by(subtype,geography_name)%>%
  dplyr::summarise(mean_price = mean(price),
            median_price = median(price),
            std_price = sd(price))


# Print the results
print(metrics_by_subtype)
print(metrics_by_area)
print(metrics_by_sub_type_area)






# Alternative approach (linear regression)
#
# Linear regression with y dependent variable the price and 
#  explanatory x variable the categorical variable sybtype without constant
#  This approach will reproduce the same results as the quick way. 

# run the regression for house type 
lm_type = lm(df_clean$price~df_clean$subtype-1)
# show the coefficients of regression (mean price per house type)
tidy(lm_type)
# check if both approaches give the same results
round(lm_type$coefficients,4) == round(metrics_by_subtype$mean_price,4)



# run the regression for per house type per geography area
lm_area = lm(df_clean$price~df_clean$geography_name-1)
# show the coefficients of regression (mean price per house type)
tidy(lm_area)
# check if both approaches give the same results
round(lm_area$coefficients,4) == round(metrics_by_area$mean_price,4)


# run the regression for per geography area
lm_both = lm(df_clean$price~(df_clean$geography_name:df_clean$subtype)+0,data=df_clean)
# show the coefficients of regression (mean price per house type)
tidy(lm_both)
# check if both approaches give the same results
reg = na.omit(as.numeric(round(lm_both$coefficients,1)));reg
dat = as.numeric(round(metrics_by_sub_type_area$mean_price,1));dat
d3 = tibble(reg,dat);d3
reg==dat





#   ----    Question 2
#
#
#
#    Below we suggest a few possible metrics that could be used to measure the competitiveness of an area:
#  
# 1)  Market Share: The market share of a specific area could be calculated by determining 
#     the percentage of listings in that area 
#     compared to the total number of listings across all areas. 
#     A higher market share for an area would indicate a higher level of competitiveness.
#
# 2)  Average Ranking Score: The average ranking score of listings in a specific area could be calculated. 
#     A higher average ranking score would indicate a higher level of competitiveness 
#     as it would mean that listings  in that area are of higher quality.
#
# 3) Price per sq_meters: The average price per sq_meters of houses in a specific area can be considered.
#    A higher average price per sq_meters would indicate a higher level of competitiveness
#    as it would mean that properties in that area are more expensive.
#



Metrics = df_clean%>%
  dplyr::group_by(geography_name,ad_type)%>%

  # 1) Calculate the market share of each area
  dplyr::summarize(market_share = n() / nrow(df_clean),
  # 2) Calculate the average ranking score of each area
            avg_ranking_score = mean(ranking_score),
  # 3) Calculate the average price per sq_meters of each area
            price_per_sqm = mean(price/sq_meters))%>%
  # we create a new variable ad_type (the numeric levels of the original ad_type variable)
  dplyr::mutate(ad_type2 = base::ifelse(ad_type=="star",4,
                                        base::ifelse(ad_type=="premium",3,
                                                     base::ifelse(ad_type=="up",2,1))))%>%
  # we arrange the whole data frame in a descending order according to ad_type2
  dplyr::arrange(desc(ad_type2))%>%
  # we drop the created variable
  dplyr::select(-ad_type2)
  
Metrics
# Plot the metric  market share 
ggplot2::ggplot(Metrics, aes(x = geography_name, y = market_share, fill = ad_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mean ranking score by area and subtype") +
  labs(x = "Area", y = "Mean ranking score") + 
  theme(legend.title = element_blank())

# Plot the metric average ranking score of each area
ggplot2::ggplot(Metrics, aes(x = geography_name, y = avg_ranking_score, fill = ad_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mean ranking score by area and subtype") +
  labs(x = "Area", y = "Mean ranking score") + 
  theme(legend.title = element_blank())

# Plot the metric average price per sq_meters of each area
ggplot2::ggplot(Metrics, aes(x = geography_name, y = price_per_sqm, fill = ad_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mean ranking score by area and subtype") +
  labs(x = "Area", y = "Mean ranking score") + 
  theme(legend.title = element_blank())



#  Alternative 1
#
#  We can use the mean,standard deviation of ranking score per area per house type
#
#  Calculate competitiveness metrics
metrics_comp = df_clean%>%
  dplyr::group_by(geography_name, ad_type) %>% 
  dplyr::summarize(mean_score = mean(ranking_score),sd_score=sd(ranking_score),number_of_listings = n())%>%
  dplyr::mutate(ad_type2 = ifelse(ad_type=="star",4,
                           ifelse(ad_type=="premium",3,
                                  ifelse(ad_type=="up",2,1))))%>%
  dplyr::arrange(desc(ad_type2),mean_score)%>%
  dplyr::select(-ad_type2)
metrics_comp
# Plot the metric mean of ranking score per area per house type
ggplot2::ggplot(metrics_comp, aes(x = geography_name, y = mean_score, fill = ad_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mean ranking score by area and subtype") +
  labs(x = "Area", y = "Mean ranking score") + 
  theme(legend.title = element_blank())
# Plot the metric standard deviation of ranking score per area per house type
ggplot2::ggplot(metrics_comp, aes(x = geography_name, y = sd_score, fill = ad_type)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mean ranking score by area and subtype") +
  labs(x = "Area", y = "Mean ranking score") + 
  theme(legend.title = element_blank())





#  Alternatively we suggest (not quick way) : 
#
#  1) Number of unique keywords per area: This metric would show the number of unique keywords
#    used in the listings of a specific area. A higher number of unique keywords would indicate a higher level 
#    of competition in that area, as it would be harder for a listing to rank for all the keywords used in that area.
#    In this context, keywords refer to the attributes of a property that agents would input in order to get a
#    property's price valuation.
#
# 2) The year of construction can be a competitiveness metric as it can indicate how new or old a property
#    is compared to others in the same area. A newer property may have a better chance of ranking higher 
#    in search results than an older property in the same area. 
#    However, it's important to note that this metric alone may not be sufficient in determining
#    the competitiveness of an area and should be considered in conjunction with other metrics.
#
#   (Require historical data)   
# 3) Number of new listings per area: This metric would show the number of new listings added 
#    to a specific area over a certain period of time. 
#    A higher number of new listings would indicate a higher level of competition in that area, 
#    as it would be harder for new listings to rank among a higher number of established listings.






#   ----    Question 3
#
#
#
#
#    The clean data as we used in question 1 and 2 are now merged (left_join) with the full data set 
#    with column criteria the names of the clean data frame
df_clean2 = left_join(df_clean,data,by=c("id","ranking_score","agent_id","geography_name","sq_meters",
                                         "price","year_of_construction","subtype","ad_type"))
df_clean2


#  more additional changes in order to create a final data frame to work with 
df_final = df_clean2%>%
  # we change the class of the all vectors from as.interger to as.double
  dplyr::mutate_if(is.integer, as.double)%>%
  # we select only the columns that are double (numeric) and the geography name
  dplyr::select(where(is.double),geography_name)%>%
  # we change the column variable revonation year to 1 and 0:
  # If a listing has a renovation year then is equal to 1
  # otherwise 0. 
  dplyr::mutate(renovation = ifelse(!is.na(renovation_year),1,0))%>%
  # we drop the columns id,(original) renovation_year, agent_id and ranking score 
  # because they are irrelevant to the pricing that the agent will 
  # use in order to valuate a property.
  dplyr::select(-c(id,renovation_year,agent_id,ranking_score))
df_final

#  we run a linear regression model in order to identify collinearity. 
# (collinearity is the perfect (deterministic) linear relationship between 
# one explanatory variable with (some of) the rest of the explanatories)
# When 2 covariates are highly related => they carry similar information.
# (since when we know the value of the one we can precisely predict the value of the other)
# Therefore, such variables are not adding any further information about the effect on Y (price of a house)
# when we add them sequentially.

# we run the full model with all variables 
model = lm(price ~.,data= df_final)


#  the statistical measure of collinearity is the variance inflation factor (vif). 
#  If an explanatory variable has vif >10 then this variable must be droped 
#  because contains no further information.
car::vif(model)
# all the variables have less <10 vif
summary(model)
# from the summary of the model the variable model balcony_area is 
# statistical not significant and can be droped. 

df_final2=df_final%>%select(-balcony_area)
df_final2

# we run the final model without constant (has no logical sense in a multivariate regression
# such this SpAn data)
# we run the linear regression model without the constant b0.
model2 = lm(price~.-1,data=df_final2)
summary(model2)
betas = round(as.numeric(coef(model2)),2);betas
variable = names(coef(model2))
dat = tibble(betas,variable);dat






# --   Alternatives (not quick way) 
#
#
# 1) We suggest Principal Component Analysis (PCA) which is a dimensionality reduction technique 
#    that can be used to identify the most important attributes or features in the housing market. 
#    It works by transforming the original dataset into a new set of uncorrelated variables, 
#    called principal components, which can explain the most variance in the original data.
#    After this reduction we can run a linear model based on the loadings of each variable on each 
#    principal component.   

# 2) Additionally we suggest the Lasso Regression: 
#     With this method we can identify the most important attributes
#     by applying a L1 penalty term to linear regression which shrinks the coefficients
#     of less important features to zero. However, it is important to note that categorical 
#     variables need to be encoded as numeric values before they can be used in the model. 
#     Common encoding techniques can be used such as dummy coding as we did in the proposed lm model.

