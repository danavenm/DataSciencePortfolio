# DataSciencePortfolio
Final Project

Read Data
Read the the youtube statistics data

data <- read.csv("youtube-stats.csv")
head(data)
##   rank                   Youtuber subscribers  video.views         category
## 1    1                   T-Series   245000000 228000000000            Music
## 2    2             YouTube Movies   170000000            0 Film & Animation
## 3    3                    MrBeast   166000000  28368841870    Entertainment
## 4    4 Cocomelon - Nursery Rhymes   162000000 164000000000        Education
## 5    5                  SET India   159000000 148000000000            Shows
## 6    6                      Music   119000000            0              nan
##                        Title uploads       Country Abbreviation  channel_type
## 1                   T-Series   20082         India           IN         Music
## 2              youtubemovies       1 United States           US         Games
## 3                    MrBeast     741 United States           US Entertainment
## 4 Cocomelon - Nursery Rhymes     966 United States           US     Education
## 5                  SET India  116536         India           IN Entertainment
## 6                      Music       0           nan          nan         Music
##   video_views_rank country_rank channel_type_rank
## 1                1            1                 1
## 2          4055159         7670              7423
## 3               48            1                 1
## 4                2            2                 1
## 5                3            2                 2
## 6          4057944          NaN               NaN
##   video_views_for_the_last_30_days lowest_monthly_earnings
## 1                        2.258e+09                  564600
## 2                        1.200e+01                       0
## 3                        1.348e+09                  337000
## 4                        1.975e+09                  493800
## 5                        1.824e+09                  455900
## 6                              NaN                       0
##   highest_monthly_earnings lowest_yearly_earnings highest_yearly_earnings
## 1                  9.0e+06                6.8e+06               1.084e+08
## 2                  5.0e-02                4.0e-02               5.800e-01
## 3                  5.4e+06                4.0e+06               6.470e+07
## 4                  7.9e+06                5.9e+06               9.480e+07
## 5                  7.3e+06                5.5e+06               8.750e+07
## 6                  0.0e+00                0.0e+00               0.000e+00
##   subscribers_for_last_30_days created_year created_month created_date
## 1                        2e+06         2006           Mar           13
## 2                          NaN         2006           Mar            5
## 3                        8e+06         2012           Feb           20
## 4                        1e+06         2006           Sep            1
## 5                        1e+06         2006           Sep           20
## 6                          NaN         2013           Sep           24
##   Gross.tertiary.education.enrollment.... Population Unemployment.rate
## 1                                    28.1 1366417754              5.36
## 2                                    88.2  328239523             14.70
## 3                                    88.2  328239523             14.70
## 4                                    88.2  328239523             14.70
## 5                                    28.1 1366417754              5.36
## 6                                     NaN        NaN               NaN
##   Urban_population Latitude Longitude
## 1        471031528 20.59368  78.96288
## 2        270663028 37.09024 -95.71289
## 3        270663028 37.09024 -95.71289
## 4        270663028 37.09024 -95.71289
## 5        471031528 20.59368  78.96288
## 6              NaN      NaN       NaN
Preprocess data
Replace special characters in the names

data <- data %>%
      mutate_if(is.character, 
                function(col) iconv(col, to="ASCII//TRANSLIT"))
We’ll see if we have any null data

sum(is.na(data))
## [1] 1494
We have null values, we can remove all the nans from the dataset

data <- na.omit(data)
dim(data)
## [1] 522  28
We still have null values in category table but as string

length(data[data$category== 'nan',])
## [1] 28
Change these to General

data$category[data$category == 'nan'] <- 'General'
Data Visualization
Now that we have clean data, lets try to visualize the data

Bar graph of average revenue in each country
# Group and mean
countryHighestRevenueAvg <- data %>% 
  group_by(Country) %>% 
  summarise(highest_yearly_earnings = mean(highest_yearly_earnings))

ggplot(data=countryHighestRevenueAvg, aes(x=Country, y=highest_yearly_earnings, fill=Country)) +
  geom_bar(stat="identity") + coord_flip()


Bar graph of average subscribers in each country
countrySubscribersAvg <- data %>% 
  group_by(Country) %>% 
  summarise(subscribers_for_last_30_days = mean(subscribers_for_last_30_days))

ggplot(data=countrySubscribersAvg, aes(x=Country, y=subscribers_for_last_30_days, fill=Country)) +
  geom_bar(stat="identity") + coord_flip()


Top categories based on view count
categoryTotalViews <- data %>% 
  group_by(category) %>% 
  summarise(video.views = sum(video.views))

ggplot(data=categoryTotalViews, aes(x=category, y=video.views, fill=category)) +
  geom_bar(stat="identity") + coord_flip()


Top 10 Highest revenue youtube channels
dataOrdered <- data[order(data$highest_yearly_earnings, decreasing=TRUE),]
dataOrdered <- head(dataOrdered,10)

ggplot(data=dataOrdered, aes(x=highest_yearly_earnings, y=Youtuber, fill=Youtuber)) +
  geom_bar(stat="identity") 


Scatter plot of views vs subscribers in the last 30 days
ggplot(data, aes(x=video_views_for_the_last_30_days, y=subscribers_for_last_30_days)) + geom_point()


Prediction
Select only numeric attribute

subdata <- select_if(data, is.numeric)
Apply the model:

The columns lowest_monthly_earnings, lowest_yearly_earnings, highest_monthly_earnings

regressionModel <- train(
  highest_yearly_earnings~ .,     data = subdata, 
  method = "lm")
summary(regressionModel$finalModel)
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60997 -21793    432  20617  64652 
## 
## Coefficients:
##                                           Estimate Std. Error t value Pr(>|t|)
## (Intercept)                              5.447e+05  5.844e+05   0.932    0.352
## rank                                     2.865e+00  5.718e+00   0.501    0.617
## subscribers                             -8.466e-05  1.645e-04  -0.515    0.607
## video.views                              9.976e-08  1.897e-07   0.526    0.599
## uploads                                 -2.103e-02  3.156e-02  -0.666    0.505
## video_views_rank                         1.216e-03  5.187e-03   0.234    0.815
## country_rank                            -7.912e-02  3.301e+00  -0.024    0.981
## channel_type_rank                       -7.303e-01  3.930e+00  -0.186    0.853
## video_views_for_the_last_30_days        -6.860e-07  3.523e-06  -0.195    0.846
## lowest_monthly_earnings                  1.912e+02  2.116e+00  90.383   <2e-16
## highest_monthly_earnings                 2.502e-02  9.229e-02   0.271    0.786
## lowest_yearly_earnings                   3.006e-02  1.061e-01   0.283    0.777
## subscribers_for_last_30_days             2.910e-03  3.175e-03   0.917    0.360
## created_year                            -2.724e+02  2.903e+02  -0.939    0.348
## created_date                            -4.998e+01  1.420e+02  -0.352    0.725
## Gross.tertiary.education.enrollment.... -3.066e+00  1.043e+02  -0.029    0.977
## Population                               1.283e-05  1.588e-05   0.808    0.419
## Unemployment.rate                        1.109e+03  7.215e+02   1.538    0.125
## Urban_population                        -3.931e-05  4.685e-05  -0.839    0.402
## Latitude                                -1.202e+02  7.307e+01  -1.645    0.101
## Longitude                                1.745e+01  3.021e+01   0.578    0.564
##                                            
## (Intercept)                                
## rank                                       
## subscribers                                
## video.views                                
## uploads                                    
## video_views_rank                           
## country_rank                               
## channel_type_rank                          
## video_views_for_the_last_30_days           
## lowest_monthly_earnings                 ***
## highest_monthly_earnings                   
## lowest_yearly_earnings                     
## subscribers_for_last_30_days               
## created_year                               
## created_date                               
## Gross.tertiary.education.enrollment....    
## Population                                 
## Unemployment.rate                          
## Urban_population                           
## Latitude                                   
## Longitude                                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28280 on 501 degrees of freedom
## Multiple R-squared:      1,  Adjusted R-squared:      1 
## F-statistic: 7.637e+06 on 20 and 501 DF,  p-value: < 2.2e-16
Our model is over fitted, lets remove columns lowest_monthly_earnings, lowest_yearly_earnings, highest_monthly_earnings which have high corrrelation

regressionModel <- train(
  highest_yearly_earnings~ . - lowest_monthly_earnings - lowest_yearly_earnings - highest_monthly_earnings,     data = subdata, 
  method = "lm")
summary(regressionModel$finalModel)
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -53127416  -2443963    -87511   2120441  55286505 
## 
## Coefficients:
##                                           Estimate Std. Error t value Pr(>|t|)
## (Intercept)                             -2.386e+08  1.537e+08  -1.553  0.12117
## rank                                    -4.089e+03  1.493e+03  -2.739  0.00638
## subscribers                             -3.308e-01  4.070e-02  -8.129 3.38e-15
## video.views                              6.521e-04  4.053e-05  16.091  < 2e-16
## uploads                                  1.940e+01  8.237e+00   2.355  0.01891
## video_views_rank                        -6.200e+00  1.340e+00  -4.628 4.69e-06
## country_rank                             7.367e+02  8.695e+02   0.847  0.39726
## channel_type_rank                        3.248e+03  1.026e+03   3.166  0.00164
## video_views_for_the_last_30_days         8.983e-03  8.382e-04  10.716  < 2e-16
## subscribers_for_last_30_days             1.217e+01  6.370e-01  19.101  < 2e-16
## created_year                             1.210e+05  7.634e+04   1.585  0.11353
## created_date                            -2.547e+04  3.738e+04  -0.681  0.49590
## Gross.tertiary.education.enrollment....  1.146e+03  2.741e+04   0.042  0.96668
## Population                               8.963e-03  4.167e-03   2.151  0.03196
## Unemployment.rate                        8.793e+04  1.902e+05   0.462  0.64402
## Urban_population                        -2.372e-02  1.231e-02  -1.928  0.05447
## Latitude                                 2.783e+04  1.922e+04   1.448  0.14832
## Longitude                               -1.366e+04  7.938e+03  -1.721  0.08590
##                                            
## (Intercept)                                
## rank                                    ** 
## subscribers                             ***
## video.views                             ***
## uploads                                 *  
## video_views_rank                        ***
## country_rank                               
## channel_type_rank                       ** 
## video_views_for_the_last_30_days        ***
## subscribers_for_last_30_days            ***
## created_year                               
## created_date                               
## Gross.tertiary.education.enrollment....    
## Population                              *  
## Unemployment.rate                          
## Urban_population                        .  
## Latitude                                   
## Longitude                               .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7457000 on 504 degrees of freedom
## Multiple R-squared:  0.7706, Adjusted R-squared:  0.7629 
## F-statistic:  99.6 on 17 and 504 DF,  p-value: < 2.2e-16
