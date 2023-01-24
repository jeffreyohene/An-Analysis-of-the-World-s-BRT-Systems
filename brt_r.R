#Packages needed
library('tidyverse', 'scales')

set.seed(1234)
theme_set(theme_classic())
#Load datasets for Asian countries

china <- read_csv('brtdata-china.csv')
india <- read_csv('brtdata-india.csv')
indonesia <- read_csv('brtdata-indonesia.csv')
iran <- read_csv('brtdata-iran.csv')
israel <- read_csv('brtdata-israel.csv')
japan <- read_csv('brtdata-japan.csv')
pakistan <- read_csv('brtdata-pakistan.csv')
rep_of_korea <- read_csv('brtdata-republic_of_korea.csv')
taiwan <- read_csv('brtdata-taiwan.csv')
thailand <- read_csv('brtdata-thailand.csv')
vietnam <- read_csv('brtdata-vietnam.csv')

#Bind datasets 
asien <- rbind(
  china,india,indonesia,iran,israel,japan,pakistan,
  rep_of_korea,taiwan,thailand,vietnam)

str(asien)

#Rename columns for more efficient identification

asien <- asien %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )

head(asien, n= 3)

#Calculate daily passenger total & Corridor length

tot_asi <- asien %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

print(tot_asi)

#Top 10 cities by daily passenger count

top_cities_asien <- asien %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_asien

#Calculate mean of the passenger distribution
avg_asi <- asien %>%
  select(city, passengers) %>%
  summarize(
    avg = mean(passengers),
    avg_km = mean(asien$length)
  )

avg_asi

#Split to quartiles to check real structure of passenger distribution
quantile(asien$passengers)
IQR(asien$passengers)


#Visualize quartiles
p0 <- ggplot(
  asien, aes(passengers)
) +
  geom_boxplot(fill = 'darkgreen', alpha=0.6)

p0 + coord_flip()

#Load datasets for African countries
nigeria <- read_csv('brtdata-nigeria.csv')
south_afrika <- read_csv('brtdata-south_africa.csv')
tanzania <- read_csv('brtdata-tanzania.csv')

#Bind datasets into a single dataframe
afrika <- rbind(
  nigeria, south_afrika, tanzania
)

afrika

#Rename columns for more efficient identification
afrika <- afrika %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )

afrika

str(afrika)

#Sum of daily passengers and corridor distance
tot_afr <- afrika %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

tot_afr

#Top 3 African cities by daily passenger total
top_cities_afrika <- afrika %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_afrika

#Calculate mean of the passenger distribution
avg_afr <- afrika %>%
  select(city,passengers) %>%
  summarize(
    avg_ppl=mean(passengers),
    avg_km=mean(afrika$length)
  )

avg_afr

#Split to quartiles to check real structure of passenger distribution
quantile(afrika$passengers)
IQR((afrika$passengers))

#Visualize quartiles
p2 <- ggplot(
  afrika, aes(passengers)
) +
  geom_boxplot(fill = 'orange', alpha = 0.6)

p2 + coord_flip()

#Density plot for Africa

p3 <- ggplot(
  afrika,aes(passengers)
) +
  geom_density(fill = 'orange', alpha = 0.6)

p3 + geom_vline(
  aes(
    xintercept = mean(afrika$passengers)),
  color = 'black', linetype = "dashed", size = 1)+
  scale_y_continuous(labels = comma)

#Load datasets for European countries
finland <- read_csv('brtdata-finland.csv')
france <- read_csv('brtdata-france.csv')
germany <- read_csv('brtdata-germany.csv')
netherlands <- read_csv('brtdata-netherlands.csv')
spain <- read_csv('brtdata-spain.csv')
sweden <- read_csv('brtdata-sweden.csv')
switzerland <- read_csv('brtdata-switzerland.csv')
turkey <- read_csv('brtdata-turkey.csv')
united_kingdom <- read_csv('brtdata-united_kingdom.csv')


#Bind datasets into a single dataframe
europa <- rbind(
  finland, france, germany, netherlands, 
  spain, sweden, switzerland, turkey, united_kingdom)


#Rename columns for more efficient identification
europa <- europa %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )

europa

str(europa)

#Sum of passengers per day and total corridor distance
tot_eur <- europa %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

tot_eur

#Top 3 European cities by daily passenger total
top_cities_europa <- europa %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_europa

#Calculate mean of the passenger distribution
avg_eu <- europa %>%
  select(city, passengers) %>%
  summarize(
    avg_ppl = mean(passengers),
    avg_km = mean(europa$length)
  )

avg_eu

#Split to quartiles to check real structure of passenger distribution
quantile(europa$passengers)
IQR((europa$passengers))

#Visualize quartiles
p4 <- ggplot(
  europa,aes(passengers)
) +
  geom_boxplot(fill = 'orange', alpha = 0.6)

p4 + coord_flip()

#Density plot for Europe
p5 <- ggplot(
  europa,aes(passengers)
) +
  geom_density(fill = 'orange', alpha = 0.6)

p5 + geom_vline(aes(xintercept = mean(europa$passengers)),
                color = 'black', linetype = "dashed", size = 1)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)

#Load datassets
canada <- read_csv('brtdata-canada.csv')
united_states <- read_csv('brtdata-united_states.csv')

#Bind datasets into a single dataframe
nord_amerika <- rbind(
  canada, united_states
)

#Rename columns for more efficient identification
nord_amerika <- nord_amerika %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )

str(nord_amerika)

#Sum of passengers per day and total corridor distance
tot_na <- nord_amerika %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

tot_na

#Top 3 cities by daily passenger total
top_cities_n_amerika <- nord_amerika %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_n_amerika

#Calculate mean of the passenger distribution
avg_na <- nord_amerika %>%
  select(city, passengers) %>%
  summarize(
    avg_ppl = mean(passengers),
    avg_km = mean(nord_amerika$length)
  )

avg_na

#Split to quartiles to check real structure of passenger distribution
quantile(nord_amerika$passengers)
IQR(nord_amerika$passengers)
#Visualize quartiles
p6 <- ggplot(
  nord_amerika, aes(passengers)) +
  geom_boxplot(fill = 'black', alpha = 0.6)

p6 + coord_flip()

#Density plot for North America

p7 <- ggplot(nord_amerika,aes(passengers)) +
  geom_density(fill = 'black', alpha = 0.6)

p7 + geom_vline(aes(xintercept=mean(nord_amerika$passengers)),
                color = 'black', linetype = 'dashed', size = 1)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)

#Load datassets
argentina <- read_csv('brtdata-argentina.csv')
brazil <- read_csv('brtdata-brazil.csv')
chile <- read_csv('brtdata-chile.csv')
colombia <- read_csv('brtdata-colombia.csv')
ecuador <- read_csv('brtdata-ecuador.csv')
el_salvador <- read_csv('brtdata-el_salvador.csv')
guatemala <- read_csv('brtdata-guatemala.csv')
mexico <- read_csv('brtdata-mexico.csv')
peru <- read_csv('brtdata-peru.csv')
uruguay <- read_csv('brtdata-uruguay.csv')
venezuela <- read_csv('brtdata-venezuela.csv')

#Bind datasets into a single dataframe
latein_amerika <- rbind(
  argentina, brazil, chile, colombia, ecuador, 
  el_salvador, guatemala, mexico, peru, uruguay, venezuela
)

#Rename columns for more efficient identification
latein_amerika <- latein_amerika %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )

str(latein_amerika)

#Sum of passengers per day and total corridor distance for Latin America
tot_la <- latein_amerika %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

tot_la

#Top 3 Latin American cities by passengers per day
top_cities_l_amerika <- latein_amerika %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_l_amerika

#Calculate mean of the passenger distribution
avg_la <- latein_amerika %>%
  select(city, passengers) %>%
  summarize(
    avg_ppl = mean(passengers),
    avg_km = mean(latein_amerika$length)
  )

avg_la

#Split to quartiles to check real structure of passenger distribution
quantile(latein_amerika$passengers)
IQR(latein_amerika$passengers)

#Visualize quartiles
p8 <- ggplot(
  latein_amerika,aes(passengers)
) +
  geom_boxplot(fill = 'orange', alpha = 0.6)

p8 + coord_flip()

#Density plot for Latin Amerika

p9 <- ggplot(
  latein_amerika,aes(passengers)
) +
  geom_density(fill = 'orange', alpha = 0.6)

p9 + geom_vline(
  aes(xintercept = mean(latein_amerika$passengers)),
  color = 'black', linetype = 'dashed', size = 1)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)


#Load datassets
australia <- read_csv('brtdata-australia.csv')
new_zealand <- read_csv('brtdata-new_zealand.csv')

#Bind datasets into a single dataframe
oceania <- rbind(
  australia, new_zealand
)

#Rename columns for more efficient identification
oceania <- oceania %>%
  rename(
    city = Cities,
    passengers = `Passengers per Day`,
    corridors = `Number of Corridors`,
    length = `Length (km)`
  )


str(oceania)

#Total passengers per day and total distance for Oceania
tot_oce <- oceania %>%
  summarize(
    passengers_per_day = sum(passengers),
    total_distance = sum(length)
  )

tot_oce

#Top 3 cities by daily total passenger total
top_cities_oceania <- oceania %>%
  select(city, passengers, corridors, length) %>%
  arrange(desc(passengers))%>%
  slice(1:3)

top_cities_oceania

#Calculate mean of the passenger distribution
avg_oce <- oceania %>%
  select(city, passengers) %>%
  summarize(
    avg_ppl = mean(passengers),
    avg_km = mean(oceania$length)
  )

avg_oce

#Split to quartiles to check real structure of passenger distribution
quantile(oceania$passengers)
IQR(oceania$passengers)

#Visualize quartiles
p10 <- ggplot(
  oceania, aes(passengers)
) +
  geom_boxplot(fill = 'lightblue', alpha = 0.6)

p10 + coord_flip()

#Density plot for Oceania
set.seed(250)

p11 <- ggplot(
  oceania, aes(passengers)
) +
  geom_density(fill = 'lightblue', alpha = 0.6)

p11 + geom_vline(aes(xintercept = mean(oceania$passengers)),
                 color = 'black', linetype = 'dashed', size = 1)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)


#Bind continents to a single dataframe
kontinenten <- rbind(
  afrika, asien, europa, nord_amerika, latein_amerika, oceania
)

head(kontinenten)

afr_cities <- 5
asi_cities <- 40
eur_cities <- 40
na_cities <- 20
la_cities <- 59
oce_cities <- 4


#Assign a continent column
afrika$continent <- 'Africa'
asien$continent <- 'Asia'
europa$continent <- 'Europe'
latein_amerika$continent <- 'Latin America'
nord_amerika$continent <- 'North America'
oceania$continent <- 'Oceania'

#Africa
top_city_afr <- afrika %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Asia
top_city_asi <- asien %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Europa
top_city_eur <- europa %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Latein Amerika
top_city_la <- latein_amerika %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Nord-Amerika
top_city_na <- nord_amerika %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Oceania
top_city_oce <- oceania %>%
  select(city, passengers, continent) %>%
  arrange(desc(passengers))%>%
  slice(1:1)

#Join results
top_city_kontinent <- rbind(
  top_city_afr, top_city_asi, top_city_eur,
  top_city_la, top_city_na, top_city_oce
)

#Rank continents according to their cities with highest daily passenger total
top_city_kontinent %>%
  group_by(passengers, continent) %>%
  arrange(desc(passengers))

#Rank continents according to total passengers
top_kontinent <- kontinenten%>%
  group_by(kontinenten$continent) %>% 
  summarise(passengers = sum(passengers))%>%
  arrange(desc(passengers))

head(top_kontinent)

#Rank total corridor distance by continent
top_dist_kontinent <- kontinenten %>%
  group_by(kontinenten$continent) %>% 
  summarise(length = sum(length))%>%
  arrange(desc(length))

head(top_dist_kontinent)

#Rank corridor total by continent
top_corridors <- kontinenten%>%
  group_by(kontinenten$continent) %>% 
  summarise(corridors = sum(corridors))%>%
  arrange(desc(corridors))

head(top_corridors)

#Cities with over a million daily passenger total
million_daily <- kontinenten%>%
  group_by(kontinenten$city) %>% 
  filter(passengers > 1000000)%>%
  arrange(desc(passengers))

head(million_daily)

#Variable 1: length
ggplot(
  kontinenten,aes(length)
)+
  geom_histogram()

#Variable 2: passengers
ggplot(
  kontinenten,aes(passengers)
)+
  geom_density(fill = 'green', alpha = 0.7)

#Bivariate relationship visual
ggplot(
  kontinenten, aes(length)
) +
  geom_histogram(aes(y =..density..),
                 colour = 'black', fill = 'white') +
  geom_density(alpha = 0.6, fill = 'darkgreen')

#Scatterplot to visualize bivariate correlation
ggplot(
  kontinenten, aes(length, passengers)
) +
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(title = 'Corridor Length & Passengers',
       x = 'Corridor Length',
       y = 'Daily Passengers')


#Statistical Correlation test
cor.test(kontinenten$passengers,kontinenten$length)

#Variable 1: corridors
ggplot(
  kontinenten,aes(corridors)
)+
  geom_histogram(bins = 10)

#Variable 2: passengers
ggplot(
  kontinenten,aes(passengers)
)+
  geom_density(fill = 'brown', alpha = 0.7)

ggplot(
  kontinenten, aes(corridors)
) +
  geom_histogram(aes(y = ..density..),
                 colour = 'black', fill = 'white') +
  geom_density(alpha = 0.6, fill = 'brown')


#Scatterplot to visualize bivariate correlation
ggplot(
  kontinenten, aes(corridors, passengers)
) +
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Corridors & Passengers',
       x = 'Corridor Length',
       y = 'Daily Passengers') 

#Statistical Correlation test
cor.test(kontinenten$passengers, kontinenten$corridors)
