#### Installing Packages ####

install.packages("rgdal")
install.packages("maptools")
install.packages("ggmap")
install.packages
install.packages("psych")
install.packages("GISTools")
install.packages("tripack")
install.packages("spam")
install.packages("spdep")
library(rgdal)
library(maptools)
library(sp)
library(foreign)
library(ggmap)
library(psych)
library(GISTools)
library(tripack)
library(spdep)

#### Constructing Matricies ####

## Constructing matrix of inverse distance weights

## Finding Neighboring Tracts 
neighbors.nb <- poly2nb(Tracts3)
print (neighbors.nb, zero.policy = TRUE)

#attr(neighbors.nb, "region.id")
#is.symmetric.nb(neighbors.nb)

## Creating matrix of inverse distance weights

d <- dnearneigh(coordinates(Tracts3),0,16093.4) #10 miles
dlist <- nbdists(d, coordinates(Tracts3))
idlist <- lapply(dlist, function(x) 1/x)           #inverse distance
w <- nb2listw(d, glist = idlist, style ="W")     # "W" style is the indicator for row standardization

w

weight_matrix <- listw2mat(w)

any(is.na(w$weights))

write.csv(weight_matrix,"N:/ProviderData/distance_weights.csv")


##Creating contiguity weight matrix

neighbors.lw <- nb2listw(neighbors.nb, zero.policy = TRUE, style = "W")

neighbors_matrix <- listw2mat(neighbors.lw)

write.csv(neighbors_matrix,"N:/ProviderData/weights.csv")

#### Moran's I on SAI ####

## Moran's I for SAI with contiguity weights
moran.test(Tracts3$SAI,neighbors.lw, zero.policy = TRUE)

moran1 <- moran.test(Tracts3$SAI,neighbors.lw, zero.policy = TRUE)

## Moran's I for SAI with inverse distance weights
moran.test(Tracts3$SAI,w)

moran2 <- moran.test(Tracts3$SAI,w)


#### Spatial Dep. Tests ####

## OLS Estimation

OLS <-lm(SAI ~ Pop + MI + Urban + Share_Private + Share_Public + Share_Unsur + Share_White + Share_Hispanic + Share_Black + Share_Asian + Total_Other_Share
          + Share_No_HS + Share_HS + Share_AA + Share_Bachelors + Share_Graduate, data = Tracts3)

summary(OLS)
stargazer(OLS, type = "text", title = "OLS Results", digits=4) 

## LM Tests using contiguity weights

lm.morantest(OLS, neighbors.lw, zero.policy = TRUE)

lm.LMtests(OLS, neighbors.lw, zero.policy = TRUE, test = "all")

## LM Tests using inverse distance weights

lm.morantest(OLS, w)

lm.LMtests(OLS, w, test = "all")
