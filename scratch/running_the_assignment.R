
---
title: "bioe215_working_data"
subtitle: "Assessment"
date: 2023-10-18
format: html
---
  
__________________________________________________________________

< Download Dataset >
  
  For this assessment, we’ll use the breeding bird phenology from Hällfors et al. (2020a). Their data are on Dryad (Hällfors et al. 2020b). We’ll complement that with the AVONET (Tobias et al. 2022) database of bird functional traits, which is on Figshare.

Download the Dryad dataset. Copy “73_species.csv” and “Traits_73_species.csv” to your data/ folder.

From the AVONET database on Figshare, download “AVONET Supplementary dataset 1.xlsx” do your data/ folder.


```{r}

download.file("https://datadryad.org/stash/downloads/file_stream/382103",
              "data/73_species.csv")

download.file("https://datadryad.org/stash/downloads/file_stream/382102",
              "data/Traits_73_species.csv")

download.file("https://figshare.com/ndownloader/files/34480856?private_link=b990722d72a26b5bfead",
              "data/AVONET_Supplementary_dataset_1.xlsx")# original file name not using _ ?

```

__________________________________________________________________

< Load Packages >
  
  ```{r}

library(tidyverse)
library(readxl) #seems to be needed

```

__________________________________________________________________

< Read Data >
  
  Use read_csv(“data/73_species.csv”) to read “73_species.csv” and assign it to a variable called bor_nestlings.

Use read_csv() to read “Traits_73_species.csv” and assign it to a variable called bor_traits.

Use readxl::read_excel() to read the “AVONET1_BirdLife” sheet from “AVONET Supplementary dataset 1.xlsx” and assign it to a variable called avonet.

```{r}

bor_nestlings <- read_csv("data/73_species.csv")

bor_traits <- read_csv("data/Traits_73_species.csv")

avonet <- readxl::read_excel("data/AVONET_Supplementary_dataset_1.xlsx")


```

__________________________________________________________________

< Explore >
  
  We’ll explore the boreal bird nestling data together. 

Follow along in your Quarto document.

```{r}

all_birds_trend <- bor_nestlings %>% 
  group_by(Year) %>% 
  summarize(mean_doy = mean(Dayofyear))

ggplot(all_birds_trend, aes(Year, mean_doy)) + 
  geom_point() +
  geom_smooth(method = "lm")

```

That combines 73 species. Let’s see the breakdown by species.

```{r}

species_trends <- bor_nestlings %>% 
  group_by(Year, Species) %>% 
  summarize(mean_doy = mean(Dayofyear),
            .groups = "drop")

ggplot(species_trends, aes(Year, mean_doy, color = Species)) + 
  geom_point() +
  geom_smooth(method = "lm")
```

So chaotic! What if we just look at the 5 most data-rich species?
  
  ```{r}

data_richness <- bor_nestlings %>% 
  count(Species)

most_rich <- data_richness %>% 
  arrange(desc(n)) %>% 
  slice(1:5)

most_rich_trends <- bor_nestlings %>% 
  filter(Species %in% most_rich$Species) %>% 
  group_by(Species, Year) %>% 
  summarize(mean_doy = mean(Dayofyear), 
            .groups = "drop")

ggplot(most_rich_trends, aes(Year, mean_doy, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

```

A general pattern, with one conflicting example.

What species have the strongest trends?
  
  ```{r}

# I'm giving you a *function* to help here. You'll learn more about those next week.

# Finds the slope of the relationship between y and x
trend <- function(x, y) {
  xy_lm <- lm(y ~ x)
  coef(xy_lm)[2]
}

# Calculate the trend for all species
bor_trends <- bor_by_year %>% 
  group_by(Species) %>% 
  summarize(doy_trend = trend(Year, mean_doy))
```

Spot check two species

```{r}

soi <- c("ARDCIN", "LARMIN")
bor_by_year %>% 
  filter(Species %in% soi) %>% 
  ggplot(aes(Year, mean_doy, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

```

__________________________________________________________________

< Your Turn >
  
  Find the species with the most positive and most negative trends, then plot just those.

nrow_bor_trends <- nrow(bor_trends) # Use this later
bor_extreme <- bor_trends %>% 
  # Sort by the day of year trend
  ___(___) %>% 
  # Keep just the first (most negative trend) and last (most positive trend) rows
  slice(c(___, ___))

# Now plot them
bor_by_year %>% 
  filter(Species %in% ___) %>% 
  ggplot(aes(Year, mean_doy, color = Species)) + 
  geom_point() +
  geom_smooth(method = "lm")
















