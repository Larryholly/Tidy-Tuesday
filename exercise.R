
df |> 
  count(activity_type)

#most common - H II - 203, Liner - 94 , Seyfert 52


df |> 
  group_by(hubble_type) |> 
  count(activity_type) |> 
  print(n= 50)


df$hubble_type


df |> 
 group_by(hubble_type) |> 
  
library(dplyr)
library(stringr)

df <- df |> 
  mutate(
    morph_group = case_when(
      str_starts(hubble_type, "E") ~ "elliptical",
      str_starts(hubble_type, "S0") ~ "lenticular",
      str_starts(hubble_type, "SB0") ~ "lenticular",
      str_starts(hubble_type, "S") ~ "spiral",
      str_starts(hubble_type, "SB") ~ "spiral",
      str_starts(hubble_type, "I") ~ "irregular",
      TRUE ~ "other"
    )
  )



df$morph_group



#vary with galaxy morphology

df |> 
  group_by(morph_group) |> 
  count(activity_type) |> 
  arrange(desc(n))

# Spiral are the most common n= 319
# Ellipical is pretty uncommon n= 25

#Which galaxy morphological types are most likely to host Seyfert nuclei vs. LINERs?

df |> 
  group_by(morph_group) |> 
  count(activity_type) |> 
  filter(activity_type %in% c("Seyfert","LINER")) |>
  mutate(percent= n/sum(n)* 100) |> 
  arrange( desc(percent))

df |> 
  group_by(morph_group) |> 
  summarise(
    total= n(),
    seyfert_liner = sum(activity_type %in% c("Seyfert","LINER")), 
    percent = seyfert_liner/ total *100
  )

# elliptical - 92%
#  other - 57%
# Lenticular- 56%


names(df)


#Is there a relationship between a galaxy's velocity dispersion (a proxy for central mass) 
# and the type of nuclear activity it hosts?


library(ggplot2)

# galaxy's velocity dispersion vs  nuclear activity 

ggplot(df, aes( x= velocity_dispersion_km_s, y= activity_type )) +
  geom_point(na.rm=TRUE)

ggplot(df,aes(x=velocity_dispersion_km_s , y=activity_type))+
  geom_boxplot(na.rm=TRUE)


# 



ggplot(df, aes( x=activity_type , y= velocity_dispersion_km_s )) +
  geom_point(na.rm=TRUE)

ggplot(df,aes(x=activity_type , y=velocity_dispersion_km_s))+
         geom_boxplot(na.rm=TRUE)


# Velocity dispersion differs across activity types.
# Active nuclei (LINER, Seyfert) 
#tend to have higher velocity dispersion than H II galaxies.

library(tidyverse)

df <- df |> 
  mutate(
    oi_hg = oiii_5007/ h_gamma,
    ni_h = nii_6583/h_beta,
    log_oi_hg = log10(oi_hg),
    log_ni_h  = log10(ni_h)
  )

 
#final plot
 
ggplot(df, aes(x = log_oi_hg, y = log_ni_h, shape = morph_group)) +
  geom_point(na.rm = TRUE) +
  labs(
    x = expression(log[10]("[NII] 6583 / H"*alpha)),
    y = expression(log[10]("[OIII] 5007 / H"*beta)),
    title = "oxygen excitation ratio vs nitrogen excitation ratio"
  )

names(df)
    
#h_gamma - H-gamma (4340 angstrom) line intensity relative to H-alpha (= 1.0).
# oiii_5007 -5007 angstrom forbidden line intensity relative to H-alpha.
# Strongest line from doubly-ionized oxygen. = oi_hg 



#h _beta: H-beta (4861 angstrom) line intensity relative to H-alpha (= 1.0). 
# Fundamental reference line for emission-line diagnostics.
# nii_6583:  6583 angstrom forbidden line intensity relative to 
# H-alpha. Key line in BPT classification diagrams. = ni_h









