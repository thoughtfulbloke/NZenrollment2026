library(ggplot2)
library(readr)
library(dplyr)
# census info from Aotearoa Data Explorer
# concordance made from StatsNZ geographic boundary files
# enrollments from elections.nz
source("~/theme.R")

census <- read_csv("STATSNZ,CEN23_LOC_005,1.0,filtered,2026-09-08 13-51-54.csv")
concordance <- read_csv("SA2Electorateconcordance.csv")
enrollments <- read_csv("enrollment_as_31Aug_combined.csv")


##############
 joined <- concordance |> 
  mutate(CEN23_GEO_002 = as.numeric(sa2_id)) |> 
  inner_join(census |> filter(`Usual residence five years ago summary (RC)` == "Same as usual residence", Age == "15-29 years") |> select(CEN23_GEO_002, same15 = OBS_VALUE), 
             by="CEN23_GEO_002") |> 
  inner_join(census |> filter(`Usual residence five years ago summary (RC)` == "Total - usual residence five years ago summary (RC)", Age == "15-29 years") |> select(CEN23_GEO_002, total15 = OBS_VALUE), 
             by="CEN23_GEO_002") |> 
  inner_join(census |> filter(`Usual residence five years ago summary (RC)` == "Same as usual residence", Age == "30-64 years") |> select(CEN23_GEO_002, same30 = OBS_VALUE), 
             by="CEN23_GEO_002") |> 
  inner_join(census |> filter(`Usual residence five years ago summary (RC)` == "Total - usual residence five years ago summary (RC)", Age == "30-64 years") |> select(CEN23_GEO_002, total30 = OBS_VALUE), 
             by="CEN23_GEO_002")

aggregated <- joined |> 
  summarise(.by=c(electorate_id, electorate_name, electorate_name_ascii),
            there15 = sum(proportion_of_sa2 * same15, na.rm = TRUE),
            total15 = sum(proportion_of_sa2 * total15, na.rm = TRUE),
            there30 = sum(proportion_of_sa2 * same30, na.rm = TRUE),
            total30 = sum(proportion_of_sa2 * total30, na.rm = TRUE)
  ) |> 
  mutate(resident15 = 100 * there15 /total15) 

youth <- enrollments |> filter(Age == "18 - 24")

combo <- aggregated |> 
  inner_join(youth, by = join_by(electorate_name))

ggplot(combo, aes(x=resident15, y=`% Enrolled`)) +
  geom_point() + geom_smooth(method="lm", formula = 'y ~ x') +
  labs(x="Percentage 15-29 at same address as 5 years ago",
       y="Percentage of 18-24 enrolled",
       title="Youth residental stablity has little effect on enrollment",
       subtitle="at least at an electorate level",
       caption="Sources: StatsNZ, elections.nz") +
  theme_david()

ggsave(filename="~/Desktop/ggsky.jpg", width=2016, height=1134, units="px")
