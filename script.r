library(tidyverse)
library(janitor)
library(skimr)

#Import data and begin initial exploration

data <- read_csv(file = 'data-raw/Soccer Force Plate Data.csv') |> 
  #formats column names to a standard format adjsut some data types like date
  clean_names() |> 
  mutate(date=mdy(date)) 

#filter down to CMJ test specifically remove any entirely empty columns and provide brief overview of data
data<-data |> 
  filter(test_type=='CMJ') |>
  remove_empty("cols") |> 
  glimpse()

names(data)

# # Visualize missing data pattern before proceeding
# data |> 
#   ungroup() |> 
#   summarise(across(everything(), ~mean(is.na(.)))) |> 
#   pivot_longer(everything(), names_to = "column", values_to = "pct_missing") |> 
#   filter(pct_missing > 0.70) |> 
#   arrange(desc(pct_missing)) |> 
#   ggplot(aes(pct_missing, column)) + 
#   geom_col(width = .25)

# # Selects only columns containing any of these strings
# data |> select(contains(c('peak_force','m_s')))

# Decide on metrics
#Peak power = peak_power_w
#Rate of Power Development = 
#Mean propulsive force = mean_concentric_peak_power
#Propulsive impulse (product of force and time during ascent) = concentric_impulse_100ms_ns
#Jump height = which to use?
#F0 = concentric_peak_force_n
#V0 = concentric_peak_velcotiy_m_s

# data<-data |> 
#   select(athlete,date,position_group,weight_kg,peak_power_w,mean_concentric_peak_power,concentric_impulse_100ms_ns,concentric_peak_force_n,concentric_peak_velocity_m_s,concentric_rfd_100ms_n_s)

#Since there is three trials per athlete, I'll combine find the mean of the trials with numeric values 
data1<-data |>
  group_by(date,athlete,position_group) |>
  summarise(
    across(where(is.numeric), ~mean(., na.rm = TRUE)),
    across(where(is.character), ~first(.))  # Keep first value for character columns
  ) |> ungroup()


# # preliminary plots w/z-score/STEN
# zscores<-data1 |> 
#   group_by(position_group) |> 
#   mutate(across(4:9,scale))|> 
#   select(-weight_kg) |> 
#   pivot_longer(cols = -c(date, athlete, position_group), 
#                names_to = 'metric', 
#                values_to = 'value')
# 
# zscores |> 
#   mutate(sten = (value*2) + 5.5) |> 
#   group_by(position_group, athlete) |>  
#   mutate(metric = fct_reorder(metric, sten)) |>  
#   ungroup() |> 
#   ggplot(aes(x = sten, y = metric, fill = metric)) +
#   geom_col() +
#   geom_vline(xintercept = 5.5, linetype = "dashed") + 
#   facet_wrap(~position_group + athlete) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none",
#     axis.title.y = element_blank()
#   )

#### ATTACKER POWER PROFILES #####
data1 |> 
  filter(position_group=='Attacker') |> 
  mutate(peak_power_w_kg=peak_power_w/weight_kg) |> 
  mutate(across(c(peak_power_w_kg,concentric_rfd_100ms_n_s),scale)) |> 
  ggplot(aes(peak_power_w_kg, concentric_rfd_100ms_n_s, colour = position_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = paste0("Athlete ",athlete)), size = 3,vjust = 2, colour = 'black') +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Peak Power",
    y = "RFD-100ms",
    title = "Power Profiles | Attackers ",
    colour = "Position")+
  annotate("text", x = 1, y = 1, label = "High Power\nFast RFD", 
             size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = 1, label = "Low Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = 1, y = -1, label = "High Power\nSlow RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = -1, label = "Low Power\nSlow RFD", 
           size = 5, alpha = 0.25)+ 
  geom_smooth(method = "lm", se = FALSE, color = "gray50", 
                                               linetype = "dotted", linewidth = 0.5)
#### DEFENDER POWER PROFILES ####
data1 |> 
  filter(position_group=='Defender') |> 
  mutate(peak_power_w_kg=peak_power_w/weight_kg) |> 
  mutate(across(c(peak_power_w_kg,concentric_rfd_100ms_n_s),scale)) |> 
  ggplot(aes(peak_power_w_kg, concentric_rfd_100ms_n_s, colour = position_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = paste0("Athlete ",athlete)), size = 3,vjust = 2, colour = 'black') +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Peak Power",
    y = "RFD-100ms",
    title = "Power Profiles | Defenders ",
    colour = "Position")+
  annotate("text", x = 1, y = 1, label = "High Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = 1, label = "Low Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = 1, y = -1, label = "High Power\nSlow RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = -1, label = "Low Power\nSlow RFD", 
           size = 5, alpha = 0.25)+ 
  geom_smooth(method = "lm", se = FALSE, color = "gray50", 
              linetype = "dotted", linewidth = 0.5)

#### MIDFIELDER POWER PROFILES ####
data1 |> 
  filter(position_group=='Midfield') |> 
  mutate(peak_power_w_kg=peak_power_w/weight_kg) |> 
  mutate(across(c(peak_power_w_kg,concentric_rfd_100ms_n_s),scale)) |> 
  ggplot(aes(peak_power_w_kg, concentric_rfd_100ms_n_s, colour = position_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = paste0("Athlete ",athlete)), size = 3,vjust = 2, colour = 'black') +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Peak Power",
    y = "RFD-100ms",
    title = "Power Profiles | Midfielders ",
    colour = "Position")+
  annotate("text", x = 1, y = 1, label = "High Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = 1, label = "Low Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = 1, y = -1, label = "High Power\nSlow RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = -1, label = "Low Power\nSlow RFD", 
           size = 5, alpha = 0.25)+ 
  geom_smooth(method = "lm", se = FALSE, color = "gray50", 
              linetype = "dotted", linewidth = 0.5)

#### Goalkeeper POWER PROFILES ####
data1 |> 
  filter(position_group=='Goalkeeper') |> 
  mutate(peak_power_w_kg=peak_power_w/weight_kg) |> 
  mutate(across(c(peak_power_w_kg,concentric_rfd_100ms_n_s),scale)) |> 
  ggplot(aes(peak_power_w_kg, concentric_rfd_100ms_n_s, colour = position_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = paste0("Athlete ",athlete)), size = 3,vjust = 2, colour = 'black') +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Peak Power",
    y = "RFD-100ms",
    title = "Power Profiles | Goalkeepers ",
    colour = "Position")+
  annotate("text", x = 1, y = 1, label = "High Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = 1, label = "Low Power\nFast RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = 1, y = -1, label = "High Power\nSlow RFD", 
           size = 5, alpha = 0.25) +
  annotate("text", x = -1, y = -1, label = "Low Power\nSlow RFD", 
           size = 5, alpha = 0.25)+ 
  geom_smooth(method = "lm", se = FALSE, color = "gray50", 
              linetype = "dotted", linewidth = 0.5)


data1 |> 
  mutate(across(c(concentric_impulse_ns, concentric_peak_force_n), scale)) |> 
  ggplot(aes(concentric_impulse_ns, concentric_peak_force_n, colour = position_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Concentric Impulse",
    y = "Concentric Peak Force",
    title = "Force-Impulse Profile by Position",
    colour = "Position"
  )+annotate("text", x = 1.5, y = 1.5, label = "High Force\nHigh Impulse", 
                size = 3, alpha = 0.5) +
  annotate("text", x = -1.5, y = 1.5, label = "High Force\nLow Time", 
           size = 3, alpha = 0.5) +
  annotate("text", x = 1.5, y = -1.5, label = "Low Force\nLong Duration", 
           size = 3, alpha = 0.5) +
  annotate("text", x = -1.5, y = -1.5, label = "Low Force\nLow Impulse", 
           size = 3, alpha = 0.5)+ geom_smooth(method = "lm", se = FALSE, color = "gray50", 
                                               linetype = "dotted", linewidth = 0.5)

data1 |> 
  ggplot(aes(athlete,concentric_rfd_n_s))+geom_col()



