##Looking a bit further back to see if theres more of a difference or are we fallling back to a more stable %
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(segmented)
library(strucchange)
library(tidyr)
library(stringr)


me_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_Surveillance_Data_Quarter.csv")
me_data$DOD <- dmy(me_data$DOD)

me_data <- me_data %>%
  mutate(
    substance_clean = str_to_lower(str_squish(substance_list)),
    n_substances = if_else(
      is.na(substance_clean) | substance_clean == "",
      0L,
      str_count(substance_clean, ",") + 1L
    ),
    only_fentanyl = as.integer(fentanyl_ct & n_substances == 1),
    only_cocaine = as.integer(cocaine_ct & n_substances == 1),
    only_meth = as.integer(psy_stimulant_ct & n_substances == 1),
    fentanyl_and_cocaine_only = as.integer(fentanyl_ct & cocaine_ct == 1),
    fentanyl_and_meth_only = as.integer(fentanyl_ct & psy_stimulant_ct == 1),
    fentanyl_and_stim = as.integer(fentanyl_ct & cocaine_ct & psy_stimulant_ct == 1)
  )


me_data1 <- me_data %>%
  filter(year != 2026) %>%
  dplyr::select(DOD, year, fentanyl_ct, nat_semi_synth_opioid_ct)



opioid_data <- me_data1 %>%
  mutate(year_month = format(DOD, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(Fentanyl_ct = sum(fentanyl_ct, na.rm=TRUE),
            Nat_ct = sum(nat_semi_synth_opioid_ct, na.rm=TRUE))



opioid_data$year_month <- as.Date(paste0(opioid_data$year_month, "-01"))


##Segmented model to see where the points of change occured
##turning the dataset into a time series
fent_ts <- ts(opioid_data$Fentanyl_ct, start = c(2010, 1), frequency = 12)
nat_ts <- ts(opioid_data$Nat_ct, start = c(2010, 1), frequency = 12)

#run breakpoint detection
fent_bp <- breakpoints(fent_ts ~ 1)
nat_bp <- breakpoints(nat_ts ~ 1)

plot(fent_bp)
plot(nat_bp)

##fitting a segments regression with slopes
m_fent <- lm(Fentanyl_ct ~ year_month, data = opioid_data)
seg_fent <- segmented(m_fent, seg.Z = ~year_month)
summary(seg_fent)
plot(seg_fent)


m_nat <- lm(Nat_ct ~ year_month, data = opioid_data)
seg_nat <- segmented(m_nat, seg.Z = ~year_month)
summary(seg_nat)
plot(seg_nat)

##pivoting to the long way to look at both substances at the same time
long_data <- opioid_data %>%
  pivot_longer(
    cols = c(Fentanyl_ct, Nat_ct), 
    names_to = "substance",
    values_to = "count"
  )

##Basic regression against both
reg_both <- lm(count ~ year_month * substance, data = long_data)
summary(reg_both)

##segmented for both
seg_by_substance <- long_data %>%
  group_by(substance) %>%
  group_modify(~ {
    m <- lm(count ~ year_month, data = .x)
    seg <- segmented(m, seg.Z = ~year_month)
    tibble(
      substance = unique(.x$substance),
      breakpoint = seg$psi[2],
      bp_date = as.Date(seg$psi[2], origin = "1970-01-01"),
      slope_before = slope(seg)$year_month[1,1],
      slope_after = slope(seg)$year_month[2,1]
    )
  })

##Adding in the breakpoint dates onto the overall graph from the beginning
nat_bp <- as.Date("2015-12-31")
fen_bp <- as.Date("2023-08-01")


pal <- c("Fentanyl_ct" = "#0072B2",
         "Nat_ct"       = "#E69F00")

pal_labels <- c("Fentanyl_ct" = "Fentanyl (Synthetic)",
                "Nat_ct" = "Natural/Semi-synthetic")

plot2 <- ggplot(long_data, aes(x = year_month, y = count, color = substance)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.8) +
  
  scale_color_manual(values = pal, labels= pal_labels) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  
  labs(
    title    = "Synthetic vs. Semi-synthetic Opioids Over Time",
    subtitle = "Monthly counts, 2010–2026",
    x = NULL,
    y = "Monthly count",
    color = NULL,
    caption = "Source: Medical Examiner Database"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold", size = 18),
    plot.subtitle     = element_text(color = "grey40", margin = margin(b = 10)),
    plot.caption      = element_text(color = "grey50", size = 10, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.title.y      = element_text(margin = margin(r = 8)),
    
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    
    legend.position   = "top",
    legend.justification = "left",
    legend.text       = element_text(size = 12),
    
    plot.margin       = margin(15, 15, 10, 15)
  )

plot2

plot2 +
  geom_vline(
    xintercept = as.numeric(nat_bp), linetype = "dashed",color = "#D62728",size = 1
  ) +
  geom_vline(xintercept = as.numeric(fen_bp), linetype = "dashed", color = "black", size =1)

##Now looking at counts of meth, cocaine and fentanyl
sub_data <- me_data %>%
  filter(year != 2026) %>%
  dplyr::select(DOD, year,fentanyl_and_stim, fentanyl_and_cocaine_only, fentanyl_and_meth_only, only_fentanyl, only_cocaine, only_meth)

sub_data <- sub_data %>%
  mutate(year_month = format(DOD, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(Fentanyl_ct = sum(only_fentanyl, na.rm=TRUE),
            Cocaine_ct = sum(only_cocaine, na.rm=TRUE),
            Meth_ct = sum(only_meth, na.rm=TRUE),
            fent_coke = sum(fentanyl_and_cocaine_only, na.rm=TRUE),
            fent_meth = sum(fentanyl_and_meth_only, na.rm=TRUE),
            all_subs = sum(fentanyl_and_stim, na.rm=TRUE)) %>%
  mutate(year_month = as.Date(paste0(year_month, "-01")),
         t = as.numeric(year_month - min(year_month))/365.25)




##pivoting to the long way to look at both substances at the same time
long_data_sub <- sub_data %>%
  pivot_longer(
    cols = c(Fentanyl_ct, Cocaine_ct, Meth_ct), 
    names_to = "substance",
    values_to = "count"
  ) %>%
  mutate(
    year_month = as.Date(paste0(year_month, "-01")),
    substance = factor(substance, levels = names(pal_labels2))
  ) %>%
  arrange(substance, year_month) %>%
  group_by(substance) %>%
  mutate(count_smooth = rollmean(count, k = 3, fill =NA, align = "center")) %>%
  ungroup()

pal2 <- c("Fentanyl_ct" = "#0072B2",
         "Cocaine_ct"       = "#E69F00",
         "Meth_ct" = "#009E73")

pal_labels2 <- c("Fentanyl_ct" = "Fentanyl (Synthetic)",
                "Cocaine_ct" = "Cocaine",
                "Meth_ct" = "Meth")




plot3 <- ggplot(long_data_sub, aes(x = year_month, color = substance)) +
  geom_line(aes(y=count), linewidth = 0.4, alpha = 0.35) +
  geom_line(aes(y=count_smooth), linewidth = 1.1, na.rm=TRUE) +

  
  scale_color_manual(values = pal2, labels = pal_labels2) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  
  labs(
    title    = "Substance Detection Over Time",
    subtitle = "Monthly counts (faint) with 3-month moving avg (bold), 2010–2026",
    x = NULL,
    y = "Monthly count",
    color = NULL,
    caption = "Source: Medical Examiner Database"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold", size = 18),
    plot.subtitle     = element_text(color = "grey40", margin = margin(b = 10)),
    plot.caption      = element_text(color = "grey50", size = 10, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.title.y      = element_text(margin = margin(r = 8)),
    
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    
    legend.position   = "top",
    legend.justification = "left",
    legend.text       = element_text(size = 12),
    
    plot.margin       = margin(15, 15, 10, 15)
  )


plot3

##Looking at counts of combos over time
long_data_combo <- sub_data %>%
  pivot_longer(
    cols = c(fent_coke, fent_meth, all_subs), 
    names_to = "substance",
    values_to = "count"
  ) %>% dplyr::select(year_month, substance, count)  %>%
  arrange(substance, year_month) %>%
  group_by(substance) %>%
  mutate(count_smooth = rollmean(count, k = 3, fill =NA, align = "center")) %>%
  ungroup()

pal3 <- c("fent_coke" = "#CC0000",
          "fent_meth"       = "#663366",
          "all_subs" = "#CC9900")

pal_labels3 <- c("fent_coke" = "Fentanyl + Cocaine",
                 "fent_meth" = "Fentanyl + Meth",
                 "all_subs" = "Combo of All")




plot4 <- ggplot(long_data_combo, aes(x = year_month, color = substance)) +
  geom_line(aes(y=count), linewidth = 0.4, alpha = 0.35) +
  geom_line(aes(y=count_smooth), linewidth = 1.1, na.rm=TRUE) +
  
  
  scale_color_manual(values = pal3, labels = pal_labels3) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  
  labs(
    title    = "Substance Detection Over Time",
    subtitle = "Monthly counts (faint) with 3-month moving avg (bold), 2010–2026",
    x = NULL,
    y = "Monthly count",
    color = NULL,
    caption = "Source: Medical Examiner Database"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold", size = 18),
    plot.subtitle     = element_text(color = "grey40", margin = margin(b = 10)),
    plot.caption      = element_text(color = "grey50", size = 10, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.title.y      = element_text(margin = margin(r = 8)),
    
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    
    legend.position   = "top",
    legend.justification = "left",
    legend.text       = element_text(size = 12),
    
    plot.margin       = margin(15, 15, 10, 15)
  )


plot4

##Looking at share of deaths that fentanyl have
shares <- sub_data %>%
  transmute(year_month,
            Cocaine = if_else(Cocaine_ct + fent_coke >=5, 100 * fent_coke / (Cocaine_ct + fent_coke), NA_real_),
            Meth = if_else(Meth_ct + fent_meth >= 5, 100 * fent_meth / (Meth_ct + fent_meth), NA_real_)) %>%
  pivot_longer(-year_month, names_to = "stimulant", values_to = "pct_with_fentanyl") %>%
  filter(!is.na(pct_with_fentanyl))


cols <- c(Cocaine = "#2C7FB8", Meth = "#D95F0E")

ggplot(shares, aes(year_month, pct_with_fentanyl, color = stimulant, fill = stimulant)) +
   # raw monthly values, faded
     geom_point(alpha = 0.35, size = 1.8) +
     # smoothed trend is the focal element
     geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.12, linewidth = 1.3) +
     scale_color_manual(values = cols) +
     scale_fill_manual(values = cols) +
     scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
     scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1), breaks = seq(0, 100, 25)) +
     labs(
         title = "Fentanyl involvement in stimulant deaths, Davidson County",
         subtitle = "Percent of cocaine and methamphetamine deaths that include fentanyl; points are monthly, lines are smoothed trends",
         x = NULL, y = NULL, color = NULL, fill = NULL,
         caption = "Source: Medical Examiner data. Months with fewer than 5 cases omitted."
       ) +
     theme_minimal(base_size = 13) +
   theme(
         plot.title = element_text(face = "bold", size = 15),
         plot.subtitle = element_text(color = "grey35", size = 10.5, margin = margin(b = 10)),
         plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
         panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line(color = "grey88"),
         legend.position = "top",
         legend.justification = "left",
         plot.title.position = "plot"
       )


##compare substances with and without fentanyl
pivot_coke <- sub_data %>%
  pivot_longer(
    cols = c(fent_coke, Cocaine_ct), 
    names_to = "substance",
    values_to = "count"
  ) %>% dplyr::select(year_month, substance, count)  %>%
  arrange(substance, year_month) %>%
  group_by(substance) %>%
  mutate(count_smooth = rollmean(count, k = 3, fill =NA, align = "center")) %>%
  ungroup()

pal4 <- c("fent_coke" = "#FF3300",
          "Cocaine_ct"       = "#33FF99")

pal_labels4 <- c("fent_coke" = "Fentanyl + Cocaine",
                 "Cocaine_ct" = "Cocaine")



ggplot(pivot_coke, aes(year_month, count, color = substance)) +
  # raw monthly values, faded
  geom_point(alpha = 0.35, size = 1.8) +
  # smoothed trend is the focal element
  geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.12, linewidth = 1.3) +
  scale_color_manual(values = pal4, labels = pal_labels4) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  labs(
    title = "Cocaine counts with OR without Fentanyl co-detection, Davidson County",
    subtitle = "Points are monthly, lines are smoothed trends",
    x = NULL, y = NULL, color = NULL, fill = NULL,
    caption = "Source: Medical Examiner data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10.5, margin = margin(b = 10)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    legend.position = "top",
    legend.justification = "left",
    plot.title.position = "plot"
  )


##compare substances with and without fentanyl - METH
pivot_meth <- sub_data %>%
  pivot_longer(
    cols = c(fent_meth, Meth_ct), 
    names_to = "substance",
    values_to = "count"
  ) %>% dplyr::select(year_month, substance, count)  %>%
  arrange(substance, year_month) %>%
  group_by(substance) %>%
  mutate(count_smooth = rollmean(count, k = 3, fill =NA, align = "center")) %>%
  ungroup()



##regression
reg_meth <- glm(Meth_ct ~ fent_meth, data = sub_data, family = poisson)
summary(reg_meth)

nb_meth <- glm.nb(Meth_ct ~ fent_meth, data = sub_data)
summary(nb_meth)


pal5 <- c("fent_meth" = "#339",
          "Meth_ct"       = "#FFCC66")

pal_labels5 <- c("fent_meth" = "Fentanyl + Meth",
                 "Meth_ct" = "Meth")



ggplot(pivot_meth, aes(year_month, count, color = substance)) +
  # raw monthly values, faded
  geom_point(alpha = 0.35, size = 1.8) +
  # smoothed trend is the focal element
  geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.12, linewidth = 1.3) +
  scale_color_manual(values = pal5, labels = pal_labels5) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  labs(
    title = "Meth counts with OR without Fentanyl co-detection, Davidson County",
    subtitle = "Points are monthly, lines are smoothed trends",
    x = NULL, y = NULL, color = NULL, fill = NULL,
    caption = "Source: Medical Examiner data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10.5, margin = margin(b = 10)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    legend.position = "top",
    legend.justification = "left",
    plot.title.position = "plot"
  )


##Looking at when testing or detection began for the two combos
pivot_both <- sub_data %>%
  pivot_longer(
    cols = c(fent_meth, fent_coke), 
    names_to = "substance",
    values_to = "count"
  ) %>% dplyr::select(year_month, substance, count)  %>%
  arrange(substance, year_month) %>%
  group_by(substance) %>%
  mutate(count_smooth = rollmean(count, k = 3, fill =NA, align = "center")) %>%
  ungroup()

pal6 <- c("fent_meth" = "#006666",
          "fent_coke"       = "#CC33CC")

pal_labels6 <- c("fent_meth" = "Fentanyl + Meth",
                 "fent_coke" = "Fentanyl + Cocaine")



ggplot(pivot_both, aes(year_month, count, color = substance)) +
  # raw monthly values, faded
  geom_point(alpha = 0.35, size = 1.8) +
  # smoothed trend is the focal element
  geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.12, linewidth = 1.3) +
  scale_color_manual(values = pal6, labels = pal_labels6) +
  
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08)),
    limits = c(0, NA)
  ) +
  labs(
    title = "Fentanyl Co-detections, Davidson County",
    subtitle = "Points are monthly, lines are smoothed trends",
    x = NULL, y = NULL, color = NULL, fill = NULL,
    caption = "Source: Medical Examiner data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10.5, margin = margin(b = 10)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    legend.position = "top",
    legend.justification = "left",
    plot.title.position = "plot"
  )

