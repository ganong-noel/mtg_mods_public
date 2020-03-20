##
# Corelogic CLTV
##

cltv_data <- read.csv(paste0(data_path_local, "Corelogic/average_cltv_timeseries.csv"))
cltv_data <- cltv_data %>% mutate(sample = "all")

gg <- ggplot(
  cltv_data,
  aes(x = Year, y = Average_CLTV, colour = sample)
) +
  geom_point() +
  geom_line() +
  fte_theme() +
  xlab("Year") + ylab("CLTV") +
  scale_colour_manual(values = cbPalette_set2)
ggsave(file = paste0(out_path, "heloc_cltv_timeseries.png"), gg, width = wd, height = ht)

##
# FRB Mortgage Origination
##
frb_data <- read.csv(paste0(data_path_local, "FRB_CCP/mortgage_orig_4_quart_avg.csv"), check.names = FALSE)
frb_data_long <- frb_data %>% gather(`Credit Score`, value, -Date)

gg <- ggplot(
  frb_data_long,
  aes(
    x = Date, y = value, group = `Credit Score`,
    colour = `Credit Score`, shape = `Credit Score`
  )
) +
  geom_point() +
  geom_line() +
  fte_theme() +
  xlab("Date") + ylab("Ratio to 2007 Q1") +
  theme(legend.position = "top") +
  scale_colour_manual(values = cbPalette_set2) +
  ylim(0, 2) +
  scale_x_discrete(breaks = c("00:Q1", "05:Q1", "10:Q1", "15:Q1"))
ggsave(file = paste0(out_path, "mortgage_timeseries.png"), gg, width = wd, height = ht)

##
# FRB Delinquency
#
frb_data <- read.csv(paste0(data_path_local, "FRB_Delinquency/FRB_delinquency_timeseries_raw.csv"))
frb_data <- frb_data %>% mutate(
  sample = "all",
  Quarter2 = ymd(Quarter2),
  Delinquency.Rate = Delinquency.Rate / 100
)
df <- data.frame(
  xstart = ymd("2009-07-01"),
  xend = ymd("2014-10-01"),
  ymin = 0, ymax = 0.12
)

gg <- ggplot() +
  geom_point(data = frb_data, aes(x = Quarter2, y = Delinquency.Rate, colour = sample)) +
  geom_line(data = frb_data, aes(x = Quarter2, y = Delinquency.Rate, colour = sample)) +
  fte_theme() +
  xlab("Year") + ylab("Residential Mortgage Delinquency Rate") +
  scale_colour_manual(values = cbPalette_set2) +
  scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12), labels = scales::percent) +
  geom_rect(
    data = df,
    aes(xmin = xstart, xmax = xend, ymin = ymin, ymax = ymax),
    fill = "blue3", alpha = 0.25
  )
ggsave(file = paste0(out_path, "frb_delin_timeseries.png"), gg, width = wd, height = ht)
