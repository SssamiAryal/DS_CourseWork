library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)
library(fmsb)

# Set working directory path
base_path <- "Coursework_DS_Samir_Nath_Arjyal"

house_prices_2022 <- read_csv(file.path(base_path, "Cleaned data /cleaned_house_prices_2022.csv"))
house_prices_2023 <- read_csv(file.path(base_path, "Cleaned data /cleaned_house_prices_2023.csv"))
house_prices_2024 <- read_csv(file.path(base_path, "Cleaned data /cleaned_house_prices_2024.csv"))

# Add year column
house_prices_2022 <- house_prices_2022 %>% mutate(year = 2022)
house_prices_2023 <- house_prices_2023 %>% mutate(year = 2023)
house_prices_2024 <- house_prices_2024 %>% mutate(year = 2024)

# Combine all house price data
all_house_prices <- bind_rows(house_prices_2022, house_prices_2023, house_prices_2024)

# Standardize county names to assign to Cheshire or Cumberland
all_house_prices <- all_house_prices %>%
  mutate(
    region = case_when(
      county %in% c("Cheshire", "Cheshire East", "Cheshire West And Chester") ~ "Cheshire",
      county == "Cumberland" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other")

broadband_performance <- read_csv(file.path(base_path, "Cleaned data /broadbandSpeed_performance.csv"))

# Add postcode area for classification (first letters of postcode)
broadband_performance <- broadband_performance %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  )


population_data <- read_csv(
  file.path(base_path, "Obtained data /Population2011_1656567141570.csv"),
  col_types = cols(
    Postcode = col_character(),
    Population = col_character()
  )
)

# Clean population data
population_data <- population_data %>%
  mutate(
    Postcode = str_replace_all(Postcode, " ", ""),
    Population = as.numeric(str_replace_all(Population, ",", ""))
  ) %>%
  filter(!is.na(Population))

# Extract postcode area for region mapping
population_data <- population_data %>%
  mutate(
    postcode_area = str_extract(Postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other")

# --- 1.4 Load and Process Crime Data ---
load_crime_data <- function(base_path) {
  crime_folder <- file.path(base_path, "Obtained data /CrimeRate")
  month_folders <- list.dirs(crime_folder, recursive = FALSE)
  all_crime_data <- list()
  
  for (folder in month_folders) {
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    for (file in csv_files) {
      tryCatch({
        crime_df <- read_csv(file, show_col_types = FALSE)
        if (grepl("cheshire", file, ignore.case = TRUE)) {
          crime_df$region <- "Cheshire"
        } else if (grepl("cumbria", file, ignore.case = TRUE)) {
          crime_df$region <- "Cumberland"
        }
        all_crime_data[[length(all_crime_data) + 1]] <- crime_df
      }, error = function(e) {})
    }
  }
  bind_rows(all_crime_data)
}

crime_data <- load_crime_data(base_path)
crime_data <- crime_data %>%
  mutate(
    year = as.numeric(str_sub(Month, 1, 4)),
    month_num = as.numeric(str_sub(Month, 6, 7)),
    month_year = Month
  ) %>%
  filter(!is.na(`Crime type`))

cat("Data loaded successfully!\n")

avg_house_prices_by_town <- all_house_prices %>%
  group_by(town, region, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    num_sales = n(),
    .groups = "drop"
  ) %>%
  filter(num_sales >= 5)

# --- Figure 1: BOXPLOT - Average House Price per County in 2023 ---
boxplot_data_2023 <- all_house_prices %>%
  filter(year == 2023)

p1 <- ggplot(boxplot_data_2023, aes(x = region, y = price, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  labs(
    title = "Figure 1: Boxplot of Average House Price per County in 2023",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "County",
    y = "House Price (£)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/Figure_1_boxplot_house_price_by_county_2023.png"), 
       plot = p1, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_1_boxplot_house_price_by_county_2023.png\n")

# --- Figure 2: BOXPLOT - House Price by Towns in Cheshire ---
cheshire_towns_2023 <- all_house_prices %>%
  filter(year == 2023, region == "Cheshire") %>%
  group_by(town) %>%
  filter(n() >= 10) %>%
  ungroup()

top_cheshire_towns <- cheshire_towns_2023 %>%
  group_by(town) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  pull(town)

cheshire_boxplot_data <- cheshire_towns_2023 %>%
  filter(town %in% top_cheshire_towns)

p2 <- ggplot(cheshire_boxplot_data, aes(x = reorder(town, price, FUN = median), y = price)) +
  geom_boxplot(fill = "#3498db", outlier.shape = 21, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  labs(
    title = "Figure 2: Boxplot of House Price by Towns in Cheshire",
    subtitle = "Top 10 Towns by Number of Sales (2023)",
    x = "Town",
    y = "House Price (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

ggsave(file.path(base_path, "Graphs/Figure_2_boxplot_house_price_cheshire_towns.png"), 
       plot = p2, width = 12, height = 8, dpi = 120)
cat("Saved: Figure_2_boxplot_house_price_cheshire_towns.png\n")

# --- Figure 3: BOXPLOT - House Prices in Cumberland ---
cumberland_towns_2023 <- all_house_prices %>%
  filter(year == 2023, region == "Cumberland") %>%
  group_by(town) %>%
  filter(n() >= 5) %>%
  ungroup()

top_cumberland_towns <- cumberland_towns_2023 %>%
  group_by(town) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  pull(town)

cumberland_boxplot_data <- cumberland_towns_2023 %>%
  filter(town %in% top_cumberland_towns)

p3 <- ggplot(cumberland_boxplot_data, aes(x = reorder(town, price, FUN = median), y = price)) +
  geom_boxplot(fill = "#e74c3c", outlier.shape = 21, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 800000)) +
  labs(
    title = "Figure 3: Boxplot of House Prices in Cumberland",
    subtitle = "Top 10 Towns by Number of Sales (2023)",
    x = "Town",
    y = "House Price (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

ggsave(file.path(base_path, "Graphs/Figure_3_boxplot_house_price_cumberland_towns.png"), 
       plot = p3, width = 12, height = 8, dpi = 120)
cat("Saved: Figure_3_boxplot_house_price_cumberland_towns.png\n")

# --- Figure 4: BAR CHART - Average House Price by Counties in 2022 ---
avg_prices_by_county_2022 <- all_house_prices %>%
  filter(year == 2022) %>%
  group_by(region) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    num_sales = n(),
    .groups = "drop"
  )

p4 <- ggplot(avg_prices_by_county_2022, aes(x = region, y = avg_price, fill = region)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("£", format(round(avg_price), big.mark = ","))), 
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = label_comma(prefix = "£"), 
                     limits = c(0, max(avg_prices_by_county_2022$avg_price) * 1.15)) +
  labs(
    title = "Figure 4: Barchart of Average House Price by Counties in 2022",
    subtitle = "Comparing Average House Prices in Cheshire and Cumberland",
    x = "County",
    y = "Average House Price (£)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/Figure_4_barchart_avg_house_price_2022.png"), 
       plot = p4, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_4_barchart_avg_house_price_2022.png\n")

# --- Figure 5: LINE GRAPH - Yearly House Price by County (2022-2024) ---
yearly_avg_prices <- all_house_prices %>%
  group_by(region, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    .groups = "drop"
  )

p5 <- ggplot(yearly_avg_prices, aes(x = year, y = avg_price, color = region, group = region)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0("£", round(avg_price/1000, 0), "k")), 
            vjust = -1, size = 4) +
  scale_y_continuous(labels = label_comma(prefix = "£"), 
                     limits = c(0, max(yearly_avg_prices$avg_price) * 1.2)) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    title = "Figure 5: Line Graph Showing Yearly House Price by County (2022-2024)",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Year",
    y = "Average House Price (£)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/Figure_5_linegraph_house_prices_2022_2024.png"), 
       plot = p5, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_5_linegraph_house_prices_2022_2024.png\n")

# =============================================================================
# SECTION 3: BROADBAND SPEED EDA AND VISUALIZATION (Figures 6-8)
# =============================================================================

# Filter broadband data for relevant regions
broadband_by_region <- broadband_performance %>%
  filter(region %in% c("Cheshire", "Cumberland"))

# --- Figure 6: BOXPLOT - Average Download Speed By County ---
p6 <- ggplot(broadband_by_region, aes(x = region, y = mean_avg_download, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(
    title = "Figure 6: Boxplot of Average Download Speed By County",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "County",
    y = "Average Download Speed (Mbit/s)",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/Figure_6_boxplot_download_speed_by_county.png"), 
       plot = p6, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_6_boxplot_download_speed_by_county.png\n")

# --- Create postcode district summary ---
broadband_by_district <- broadband_performance %>%
  mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>%
  group_by(postcode_district, region) %>%
  summarise(
    avg_download = mean(mean_avg_download, na.rm = TRUE),
    max_download = mean(mean_max_download, na.rm = TRUE),
    avg_upload = mean(mean_avg_upload, na.rm = TRUE),
    num_postcodes = n(),
    .groups = "drop"
  ) %>%
  filter(region %in% c("Cheshire", "Cumberland"))

# --- Figure 7: STACKED BAR CHART - Average and Maximum Download Speed in Cheshire ---
cheshire_broadband <- broadband_by_district %>%
  filter(region == "Cheshire") %>%
  arrange(desc(avg_download)) %>%
  head(15) %>%
  pivot_longer(cols = c(avg_download, max_download), names_to = "speed_type", values_to = "speed")

p7 <- ggplot(cheshire_broadband, aes(x = reorder(postcode_district, speed), y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Figure 7: Stacked Barchart of Average and Maximum Download Speed in Cheshire",
    subtitle = "By Postcode District",
    x = "Postcode District",
    y = "Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("avg_download" = "#3498db", "max_download" = "#1a5276"),
                    labels = c("Average Download", "Max Download"))

ggsave(file.path(base_path, "Graphs/Figure_7_stacked_barchart_cheshire_broadband.png"), 
       plot = p7, width = 12, height = 7, dpi = 120)
cat("Saved: Figure_7_stacked_barchart_cheshire_broadband.png\n")

# --- Figure 8: Average and Maximum Download Speeds in Cumberland ---
cumberland_broadband <- broadband_by_district %>%
  filter(region == "Cumberland") %>%
  arrange(desc(avg_download)) %>%
  head(15) %>%
  pivot_longer(cols = c(avg_download, max_download), names_to = "speed_type", values_to = "speed")

p8 <- ggplot(cumberland_broadband, aes(x = reorder(postcode_district, speed), y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Figure 8: Average and Maximum Download Speeds in Cumberland",
    subtitle = "By Postcode District",
    x = "Postcode District",
    y = "Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("avg_download" = "#e74c3c", "max_download" = "#922b21"),
                    labels = c("Average Download", "Max Download"))

ggsave(file.path(base_path, "Graphs/Figure_8_stacked_barchart_cumberland_broadband.png"), 
       plot = p8, width = 12, height = 7, dpi = 120)
cat("Saved: Figure_8_stacked_barchart_cumberland_broadband.png\n")


# Population by region for rate calculations
population_by_region <- population_data %>%
  group_by(region) %>%
  summarise(total_population = sum(Population, na.rm = TRUE), .groups = "drop")

# --- Figure 9: BOXPLOT - Drug Offense Rate by County ---
drug_offenses <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(region, `LSOA name`) %>%
  summarise(drug_count = n(), .groups = "drop")

p9 <- ggplot(drug_offenses, aes(x = region, y = drug_count, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(
    title = "Figure 9: Drug Offence Rate by County",
    subtitle = "Distribution of Drug Offenses by LSOA - Comparing Cheshire and Cumberland",
    x = "County",
    y = "Number of Drug Offenses",
    fill = "County"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/Figure_9_boxplot_drug_offenses_by_county.png"), 
       plot = p9, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_9_boxplot_drug_offenses_by_county.png\n")

# --- Figure 10: RADAR CHART - Vehicle Crime in December 2023 ---
# Get crime type breakdown for December 2023 for radar chart
crime_dec_2023 <- crime_data %>%
  filter(month_year == "2023-12") %>%
  group_by(region, `Crime type`) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(rate_per_10k = (count / total_population) * 10000)

# Prepare radar chart data
radar_crime_types <- c("Vehicle crime", "Burglary", "Drugs", "Robbery", 
                       "Criminal damage and arson", "Other theft")

radar_data <- crime_dec_2023 %>%
  filter(`Crime type` %in% radar_crime_types) %>%
  select(region, `Crime type`, rate_per_10k) %>%
  pivot_wider(names_from = `Crime type`, values_from = rate_per_10k, values_fill = 0)

# Create radar chart
if(nrow(radar_data) > 0) {
  # Prepare data for fmsb radar chart
  radar_matrix <- radar_data %>% select(-region) %>% as.data.frame()
  rownames(radar_matrix) <- radar_data$region
  
  # Add max and min rows for radar chart
  max_val <- max(unlist(radar_matrix), na.rm = TRUE) * 1.2
  radar_matrix <- rbind(rep(max_val, ncol(radar_matrix)), rep(0, ncol(radar_matrix)), radar_matrix)
  
  # Save radar chart
  png(file.path(base_path, "Graphs/Figure_10_radar_vehicle_crime_dec_2023.png"), 
      width = 800, height = 600, res = 100)
  
  par(mar = c(2, 2, 3, 2))
  radarchart(radar_matrix,
             axistype = 1,
             pcol = c("#3498db", "#e74c3c"),
             pfcol = c(rgb(0.2, 0.4, 0.7, 0.3), rgb(0.9, 0.3, 0.2, 0.3)),
             plwd = 2,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             vlcex = 0.8,
             title = "Figure 10: Radar Chart of Vehicle Crime in December 2023\n(Crime Rate per 10,000 People)")
  
  legend("topright", legend = c("Cheshire", "Cumberland"), 
         col = c("#3498db", "#e74c3c"), lty = 1, lwd = 2, bty = "n")
  
  dev.off()
  cat("Saved: Figure_10_radar_vehicle_crime_dec_2023.png\n")
}

robbery_sep_2023 <- crime_data %>%
  filter(`Crime type` == "Robbery", month_year == "2023-09") %>%
  group_by(region) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(rate_per_10k = (count / total_population) * 10000)

p11 <- ggplot(robbery_sep_2023, aes(x = region, y = rate_per_10k, fill = region)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(rate_per_10k, 3)), vjust = -0.5, size = 4) +
  labs(
    title = "Figure 11: Robbery Rate per 10,000 People in September 2023",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "County",
    y = "Robbery Rate per 10,000 People",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  scale_y_continuous(limits = c(0, max(robbery_sep_2023$rate_per_10k) * 1.3))

ggsave(file.path(base_path, "Graphs/Figure_11_barchart_robbery_rate_sep_2023.png"), 
       plot = p11, width = 10, height = 6, dpi = 120)
cat("Saved: Figure_11_barchart_robbery_rate_sep_2023.png\n")
drug_by_month <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(region, month_year) %>%
  summarise(drug_count = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(
    drug_per_10k = (drug_count / total_population) * 10000,
    date = ym(month_year)
  ) %>%
  arrange(date)

p12 <- ggplot(drug_by_month, aes(x = date, y = drug_per_10k, color = region, group = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Figure 12: Drug Offence Rate per 10,000 People for Cheshire and Cumberland",
    subtitle = "Monthly Trend Comparison",
    x = "Date",
    y = "Drug Offenses per 10,000 People",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

ggsave(file.path(base_path, "Graphs/Figure_12_linegraph_drug_offenses_per_10k.png"), 
       plot = p12, width = 12, height = 6, dpi = 120)
cat("Saved: Figure_12_linegraph_drug_offenses_per_10k.png\n")