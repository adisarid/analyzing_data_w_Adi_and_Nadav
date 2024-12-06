# Script for loading the csv into duckdb
library(tidyverse)
library(duckdb)
library(duckplyr)


# Check structure
# file1 <- read_delim("data/053cea08-09bc-40ec-8f7a-156f0677aff3.csv", 
# delim = "|", 
# locale = locale(encoding = "windows-1255"), 
# quote = "") # there is an issue with the גפ"מ, hence not accepting quotes.


# file1_processed <- file1 |> 
#   mutate(across(where(is.character), ~str_remove_all(., '\"'))) # fix redundant quotes

# file1_factorized <- file1_processed |> 
#   mutate(
#     across(
#       c(sug_degem, ramat_gimur,
#         degem_manoa, baalut, tzeva_rechev, sug_delek_nm,kinuy_mishari), 
#       as_factor)) |> 
#   mutate(
#     across(
#       c(
#         mispar_rechev, 
#         shnat_yitzur),
#       as.numeric)) |> 
#   mutate(
#     across(
#       c(mivchan_acharon_dt, tokef_dt),
#       lubridate::ymd
#     )
#   ) |> 
#   mutate(moed_aliya_lakvish = lubridate::ym(moed_aliya_lakvish))

# file2 <- read_delim("data/0866573c-40cd-4ca8-91d2-9dd2d7a492e5.csv", 
# n_max = 100, 
# delim = "|", 
# locale = locale(encoding = "utf-8"))

# file1 seems to be the main file and file2 seems to be additions.
# sum(names(file1) %in% names(file2))
# sum(names(file2) %in% names(file1))

# Initialize the duckdb

# con <- dbConnect(duckdb())

# ## Not working
# ducks <- dbWriteTable(con, "mot_tbl", file1_factorized)

# dbExecute(con, "EXPORT DATABASE 'data/duckdb'")

# dbDisconnect(con)

# Read db from files:
con <- dbConnect(duckdb())
dbExecute(con, "IMPORT DATABASE 'data/duckdb'")
dbListTables(con)



# Showing a glimpse of the table ------------------------------------------

tbl(con, "mot_tbl") %>% 
  glimpse()

tbl(con, "mot_tbl") %>% 
  count(ramat_gimur) %>% 
  collect()

# How to identify electric vehicles?

tbl(con, "mot_tbl") %>% 
  count(sug_delek_nm)

# When was the vehicle on-road?

# Monthly data:
tbl(con, "mot_tbl") %>% 
  count(moed_aliya_lakvish, sug_delek_nm) %>% 
  collect() %>% 
  ggplot(aes(x = moed_aliya_lakvish, y = n, fill = sug_delek_nm)) + 
  geom_col() + 
  ggtitle("Number of vehicles went on road per month")

# Annual data:
tbl(con, "mot_tbl") %>% 
  count(shnat_yitzur, sug_delek_nm) %>% 
  collect() %>% 
  filter(shnat_yitzur >= 2010) %>% 
  ggplot(aes(x = shnat_yitzur, y = n, fill = sug_delek_nm)) + 
  geom_col() + 
  ggtitle("Number of vehicles went on road per year")

tbl(con, "mot_tbl") %>% 
  count(shnat_yitzur, sug_delek_nm) %>% 
  collect() %>% 
  filter(shnat_yitzur >= 2010) %>% 
  ggplot(aes(x = shnat_yitzur, y = n, fill = sug_delek_nm)) + 
  geom_col(position = position_fill()) + 
  ggtitle("Number of vehicles went on road per year") + 
  scale_y_continuous(labels = scales::percent)

tbl(con, "mot_tbl") %>% 
  count(baalut) %>% 
  collect()

# Distribution of Baalut compared by fuel type
tbl(con, "mot_tbl") %>% 
  filter(sug_delek_nm %in% c("בנזין",
                             "חשמל",
                             "דיזל")) %>% 
  filter(shnat_yitzur >= 2022) %>% 
  count(sug_delek_nm, baalut) %>% 
  collect() %>% 
  ggplot(aes(x = baalut, y = n, fill = sug_delek_nm)) + 
  geom_col(position = position_fill()) + 
  ggtitle("Distribution of fuel type as a function of ownership") + 
  scale_y_continuous(labels = scales::percent)


# Percentage of electric by ownership and year ----------------------------

percent_electric_tbl <- tbl(con, "mot_tbl") %>% 
  filter(sug_delek_nm %in% c("בנזין",
                             "חשמל",
                             "דיזל")) %>% 
  filter(shnat_yitzur >= 2020) %>% 
  count(shnat_yitzur, baalut, sug_delek_nm) %>% 
  group_by(shnat_yitzur, baalut) %>% 
  mutate(prop = n/sum(n)) %>% 
  collect()

percent_owner_plot <- percent_electric_tbl %>% 
  filter(sug_delek_nm == "חשמל") %>% 
  mutate(baalut = case_match(baalut,
                             "פרטי" ~ "Private",
                             "חברה" ~ "Company",
                             "סוחר" ~ "Dealership",
                             "ליסינג" ~ "Leasing",
                             "השכרה" ~ "Rental")) %>% 
  ggplot(aes(x = shnat_yitzur, y = prop, color = baalut)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Percent of electric vehicles by ownership type and year") + 
  scale_y_continuous(labels = scales::percent) + 
  guides(color = guide_legend("Ownership")) + 
  ylab("Proportion of electric vehicles") + 
  xlab("Year")

# remotes::install_github("tidyverse/elmer")

chat <- elmer::chat_openai(system_prompt =
                             "
                           You are an expert in car market analysis.
                           Your responses are well explained.
                           "
                             )
chat$chat("Explain the trends and meaning of this graph:",
          content_image_plot()
          )


# Electric vehicles by manufacturer ---------------------------------------

car_models <- tbl(con, "mot_tbl") %>% 
  filter(sug_delek_nm == "חשמל") %>% 
  filter(shnat_yitzur >= 2020) %>% 
  count(tozeret_nm, shnat_yitzur) %>% 
  collect()

car_models_countries <- paste0(car_models$tozeret_nm, collapse = "\n")

countries_clean <- chat$chat(glue::glue("Take this list of model names, and extract just the country names.
          Separate the country names by the '|', for example:
          פיז'ו ספרד\nאאודי בלגיה 
          would yield an output of
          ספרד|בלגיה
          
          You do not need to repeat countries even if the appear more than once.
          Do not add any explanations to the output, just the requested string.
          
          Here is the list:
          
                     {car_models_countries}
          
          "))

# Retain LLM output
countries_clean <- 
  "הונג|מקסיקו|צרפת|סין|ארה''ב|גרמנ|קוריאה|בלגיה|אנגליה|גרמניה|יפן|רומניה|ספרד|סלוב|איטליה|צ'כיה|סלובקיה|אוסטריה|פולין"

car_models_cln <- car_models %>% 
  mutate(tozeret_nm = str_remove_all(tozeret_nm, countries_clean)) %>% 
  mutate(tozeret_nm = str_remove_all(tozeret_nm, "יה|ארהב|גרמנ|הולנד|סלוב|-")) %>% 
  mutate(tozeret_nm = str_squish(tozeret_nm)) %>% 
  group_by(tozeret_nm, shnat_yitzur) %>% 
  summarize(n = sum(n))

top5_2024 <- car_models_cln %>% 
  ungroup() %>% 
  filter(shnat_yitzur == 2024) %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  select(tozeret_nm)

brand_colors <- c(
  BYD = "#FF0000",    # Example: Bright Yellow
  Chery = "#FF6600",  # Example: Orange
  Geely = "#0000FF",  # Example: Blue
  MG = "#008000",     # Example: Green
  Tesla = "black"   # Example: Dark Red
)

car_models_tib <- car_models_cln %>% 
  semi_join(top5_2024) %>% 
  mutate(tozeret_nm_new = case_match(tozeret_nm,
                                 "בי ווי די" ~ "BYD",
                                 "גילי" ~ "Geely",
                                 "טסלה" ~ "Tesla",
                                 "מ.ג" ~ "MG",
                                 "צ'רי" ~ "Chery"
                                 ))

brand_images <- 
  tibble(brand = names(brand_colors),
         image_path = c(
           "logos/BYD.png",
           "logos/Chery_logo.png",
           "logos/Geely.png",
           "logos/Mg.png",
           "logos/Tesla.png"
         ),
         x = 2024.25) %>% 
  left_join(car_models_tib %>% 
              filter(shnat_yitzur == 2024) %>% 
              ungroup() %>% 
              select(tozeret_nm_new, n), 
            by = c(brand = "tozeret_nm_new")) %>% 
  mutate(n = if_else(brand == "Chery", n - 750, n))

theme_set(theme_minimal(base_family = "Rubik") + 
            theme(legend.justification = "top", panel.grid = element_blank()))

car_models_p <- car_models_tib %>% 
  ggplot(aes(x = shnat_yitzur, y = n, color = tozeret_nm_new,
             alpha = tozeret_nm_new)) +
  geom_line(size = 1) + 
  geom_point() + 
  ggtitle("Top 5 EV sales: trends in Israel 2020-2024 (Q3)",
          subtitle = "BYD leading the Charge in the EV Market (MG and Chery also on the rise)") + 
  xlab("Year") + 
  ylab("Cars sold") + 
  guides(color = "none",
         alpha = "none") + 
  labs(caption = 
         "Based on ministry of transportation data data.gov.il
          Top 5 is based on 2024 sales up to Q3") + 
  scale_y_continuous(labels = ~scales::number(., 
                                              scale_cut = scales::cut_short_scale())) + 
  theme(panel.grid.major.y = element_line(color = "grey", linewidth = 0.5)) +#,
        # panel.background = element_rect(fill = "#F5F5F5")) + 
  scale_color_manual(values = brand_colors) + 
  scale_alpha_manual(values = c(BYD = 1,
                                Chery = 0.6,
                                Geely = 0.6,
                                MG = 0.6,
                                Tesla = 0.6)) +
  geom_text(data = brand_images,
             aes(x = x, y = n, label = brand,
                 color = brand, alpha = brand),
            inherit.aes = F, show.legend = F)

car_models_p

chat$chat("Define a vector with brand colors for: 
          BYD, Chery, Geely, MG, Tesla, to be incorporated in the 
          legend of the plot instead of the current",
          elmer::content_image_plot())

chat$chat("Create a variation where BYD is visibly different than Tesla. Retain the tesla color just suggest an alternative to BYD")

chat$chat("Suggest a light background color instead of the white to improve visibility of BYD")

chat$chat("Suggest a ssubtitle that encompasses the interesting insight about BYD vs Tesla")

chat$chat("Suggest a ssubtitle about BYD supremacy")

chat$chat("How do i incorporate logos in a ggplot?")

chat$chat("
Improve:

🚗🔋 Power Shift in the EV Market: BYD Is in the Lead** 🔋🚗    
(Actually, has been since last year).

Based on an analysis of the ministry of transportation data 2020-2024 (Q3) the top 
five brands are BYD, MG, Tesla, Geely, and Chery.

- **BYD** emerges as a dominant force, setting new benchmarks for innovation and 
market presence.
- **MG** and **Chery** are also climbing, showcasing strong growth and competitive 
strategies.
- This shift highlights the dynamic nature of the automotive industry and underscores 
the importance of adaptability and innovation - Tesla started strong in 2021 but is now behind.

As the EV landscape evolves, staying ahead of these trends becomes crucial for 
stakeholders and consumers alike. 

What do you think this means for the future of electric vehicles? 🤔 Share your 
thoughts in the comments! 

#EV #BYD #Innovation #MarketTrends #Sustainability #AutomotiveIndustry
          ")
