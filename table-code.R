
# few-table-rule ----------------------------------------------------------

tibble(
  `Use Tables When` = c(
    "* The display will be used to look up individual values",
    "* It will be used to compare individual values",
    "* Precise values are required",
    "* Quantitative values include more than one unit of measure",
    "* Both detail and summary values are included"
  ),
  `Use Graphs When` = c(
    "* The display will be used to reveal relationships among whole sets of values",
    "* The message is contained in the shape of the values (e.g., patterns, trends, exceptions)",
    "",
    "",
    ""
  )
) %>% 
  gt() %>% 
  fmt_markdown(everything()) %>% 
  tab_options(
    column_labels.font.weight = "bold"
  ) %>% 
  cols_width(
    1 ~ 300,
    2 ~ 380
  ) %>% 
  tab_options(
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3)
  ) %>% 
  tab_source_note(
    md("Adapted from:<br>Few, Stephen. (2012). *Show Me the Numbers: Designing Tables and Graphs to Enlighten*.(4)57")
  ) %>% 
  gtsave(here::here("tables/few-table-rule.html"))

# few-examples ---------------------------------------------------------------

tibble(
  `Primary Function` = c("Look-Up", "", "", "Comparison", ""),
  `Relationship Type` = c("Quantitative-to-Categorical", "", "", "Quantitative-to-Quantitative", ""),
  Relationship = c("Between a **single** set of quantitative values and a **single set of categorical items**", 
                   "Between a **single** set of quantitative values and the **intersection of multiple categories**",
                   "Between a **single** set of quantitative values and the **intersection of multiple hierarchical categories**",
                   "Among a **single** set of quantitative values associated with **multiple categorical items**",
                   "Among **distinct** sets of quantitative values associated with a **single categorical item**"),
  Example = glue::glue("https://raw.githubusercontent.com/jthomasmock/radix_themockup/master/static/few{c(1:5)}.png")
) %>% 
  gt() %>% 
  fmt_markdown(c(Relationship)) %>% 
  cols_width(
    c(`Primary Function`) ~ px(150),
    2 ~ px(150),
    c(Relationship) ~ 300,
    4 ~ 300
  ) %>% 
  text_transform(
    locations = cells_body(c(Example)),
    fn = function(x){
      web_image(
        url = x,
        height = px(150)
      )
    }
  ) %>% 
  tab_options(
    table.border.top.color = "white",
    table.border.top.width = px(3),
    column_labels.font.weight = "bold"
  ) %>% 
  tab_source_note(
    md("Adapted from: Few, Stephen. (2012). *Show Me the Numbers: Designing Tables and Graphs to Enlighten* p. 53-58<br>Table Examples: @thomas_mock")
  ) %>% 
  gtsave(here::here("tables/few-table-ex.html"))

?gtsave()

# publication-quality -----------------------------------------------------

library(tidyverse)
library(gt)

player_df <- tibble(
  player = c(
    "Evan Mobley",
    "Sandro Mamukelashvili",
    "Charles Bassey",
    "Luke Garza",
    "Moses Wright",
    "Neemias Queta",
    "Isaiah Jackson",
    "Day'Ron Sharpe"
  ),
  team = c(
    "USC", "Seton Hall", "Western Kentucky",
    "Iowa", "Georgia Tech", "Utah St", "Kentucky",
    "North Carolina"
  ),
  ht = c(
    "7'0\"",
    "6'10\"",
    "6'10\"",
    "6'11\"",
    "6'9\"",
    "7'1\"",
    "6'11\"",
    "6'10\""
  ),
  dk_pct_time = c(40, 48, 50, 50, 51, 55, 60, 66),
  dk_pps = c(1.62, 1.02, 1.54,1.33,1.46,1.37,1.33,1.18),
  tip_pct_time = c(26, 10, 19, 15, 25, 27, 15, 24),
  tip_pps = c(0.88, .97,1,1.05, .63, .85, .76, .84),
  jmp_pct_time = c(33, 42, 31, 35, 25, 18, 25, 10),
  jmp_pps = c(.91, .91, .78, 1.04, .86, .74, .71, .42)
) %>%
  left_join(
    tibble(
      player = c(
        "Evan Mobley",
        "Sandro Mamukelashvili",
        "Charles Bassey",
        "Luke Garza",
        "Moses Wright",
        "Neemias Queta",
        "Isaiah Jackson",
        "Day'Ron Sharpe"
      ) %>% rep(each = 3),
      shot_type = c("Dunks + Lays", "Hooks + Floats", "Jumpers") %>% rep(8)
    ) %>%
      mutate(
        shot_type = factor(shot_type, levels = c("Jumpers", "Hooks + Floats", "Dunks + Lays")),
        shot_mix = c(
          40, 26, 33,
          48, 10, 42,
          50, 19, 31,
          50, 15, 35,
          51, 25, 25,
          55, 27, 18,
          60, 15, 25,
          66, 24, 10
        )
      ),
    by = "player"
  )

basic_tb <- player_df %>%
  group_by(player) %>%
  summarize(dunks = shot_mix[1], list_data = list(shot_mix)) %>%
  arrange(dunks) %>%
  gt()

basic_tb %>%
  gt_plt_bar_stack(list_data, width = 65,
                   labels = c("DUNKS", "HOOKS/FLOATS", "JUMPERS"),
                   palette= c("#ff4343", "#bfbfbf", "#0a1c2b")) %>%
  gt_theme_538() %>% 
  gtsave(here::here("tables/publication-quality.html"))


# data-load ---------------------------------------------------------------

url_in <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv'

raw_yields <- readr::read_csv(url_in)

yield_data <- raw_yields %>% 
  janitor::clean_names() %>% 
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>% 
  select(entity:beans, -code) %>% 
  pivot_longer(cols = wheat:beans, names_to = "crop", values_to = "yield") %>% 
  rename(Country = entity)

country_sel <- c(
  "China", "India", "United States", 
  "Indonesia", "Mexico", "Pakistan"
)

yield_data_wide <- raw_yields %>% 
  janitor::clean_names() %>% 
  rename_with(
    ~str_remove(., "_tonnes_per_hectare")
  ) %>% 
  select(entity:beans, -code) %>% 
  pivot_longer(
    cols = wheat:beans, 
    names_to = "crop", 
    values_to = "yield"
  ) %>% 
  rename(Country = entity) %>% 
  filter(
    crop %in% c("potatoes", "maize"), 
    year %in% c(2014:2016),
    Country %in% country_sel
  ) %>% 
  pivot_wider(
    names_from = year, 
    values_from = yield
  )


# basic-table -------------------------------------------------------------

yield_data_wide %>% 
  gt() %>% 
  gtsave(here::here("tables/basic-table.html"))


# add-groups --------------------------------------------------------------

yield_data_wide %>% 
  head() %>% 
  # respects grouping from dplyr
  group_by(Country) %>%  #<<
  gt(rowname_col = "crop") %>% 
  gtsave(here::here("tables/group-tab.html"))


# group-sum ---------------------------------------------------------------

yield_data_wide %>% 
  mutate(crop = str_to_title(crop)) %>% 
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  fmt_number(
    columns = 2:5, # reference cols by pos
    decimals = 2 # decrease decimal places
  ) %>% 
  summary_rows( #<<
    groups = TRUE, #<<
    # reference cols by name
    columns = c(`2014`, `2015`, `2016`), 
    fns = list(
      # add summary stats
      avg = ~mean(.),  #<<
      sd = ~sd(.) #<<
    )
  ) %>% 
  gtsave(here::here("tables/groupSum.html"))


# add-spanners ------------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_spanner( #<<
    label = "Yield in Tonnes/Hectare", #<<
    columns = 2:5 #<<
  ) %>% 
  gtsave(here::here("tables/tab-spanner.html"))


# notes-titles ------------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_footnote(
    footnote = "Yield in Tonnes/Hectare", 
    locations = cells_column_labels( #<<
      columns = 1:3
    )
  ) %>% 
  gtsave(here::here("tables/notes-titles.html"))


# notes-source ------------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_footnote(
    footnote = "Yield in Tonnes/Hectare", 
    locations = cells_column_labels(
      columns = 1:3 # note
    )
  ) %>% 
  # Adding a `source_note()`
  tab_source_note(
    source_note = "Data: OurWorldInData" #<<
  ) %>% 
  gtsave(here::here("tables/notes-source.html"))


# title-subtitle ----------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header( #<<
    title = md("**Crop Yields between 2014 and 2016**"),
    subtitle = html("<em>Countries limited to Asia</em>")
  ) %>% 
  gtsave(here::here("tables/title-subtitle.html"))


# adjust-appearance -------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = "Crop Yields between 2014 and 2016",
    subtitle = "Countries limited to Asia"
  ) %>% 
  tab_options( #<<
    heading.subtitle.font.size = 12,
    heading.align = "left",
    table.border.top.color = "red",
    column_labels.border.bottom.color = "red",
    column_labels.border.bottom.width= px(3)
  ) %>% 
  gtsave(here::here("tables/adjust-appearance.html"))


# pseudo-themes -----------------------------------------------------------

my_theme <- function(data) {
  tab_options(
    data = data,
    heading.subtitle.font.size = 12,
    heading.align = "left",
    table.border.top.color = "red",
    column_labels.border.bottom.color = "red",
    column_labels.border.bottom.width= px(3)
  )
}
yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = "Crop Yields between 2014 and 2016",
    subtitle = "Countries limited to Asia"
  ) %>% 
  my_theme() %>% #<<
  gtsave(here::here("tables/theme-tab.html"))

# tab-style ---------------------------------------------------------------

yield_data_wide %>% 
  head() %>%
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style( #<<
    style = list(
      cell_fill(color = "black", alpha = 0.2), #<<
      cell_borders( #<<
        side = c("left", "right"), 
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = crop
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "red", style = "italic") #<<
    ),
    locations = cells_body(
      columns = 3:5,
      rows = Country == "China" #<<
    )
  ) %>% 
  gtsave(here::here("tables/tab-style.html"))

# color-gradient ----------------------------------------------------------

my_pal <- scales::col_numeric(
  paletteer::paletteer_d(
    palette = "ggsci::red_material"
  ) %>% as.character(),
  domain = NULL
)

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  data_color( #<<
    columns = c(`2014`, `2015`, `2016`),
    colors = my_pal #<<
  ) %>% 
  gtsave(here::here("tables/color-gradient.html"))


# rule-1-data -------------------------------------------------------------

# data prep
potato_data <- yield_data %>% 
  filter(Country %in% country_sel, crop == "potatoes", year %in% c(2013:2016)) %>% 
  filter(crop == "potatoes") %>% 
  pivot_wider(names_from = year, values_from = "yield")

potato_data


# rule1-poor --------------------------------------------------------------

potato_tb <- potato_data %>% 
  gt() %>% 
  cols_hide(c(crop)) %>% 
  opt_table_lines(extent = "none") %>% 
  fmt_number(
    columns = 3:6,
    decimals = 2
  ) 

potato_tb %>% 
  gtsave(here::here("tables/potato-tab.html"))


# rule1-good --------------------------------------------------------------

hot_potato <- potato_tb %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% tab_source_note(
    md(
      "**Table**: @thomas_mock |
       **Data**: OurWorldInData.org
      <br>**Inspiration**: @jschwabish"
    )
  ) 

hot_potato %>% 
  gtsave(here::here("tables/hot-potato.html"))

# rule2-data --------------------------------------------------------------

rule2_data <- yield_data %>% 
  filter(Country %in% country_sel, crop == "potatoes", year %in% c(2007:2016)) %>% 
  filter(crop == "potatoes") %>% 
  select(-crop) %>% 
  pivot_wider(names_from = year, values_from = "yield") %>% 
  rowwise() %>% 
  mutate(
    avg_07_11 = mean(`2007`:`2011`),
    .before = `2012`
  ) %>% 
  mutate(
    avg_12_16 = mean(`2012`:`2016`)
  ) %>% 
  ungroup()


# rule2-bad ---------------------------------------------------------------

rule2_tab1 <- rule2_data %>% 
  gt(
    rowname_col = "Country"
  ) %>% 
  cols_label(
    avg_07_11 = "Avg.",
    avg_12_16 = "Avg."
  ) %>% 
  cols_width(
    1 ~ px(125)
  ) %>% 
  fmt_number(
    columns = 2:last_col()
  ) %>% 
  tab_style(
    style = cell_borders(
      side = "all",
      color = "grey",
      weight = px(1),
      style = "solid"
    ),
    locations = list(
      cells_body(
        everything()
      ),
      cells_column_labels(
        everything()
      )
    )
  ) %>% 
  grand_summary_rows(
    columns = 2:last_col(),
    fns = list(
      "Average" = ~mean(.)
    ),
    formatter = fmt_number
  ) %>% 
  gtsave(here::here("tables/rule2-bad.html"))

# rule2-good --------------------------------------------------------------

rule2_tab2 <- rule2_data %>% 
  add_row(
    rule2_data %>% 
      summarize(
        across(where(is.double), 
               list(Average = mean),
               .names = "{col}")
      ) %>% 
      mutate(Country = "Average")
  ) %>% 
  gt() %>% 
  cols_label(
    avg_07_11 = "Avg.",
    avg_12_16 = "Avg."
  ) %>%
  fmt_number(
    columns = 2:last_col()
  ) %>% 
  tab_style(
    style = cell_fill(
      color = "lightgrey"
    ),
    locations = list(
      cells_body(
        columns = c(avg_07_11, avg_12_16)
      ),
      cells_column_labels(
        columns = c(avg_07_11, avg_12_16)
      )
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      columns = everything(),
      rows = Country == "Average"
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black"
  ) %>% 
  gtsave(here::here("tables/rule2-good.html"))

# rule3-data --------------------------------------------------------------

# Prep data
rule3_data <- yield_data %>% 
  filter(Country == "United States", year %in% c(2016)) %>% 
  mutate(crop = str_to_title(crop)) %>% 
  pivot_wider(names_from = year, values_from = "yield") %>% 
  arrange(crop) %>% 
  select(-Country, Crop = crop)


# compare-align -----------------------------------------------------------

rule3_align <- rule3_data %>% 
  mutate(`Center align` = `2016`,
         `Right align` = `2016`) %>%
  rename(`Left align` = 2) %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  fmt_number(
    columns = 2:4
  ) %>% 
  cols_align(align = "left",
             columns = 2) %>% 
  cols_align(align = "center",
             columns = 3) %>% 
  cols_align(align = "right",
             columns = 4) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% gtsave(here::here("tables/rule3-align.html"))


# rule3-add-bad ----------------------------------------------------------

rule3_data_addendum <- yield_data %>% 
  filter(
    Country %in% c("Africa"), 
    year >= 2015,
    str_length(crop) == 5
  ) %>%
  group_by(year) %>% 
  mutate(
    crop = str_to_title(crop),
    max_yield = max(yield),
    `Top Crop` = if_else(yield == max_yield, "Y", "N")
  ) %>%
  select(Year = year, Crop = crop, `Top Crop`, Yield = yield) %>% 
  ungroup() %>% 
  head(n = 4) 

rule3_data_addendum %>% 
  gt() %>% 
  gtsave(here::here("tables/rule3-add-bad.html"))

# rule3-add-good ----------------------------------------------------------

rule3_data_addendum %>% 
  gt() %>% 
  gt::cols_align(
    align = "center",
    columns = c(`Top Crop`, Crop)
  ) %>% 
  gtsave(here::here("tables/rule3-add-good.html"))


# choose-fonts ------------------------------------------------------------


tab_font_fct <- function(data, font, column){
  tab_style(
    data = data,
    style = list(
      cell_text(font = font, decorate = "underline")
    ),
    locations = list(
      cells_column_labels(
        c({{column}})
      ),
      cells_body(
        c({{column}})
      )
    )
  )
}

rule3_text <- rule3_data %>% 
  head(n = 4) %>% 
  mutate(Karla = `2016`,
         Cabin = `2016`,
         Georgia = `2016`,
         `Fira Mono` = `2016`) %>%
  rename(Default = 2) %>% 
  gt() %>% 
  tab_font_fct("Default", Default) %>% 
  tab_font_fct("Karla", Karla) %>% 
  tab_font_fct("Cabin", Cabin) %>% 
  tab_font_fct("Georgia", Georgia) %>% 
  tab_font_fct("Fira Mono", `Fira Mono`) %>% 
  fmt_number(columns = 2:6) %>% 
  tab_spanner("Good", c(2,6)) %>% 
  tab_spanner("Bad", 3:5) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  )

rule3_text %>% gtsave(here::here("tables/font-tab.html"))


# rule4 -------------------------------------------------------------------

basic_theme <- function(data) {
  tab_options(
    data,
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.font.weight = "bold",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    data_row.padding = px(3)
  ) 
}

country_names <- c(
  "British Virgin Islands", "Cayman Islands", 
  "Democratic Republic of Congo", "Luxembourg", 
  "United States","Germany", "New Zealand", "Costa Rica", "Peru"
)
rule4_tab_left <- tibble(
  right = country_names,
  center = country_names,
  left = country_names
) %>% 
  gt()  %>% 
  cols_align(align = "left",
             columns = 3) %>% 
  cols_align(align = "center",
             columns = 2) %>% 
  cols_align(align = "right",
             columns = 1) %>% 
  cols_width(
    everything() ~ px(250)
  ) %>% 
  cols_label(
    right = md("Right aligned and<br>hard to read"),
    center = md("Centered and<br>even harder to read"),
    left = md("Left-aligned and<br>easiest to read")
  ) %>% 
  basic_theme() %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))

rule4_tab_left %>% 
  gtsave(here::here("tables/rule4-tab.html"))


# rule5 -------------------------------------------------------------------

rule5_tab <- yield_data %>% 
  filter(
    Country %in% country_sel, 
    crop == "potatoes", 
    year %in% c(2016)
  ) %>% 
  select(Country, yield) %>% 
  mutate(few = yield, right = yield) %>% 
  gt() %>% 
  fmt_number(
    columns = c(few),
    decimals = 0
  ) %>% 
  fmt_number(
    columns = c(right),
    decimals = 1
  ) %>% 
  cols_label(
    yield = md("Too many<br>decimals"),
    few = md("Too few<br>decimals"),
    right = md("About<br>right")
  ) %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))

rule5_tab %>% 
  gtsave(here::here("tables/rule5-tab.html"))


# rule6-data --------------------------------------------------------------

rule6_data <- yield_data %>% 
  filter(
    Country %in% country_sel, 
    crop == "potatoes", 
    year %in% c(2014:2016)
  ) %>% 
  filter(crop == "potatoes") %>% 
  pivot_wider(
    names_from = year, 
    values_from = "yield"
  ) %>% 
  select(-crop)


# rule6-tab ---------------------------------------------------------------

rule6_tb <- rule6_data %>% 
  arrange(desc(`2014`)) %>% 
  add_row(
    rule6_data %>% 
      summarize(across(where(is.double), list(Average = mean),
                       .names = "{col}")
      ) %>% mutate(Country = "Average")
  ) %>% 
  gt() %>% 
  fmt_number(columns = 2:4,
             decimals = 2) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      columns = everything(),
      rows = Country == "Average"
    )
  ) %>% 
  cols_width(c(Country) ~ px(125),
             2:4 ~ px(75)) %>% 
  basic_theme()

rule6_tb %>% 
  gtsave(here::here("tables/rule6-tall.html"))


# rule6-tab2 --------------------------------------------------------------

rule6_wide <- rule6_tb %>% 
  cols_width(c(Country) ~ px(125),
             2:4 ~ px(55)) %>% 
  tab_options(data_row.padding = px(10),
              table_body.hlines.color = "lightgrey") 

rule6_wide %>% 
  gtsave(here::here("tables/rule6-wide.html"))

# rule7 -------------------------------------------------------------------

rule6_tb %>% 
  fmt_percent(2:4, scale_values = FALSE) %>% 
  gtsave(here::here("tables/rep-units.html"))

rule6_tb %>% 
  gtExtras::fmt_symbol_first(column = 2:4, symbol = "%", last_row_n = 7) %>% 
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(2:4)) %>% 
  gtsave(here::here("tables/units.html"))


# rule8-data --------------------------------------------------------------

rule8_data <- yield_data %>% 
  filter(
    Country %in% country_sel, 
    crop == "potatoes", 
    year %in% 2009:2017
  ) %>% 
  group_by(Country) %>% 
  mutate(pct_change = (yield/lag(yield)-1)*100) %>% 
  ungroup() %>% 
  filter(between(year, 2010, 2016)) %>% 
  select(Country, year, pct_change) %>% 
  pivot_wider(names_from = year, values_from = pct_change)

# rule8-plain -------------------------------------------------------------

rule8_tb <- rule8_data %>% 
  gt() %>% 
  fmt_number(2:last_col()) %>% 
  cols_label(
    Country = ""
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  basic_theme() %>% 
  cols_width(c(Country) ~ px(125),
             2:last_col() ~ px(75))

rule8_tb %>% gtsave(here::here("tables/highlight-out.html"))

# rule8-color -------------------------------------------------------------

body_fct <- function(col, row){
  cells_body(
    columns = col,
    rows = {{row}} < 0
  )
}

rule8_color <- rule8_tb %>% 
  tab_style(
    style = cell_text(color = "red"),
    locations = list(
      body_fct(2, `2010`),
      body_fct(3, `2011`),
      body_fct(4, `2012`),
      body_fct(5, `2013`),
      body_fct(6, `2014`),
      body_fct(7, `2015`),
      body_fct(8, `2016`)
    )
  ) 

rule8_color %>% 
  gtsave(here::here("tables/rule8-color.html"))

# rule8-fill --------------------------------------------------------------

rule8_fill <- rule8_tb %>% 
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("red", 0.7)),
      cell_text(color = "white", weight = "bold")
    ),
    locations = list(
      body_fct(2, `2010`),
      body_fct(3, `2011`),
      body_fct(4, `2012`),
      body_fct(5, `2013`),
      body_fct(6, `2014`),
      body_fct(7, `2015`),
      body_fct(8, `2016`)
    )
  ) %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))
gtsave(rule8_fill, here::here("tables/rule8-fill.html"))

# rule9-data --------------------------------------------------------------

rule9_data <- yield_data %>% 
  filter(Country %in% country_sel[-5], year %in% c(2015, 2016),
         crop %in% c("wheat", "potatoes", "rice", "soybeans"),
         !is.na(yield)) %>% 
  pivot_wider(names_from = year, values_from = yield) %>% 
  rowwise() %>% 
  mutate(crop = str_to_title(crop),
         pct_change = (`2016`/`2015`-1)*100) %>%
  group_by(Country) %>% 
  arrange(desc(`2015`)) %>% 
  ungroup() 

# rule9-bad ---------------------------------------------------------------

rule9_bad <- rule9_data %>% 
  gt() %>% 
  fmt_number(
    columns = c(`2015`, `2016`, pct_change)
  ) %>% 
  tab_spanner(
    columns = c(`2015`, `2016`),
    label = md("**Yield in<br>Tonnes/Hectare**")
  ) %>%  
  cols_width(
    crop ~ px(125),
    c(`2015`, `2016`, pct_change) ~ 100
  ) 

gtsave(rule9_bad, here::here("tables/rule9-bad.html"))


# rule9-good --------------------------------------------------------------

rule9_grp <- rule9_data %>% 
  gt(groupname_col = "Country") %>% 
  tab_stubhead("label") %>% 
  tab_options(
    table.width = px(300)
  ) %>% 
  cols_label(
    crop = "",
    pct_change = md("Percent<br>Change")
  ) %>% 
  fmt_number(
    columns = c(`2015`, `2016`, pct_change)
  ) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  tab_spanner(
    columns = c(`2015`, `2016`),
    label = md("**Yield in Tonnes/Hectare**")
  ) %>%  
  cols_width(
    crop ~ px(125),
    c(`2015`, `2016`, pct_change) ~ 100
  ) %>% 
  basic_theme()

rule9_grp %>% 
  gtsave(here::here("tables/rule9-grp.html"))


# rule10-data -------------------------------------------------------------

country_sel <- c(
  "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
  "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
)

rule10_data <- yield_data %>% 
  filter(
    year %in% c(2013,2017), 
    crop == "potatoes", 
    Country %in% country_sel
  ) %>% 
  pivot_wider(names_from = year, values_from = yield)

rule10_data


# small-yield-data --------------------------------------------------------

small_yield <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% country_sel
  ) 

split_yield <- split(small_yield$yield, small_yield$Country)


# sparklines --------------------------------------------------------------

small_yield %>% 
  group_by(Country) %>% 
  summarise(`2013` = yield[year == 2013],
            `2017` = yield[year == 2017],
            Trend = list(yield)) %>% 
  gt() %>% 
  tab_spanner(html("Potato Yield in<br>Tonnes/Hectare"), 2:4) %>% 
  gtExtras::gt_sparkline(Trend) %>% 
  gtExtras::gt_theme_guardian() %>% 
  gtsave(here::here("tables/spark-tab.html"))


# density-plot ------------------------------------------------------------

fake_df <- gtExtras::generate_df(
  n = 100, n_grps = 5, 
  mean = sample(10:50, size = 5),
  sd = sample(5:20, size = 5), 
  with_seed = 37)

fake_df %>% 
  group_by(grp) %>% 
  summarise(mean = mean(values),
            data = list(values)) %>%  #<<
  arrange(desc(mean)) %>% 
  gt() %>% 
  gtExtras::gt_sparkline(column = data, #<<
                         type = "density") %>%  #<<
  gtsave(here::here("tables/gt-density.html"))


# bar-chart ---------------------------------------------------------------

bar_yields <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% c(
      country_sel, "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
      "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
    )
  ) %>% 
  pivot_wider(names_from = year, values_from = yield) %>%  
  select(-crop) %>% 
  rowwise() %>% 
  mutate(
    mean = mean(c(`2013`, `2014`, `2015`, `2016`, `2017`))
  ) %>% 
  ungroup() %>% 
  select(Country, `2013`, `2017`, `mean`) %>% 
  arrange(desc(mean))

gt(bar_yields) %>% 
  gtExtras::gt_duplicate_column(
    mean, dupe_name = "mean_plot") %>% 
  gtExtras::gt_plt_bar(mean_plot, color = "lightblue",
                       scale_type = "number") %>% 
  gtExtras::gt_theme_nytimes() %>% 
  cols_label(
    mean = html("Average<br>2013-17"),
    mean_plot = ""
  ) %>% 
  gtsave(here::here("tables/bar-plot.html"))


# heatmap -----------------------------------------------------------------

rule10_wide <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% c(
      country_sel, "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
      "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
    )
  ) %>% 
  pivot_wider(names_from = year, values_from = yield) %>% 
  arrange(desc(`2013`)) %>% 
  select(-crop)

rule10_wide %>% 
  gt() %>% 
  data_color(
    columns = 2:6, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  fmt_number(2:6) %>% 
  tab_spanner(
    label = "Potato Yield in Tonnes/Hectare",
    columns = c(2:6)
  ) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )
  ) %>%  
  cols_width(
    1 ~ px(125),
    2:6 ~ px(65)
  ) %>% 
  basic_theme() %>% 
  gtsave(here::here("tables/heatmap.html"))

# conf-interval -----------------------------------------------------------

# gtExtras can calculate basic conf int
# using confint() function

ci_table <- generate_df(n = 100, n_grps = 3,
                        mean = c(10, 15, 20), sd = c(22, 35, 27),
                        with_seed = 37) %>%
  dplyr::group_by(grp) %>%
  dplyr::summarise(n = dplyr::n(),
                   avg = mean(values),
                   sd = sd(values),
                   list_data = list(values)) %>%
  gt::gt() %>%
  gt::fmt_number(avg:sd, decimals = 1) %>% 
  gt_plt_conf_int(list_data, ci = 0.9)

ci_table %>% 
  gtsave(here::here("tables/ci-table.html"))


# bullet-chart ------------------------------------------------------------

set.seed(37)
bullet_tab <- tibble::rownames_to_column(mtcars) %>%
  dplyr::select(rowname, cyl:drat, mpg) %>%
  dplyr::group_by(cyl) %>%
  dplyr::mutate(target_col = mean(mpg)) %>%
  dplyr::slice_sample(n = 3) %>%
  dplyr::ungroup() %>%
  gt::gt() %>%
  gt_plt_bullet(column = mpg, target = target_col, width = 45,
                colors = c("lightblue", "black")) %>%
  gt_theme_538()

bullet_tab %>% 
  gtsave(here::here("tables/gt-bullet.html"))

# german-table ------------------------------------------------------------

library(gt)
library(gtExtras)
library(dplyr)
library(htmltools)

# original source: https://www.bloomberg.com/graphics/2021-german-election-results/

party_df <- tibble(
  Party = c("SPD", "CDU/CSU", "Greens", "FDP", "AfD", "Left", "Other"),
  Seats = c(206, 196, 118, 92, 83, 39, 1),
  `% of 2nd Votes` = c(25.7, 24.1, 14.8, 11.5, 10.3, 4.9, 8.7)
)

minimal_table <- gt(party_df) %>% 
  gt_plt_dot(column = Seats, category_column = Party,  max_value = 379,
             palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>% 
  gtExtras::gt_theme_nytimes() %>% 
  tab_header(title = "Results by Party in the Bundestag Election",
             subtitle = "Seats and votes are based on provisional official results.") %>% 
  cols_width(Party ~ px(368), 3 ~ px(30))

party_table <- gt(party_df) %>% 
  gt_plt_dot(column = Seats, category_column = Party,  max_value = 368,
             palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>% 
  gtExtras::gt_theme_nytimes() %>% 
  tab_header(title = "Results by Party in the Bundestag Election",
             subtitle = "Seats and votes are based on provisional official results.") %>% 
  cols_width(Party ~ px(300), 3 ~ px(30)) %>% 
  tab_style(style = list(cell_text(color = "grey"),cell_borders(color = "white")),
            locations = cells_body(3)) %>% 
  tab_source_note(
    html(
      paste0(
        "With a total of 735 seats<br>",
        "<span style='color:#bfbfbf;'>Data as of: Sept 26, 2021, 11:09PM CDT</span>"
      )
    )
  ) %>% 
  tab_style(style = cell_borders("right", "lightgrey", "dashed"),
            cells_body(Party)) %>% 
  tab_style(style = cell_borders("top", "white"), cells_body(rows = 1)) %>% 
  tab_options(table.border.bottom.color = "white")

combo_table <- htmltools::div(
  party_table,
  htmltools::div(
    "368 seats for majority",
    style = paste0(
      htmltools::css(
        background= "white", font.size = px(11), width = px(60),
        font.family = "arial", display = "flex", text.align = "center",
        color = "#999", position = "fixed", top = "223px", left = "560px"
      )
    )
  )
) 

combo_table %>% 
  htmltools::save_html(here::here("tables/german-election.html"))

