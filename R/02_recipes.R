
# Set parameters, including pre-calculated breakfast numbers, and macro targets per meal
min_rating <- 3
dani_min <- 0.75
snack_cals <- (15 + 17) * 4 + 6 * 9 + (28 + 5) * 4 # One fulfil bar, one kvarg
breakfast_cals <- 4 * 40 + 9 * 17 + 4 * 165
n_meals <- 2
total_cal <- (3100 - breakfast_cals - snack_cals)/n_meals
targets <- list(protein=(150 - 40 - 30)/n_meals,
                fat = (70 - 17)/n_meals,
                carb = (470 - 165 - 33)/n_meals)

# ---- Run setup functions ----

# This loads the data and does a bunch of processing and meal creation using the parameters above
source('R/functions.R')

# Get valid meals
valid_fish <- get_valid_meals(fish_meals, selected_fish, recipe_df, targets)
valid_veg<- get_valid_meals(veg_meals, selected_veg, recipe_df, targets)

# ---- Meal building ----

# Choose a meal type: fish or veg
mealtype = "fish"

# Generate meal
selected_meal <- create_meal(mealtype, valid_fish, valid_veg)

# Check macros and view meal
selected_meal
selected_meal %>% dplyr::select(calories:carbs) %>% colSums(na.rm = T)
targets

# ---- Look at all the options together ----

fish_meals <- get_all_meals(valid_meal_list = valid_fish)
veg_meals <- get_all_meals(valid_meal_list = valid_veg)

# Look at some of the most likely options
fish_meals %>%
  mutate(main_url = paste0('https://www.epicurious.com/search/', str_replace_all(main_title, " ", "%20")),
         side_url = paste0('https://www.epicurious.com/search/', str_replace_all(side_title, " ", "%20"))) %>%
  mutate(mealtype = "fish") %>%
  bind_rows(mutate(veg_meals, mealtype = "veg")) %>%
  arrange(main_portions, desc(main_dani_portions), side_portions) %>% View

