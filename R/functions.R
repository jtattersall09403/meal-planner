
library(data.table)
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)


# ---- Functions ----

suggest_sides <- function(recipe_id, recipe_df, recipe_df_raw, targets, veggie = TRUE) {
  
  # Filter options to veggie if required
  if (veggie) {
    side_options = filter(recipe_df_raw, vegetarian == 1)
  } else {
    side_options = recipe_df_raw
  }
  
  # Check nutrients remaining
  remaining <- check_remaining(recipe_id, recipe_df, targets)
  
  # Find recipes meeting those requirements
  results = side_options %>%
    mutate(portions_required = remaining$carbs/carbs) %>%
    mutate_at(vars(calories, protein, fat, carbs), function(x) x * .$portions_required) %>%
    filter(fat <= remaining$fat * 1.1,
           fat >= remaining$fat * 0.9,
           protein <= 20,
           portions_required <= 2,
           portions_required > 0.5) %>%
    dplyr::select(id, title, rating, calories, protein, fat, carbs, portions_required)
  
  # Return
  results %>%
    arrange(portions_required) %>%
    dplyr::slice(1:10)
  
}

check_remaining <- function(recipe_id, recipes_filtered, targets) {
  
  # Get main nutritional info
  nutrients = recipes_filtered %>% 
    filter(id == recipe_id) %>%
    dplyr::select(protein, fat, carbs)
  
  # Calculate necessary remaining
  remaining = unlist(targets) - nutrients
  
  # Return
  remaining
}

get_valid_meals <- function(all_meals, selected_df, recipe_df, targets) {
  
  # Filter to those with valid sides, or which already hit all the requirements
  # By 'valid sides', we mean meals where the main + the side hits the targets
  valid_sides <- sapply(all_meals, function(x) nrow(x) > 0)
  already_met <- sapply(selected_df$id, function(recipe_id) {
    remaining = check_remaining(recipe_id, recipes_filtered=recipe_df, targets=targets)
    all(remaining/targets < 0.05)
  })
  
  # Show them
  valid_mains <- selected_df[valid_sides | already_met,] %>%
    dplyr::select(id, title, rating, calories, protein, fat, carbs, portions_required, dani_portion)
  valid_meals <- all_meals[valid_sides | already_met]
  
  # Return
  list(valid_mains=valid_mains,
       valid_meals=valid_meals)
}

create_meal <- function(mealtype = "veg", valid_fish, valid_veg) {
  
  if (mealtype == "veg") {
    valid_mains <- valid_veg$valid_mains
    valid_meals <- valid_veg$valid_meals
  } else {
    valid_mains <- valid_fish$valid_mains
    valid_meals <- valid_fish$valid_meals
  }
  
  # Randomly choose a meal
  rand_meal <- sample(1:length(valid_mains), 1)
  selected_meal <- bind_rows(valid_mains[rand_meal,],
                             valid_meals[[rand_meal]][sample(1:nrow(valid_meals[[rand_meal]]), 1),])
  
  # Return
  selected_meal
  
}

get_all_meals <- function(valid_meal_list) {
  
  lapply(1:length(valid_meal_list$valid_meals), function(i) {
    
    # Get main course values
    dummy_main <- lapply(1:nrow(valid_meal_list$valid_meals[[i]]), function(x) valid_meal_list$valid_mains[i,]) %>%
      bind_rows %>%
      mutate(group_id = row_number())
    
    # Combine with side/dessert
    valid_meal_list$valid_meals[[i]] %>%
      mutate(group_id = row_number()) %>%
      bind_rows(dummy_main) %>%
      group_by(group_id) %>%
      summarise_at(vars(calories, protein, fat, carbs), sum) %>%
      mutate(main_id = valid_meal_list$valid_mains$id[i],
             main_title = valid_meal_list$valid_mains$title[i],
             main_portions = valid_meal_list$valid_mains$portions_required[i],
             main_dani_portions = valid_meal_list$valid_mains$dani_portion[i],
             side_id = valid_meal_list$valid_meals[[i]]$id,
             side_title = valid_meal_list$valid_meals[[i]]$title,
             side_portions = valid_meal_list$valid_meals[[i]]$portions_required,
             dani_cals = main_dani_portions * valid_meal_list$valid_mains$calories[i]) 
  }) %>%
    bind_rows %>%
    select(main_title, side_title, dani_cals, calories:carbs, main_portions, main_dani_portions, side_portions,
           main_id, side_id) %>%
    group_by(main_title, side_title) %>%
    filter(row_number(side_id) == 1) %>%
    ungroup
  
}

# ---- Data processing ----

# Derive target ratios
target_ratios <- list(carb_protein = targets$carb/targets$protein,
                      protein_fat = targets$protein/targets$fat)

# Load recipes
recipe_df_raw <- fread('data/epi_r.csv') %>%
  mutate(id = row_number(title),
         p_cals = protein * 4,
         f_cals = fat * 9,
         c_cals = calories - p_cals - f_cals,
         carbs = c_cals/4)

# Calculate nutrition ratios
recipe_df <- recipe_df_raw %>%
  mutate(cp_ratio = carbs/protein,
         pf_ratio = protein/fat) %>%
  filter(protein < 200) %>%
  mutate(portions_required = targets$protein/protein,
         portions_required = ifelse(portions_required < 1, 1, portions_required),
         dani_portion = 350/calories,
         dani_portion = ifelse(dani_portion > 1, 1, dani_portion)) %>%
  dplyr::select(id, title:fat, carbs, cp_ratio, pf_ratio, p_cals, f_cals, c_cals,
                portions_required, everything()) %>%
  mutate_at(vars(calories, protein, fat, carbs), function(x) x * .$portions_required) %>%
  filter(! id %in% c(12294, 8799)) # Remove some clear errors

# Get vegetarian recipes
veg_recipes <- recipe_df %>%
  filter(vegetarian == 1)

# Get fish recipes
fish_recipes <- recipe_df %>%
  filter(pescatarian == 1)

# ---- Recipe selection ----

# Filter to those with similar quantities to your targets
selected_veg <- veg_recipes %>%
  filter(portions_required <= 2,
         rating >= min_rating,
         fat <= targets$fat,
         dani_portion >= dani_min)

# Filter to those with similar quantities to your targets
selected_fish <- fish_recipes %>%
  filter(portions_required <= 2,
         rating >= min_rating,
         fat <= targets$fat,
         dani_portion >= dani_min)

# Suggest sides/desserts, to top up the non-protein targets
fish_meals <- lapply(selected_fish$id, 
                     suggest_sides, 
                     recipe_df_raw = recipe_df_raw,
                     recipe_df = recipe_df, 
                     targets = targets)

veg_meals <- lapply(selected_veg$id, 
                    suggest_sides, 
                    recipe_df_raw = recipe_df_raw,
                    recipe_df = recipe_df, 
                    targets = targets)