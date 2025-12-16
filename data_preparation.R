library(dplyr)
library(tidyr)


#read data
data <- readxl::read_xlsx(path = "data/data_identity.xlsx",
                          sheet = "DESPLAZAMIENTO",
                          range =  "C1:V120")

#preparing data

dat <- as.data.frame(data) |>
  dplyr::mutate(
    month = lubridate::month(created_at),
    context = ifelse(`88_Motivacin_del_des` == "Alimentación", "Feeding", "Defense")
  )

cols_seguidores <- grep("Seguidor", names(dat), value = TRUE)

#creating table with names(IDs), sex and reproductive status
individuos_status <- c(
  "BUENO" = "M", 
  "OSO" = "M",
  "GANDHI" = "M",
  "GRU" = "M",
  "FELIPE" = "M",
  "ALHAMBRA" = "AFL",
  "MOMI" = "AFL",
  "PATILLAS" = "AFL",
  "LOCA" = "AFL",
  "CHINA" = "AFL",
  "OJER" = "AFC",
  "BUBBLE" = "AFC",
  "WOW" = "AFL", 
  "UVE" = "AFL", 
  "QUERIDA" = "AFD",
  "TERE" = "AFD",
  "BUENA" = "AFD",
  "CYNTHIA" = "AFC",
  "ABU" = "AFC",
  "EW" = "AFC",
  "MULLET" = "AFC",
  "DOS"= "AFL",
  "SOLA" = "AFC"
)

individuos_table <- data.frame(id = names(individuos_status), 
                               sex = ifelse(individuos_status == "M", "M", "F"), 
                               reprostatus = individuos_status)

#adding columns to data
dat <- dat |>
  dplyr::rowwise()  |>
  dplyr::mutate(
    leader_sex = individuos_table$sex[ match(`78_Leader`, individuos_table$id) ],
    leader_reprostatus = individuos_table$reprostatus[ match(`78_Leader`, individuos_table$id) ],
    follower_id = list(
      individuos_table$id[
        match(
          dplyr::c_across(
            dplyr::all_of(cols_seguidores)), individuos_table$id 
        )
      ]
    ),
    month = lubridate::month(created_at),
    context = ifelse(`88_Motivacin_del_des` == "Alimentación", "Feeding", "Defense")
  )  |>
  dplyr::ungroup()


#get leaders
leaders <- dat |>
  dplyr::group_by(ind = `78_Leader`, sex = leader_sex, repro_status = leader_reprostatus, context, month) |>
  dplyr::summarise(n_leader = dplyr::n(), .groups = "drop")

#get followers  
followers_long <- dat |>
  dplyr::select(`78_Leader`, leader_sex,leader_reprostatus, follower_id, context, month)  |>
  tidyr::unnest_longer(follower_id)  |>
  dplyr::mutate(follower_sex = individuos_table$sex[match(follower_id, individuos_table$id)],
                follower_reprostatus = individuos_table$reprostatus[match(follower_id, individuos_table$id)]) |>
  na.omit()

followers <- followers_long |>
  dplyr::group_by(ind = follower_id, follower_sex, follower_reprostatus, context, month) |>
  dplyr::summarise(n_follower = dplyr::n(), .groups = "drop") |>
  na.omit()


#join followers and leaders
ind_counts <- left_join(followers, leaders, by = c("ind", "context", "month")) |>
  mutate(
    n_leader   = ifelse(is.na(n_leader), 0, n_leader),
    n_follower = ifelse(is.na(n_follower), 0, n_follower),
    total = n_leader + n_follower,
    prop_leader = n_leader / total
  ) |>
  dplyr::transmute(
    ID = ind,
    sex = follower_sex,
    repro_status = follower_reprostatus,
    context,
    month,
    n_leader,
    n_follower
  )


#get progression order index
following <- data[,5:15]

# df_followers <- dat[,c(cols_seguidores, "context", "month")]  %>%
#   dplyr::mutate(row_id = row_number(),
#          n_followers = rowSums(!is.na(.)) - 2) %>%  # count non-NA per row
#   tidyr::pivot_longer(
#     cols = -c(row_id, n_followers, context, month),
#     names_to = "column_name",
#     values_to = "name"
#   ) %>%
#   dplyr::filter(!is.na(name)) %>%    # keep only non-NA names
#   dplyr::mutate(n_position = match(column_name, names(following))) %>%
#   dplyr::select(name, n_position, n_followers, context, month) %>%
#   dplyr::mutate(order_index =(((n_position-1)/2)/((n_followers-1)/2)),
#                 fb_position = ifelse(order_index <= 0.5, "Front", "Back")) %>%
#   dplyr::group_by(name,context, month) %>%
#   dplyr::summarise(mean_order_index = mean(order_index, na.rm = TRUE),
#             .groups = "drop")  %>%
#   dplyr::transmute(
#     ID = name,
#     month,
#     context,
#     mean_order_index
#   )

df_followers <- dat[,c(cols_seguidores, "context", "month")]  %>%
  mutate(row_id = row_number(),
         n_followers = rowSums(!is.na(.)) - 2) %>%  # count non-NA per row
  pivot_longer(
    cols = -c(row_id, n_followers, context, month),
    names_to = "column_name",
    values_to = "name"
  ) %>%
  filter(!is.na(name)) %>%    # keep only non-NA names
  mutate(n_position = match(column_name, names(following))) %>%
  dplyr::select(name, n_position, n_followers, context, month) %>%
  dplyr::mutate(order_index =(((n_position-1)/2)/((n_followers-1)/2)),
                fb_position = ifelse(order_index <= 0.5, "Front", "Back"))


mean_order_index <- df_followers %>%
  group_by(name, context, month) %>%
  summarise(
    mean_order_index = mean(order_index, na.rm = TRUE),
    .groups = "drop"
  )


n_times_df <- df_followers %>%
  group_by(name, fb_position, context, month) %>%
  summarise(
    n_times = n(),
    .groups = "drop"
  )

df_followers <- mean_order_index %>%
  left_join(n_times_df, by = c("name", "context", "month"))



df_followers <- df_followers %>%
  group_by(name, month, context, fb_position, mean_order_index) %>%
  summarise(n_times = sum(n_times),
            .groups = "drop") %>%  # sum in case multiple rows
  pivot_wider(
    names_from = fb_position,
    values_from = n_times,
    values_fill = 0  # fill missing combinations with 0
  ) %>%
  dplyr::mutate(mean_poi_position = ifelse(mean_order_index <= 0.5, "Front", "Back")) %>%
  dplyr::transmute(
    ID = name,
    month,
    context,
    n_front = Front,
    n_back = Back,
    mean_order_index,
    mean_poi_position
  )



#join with leader/follower table
data_ <- left_join(df_followers, ind_counts, by = c("ID", "context", "month")) |>
  dplyr::mutate(
    sex = individuos_table$sex[ match(ID, individuos_table$id) ],
    repro_status = individuos_table$reprostatus[match(ID, individuos_table$id) ],
    n_leader = ifelse(is.na(n_leader), 0, n_leader)
  ) |>
  dplyr::transmute(
    ID,
    sex,
    repro_status,
    context,
    month,
    n_leader,
    n_follower,
    n_front,
    n_back,
    mean_poi = mean_order_index,
    mean_poi_position
  )


write.csv(data_, "data/my_data.csv", row.names = FALSE)
            
