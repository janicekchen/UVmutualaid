library(dplyr)
library(googlesheets4)

# UPDATE each time finished wrangling:
load("current_row.Rdata")

# downloading Google sheet
url <- 'https://docs.google.com/spreadsheets/d/1K1rl7pZLmlEHS3NYyDiT1l0F2IgyNQ-aRZsdykKRKCs/edit#gid=1096930982'

responsesFull <- as.data.frame(read_sheet(url, sheet = "Form Responses"), stringsAsFactors = FALSE)

responses <- responsesFull %>% slice((current_row + 1):nrow(responsesFull))

current_row <- nrow(responsesFull)
save(current_row, file = "current_row.Rdata")

names(responses) <- c("time", "name", "phone", "email", "town", "address", "quarantine", "needs", "services", "other_assist", "p2p", "long_term","fin_cont", "notes")

class(responses$phone) <- "character"

responses <- responses %>% 
  mutate(volunteer_req = "")

responses[grep("White River Junction", responses$town), "town"] <- "WRJ"

for (i in 1:nrow(responses)) {
  if (((responses$other_assist != "No" | responses$needs[i] != "I do not require assistance.") && (responses$services[i] != "I cannot volunteer."))) {
    responses[i, "volunteer_req"] <- "Both"
  }
  else if (responses$other_assist != "No" || responses$needs[i] != "I do not require assistance.") {
    responses[i, "volunteer_req"] <- "Request support"
  }
  else {
    responses[i, "volunteer_req"] <- "Volunteer to support"
  }
}


vol_raw <- responses[responses$volunteer_req == "Both" | responses$volunteer_req == "Volunteer to support", ]
req_raw <- responses[responses$volunteer_req == "Both" | responses$volunteer_req == "Request support", ]

if (nrow(vol_raw) > 0) {
  vol_tidy <- data.frame("time" = vol_raw$time, 
                         "status_code" = "", 
                         "notes_org" = "", 
                         "vol_name" = "",
                         "name" = vol_raw$name, 
                         "email" = vol_raw$email, 
                         "phone" = vol_raw$phone,
                         "town" = vol_raw$town,
                         "address" = vol_raw$address,
                         "volunteer_req" = vol_raw$volunteer_req,
                         "quarantine" = vol_raw$quarantine,
                         "grocery" = "",
                         "food_donation" = "",
                         "med_exp" = "",
                         "transport" = "",
                         "meal_prep" = "",
                         "remote" = "",
                         "employment" = "",
                         "p2p" = vol_raw$p2p,
                         "p2p_notes" = "",
                         "long_term" = vol_raw$long_term,
                         "fin_cont" = vol_raw$fin_cont,
                         "notes" = vol_raw$notes,
                         "services" = vol_raw$services,
                         stringsAsFactors = FALSE)
  
  service_list <- c("Grocery / supply delivery", "Food donation", "Medical assistance", "Transportation", "Meal preparation", "Remote learning assistance", "Employment")
  
  
  for (i in 1:nrow(vol_tidy)) {
    for(j in 1:length(service_list)) {
      if (grepl(service_list[j], vol_tidy[i, "services"])) {
        vol_tidy[i, j + 11] <- "Y"
      }
      else {
        vol_tidy[i, j + 11] <- "N"
      }
    }
  }
  
  vol_tidy[grepl("I will contribute.", vol_tidy$fin_cont), "fin_cont"] <- "Can Probably Donate"
  vol_tidy[grepl("I cannot contribute, but will help share this link.", vol_tidy$fin_cont), "fin_cont"] <- "Amplify"
  vol_tidy[grepl("I am not able to help in this regard.", vol_tidy$fin_cont), "fin_cont"] <- "Cannot Help"
  
  vol_tidy[grepl("Yes", vol_tidy$p2p), "p2p"] <- "Y"
  vol_tidy[grepl("No", vol_tidy$p2p), "p2p"] <- "N"
  vol_tidy[grepl("Maybe", vol_tidy$p2p), "p2p"] <- "M"
  
  vol_tidy <- vol_tidy %>%
    select(-c("services"))

  sheet_append(url, vol_tidy, "Volunteers")
}

if (nrow(req_raw) > 0) {
  req_tidy <- data.frame("time" = req_raw$time, 
                         "status_code" = "Open", 
                         "vol_name" = "",
                         "notes_org" = "", 
                         "name" = req_raw$name, 
                         "email" = req_raw$email, 
                         "phone" = req_raw$phone,
                         "town" = req_raw$town,
                         "address" = req_raw$address,
                         "volunteer_req" = req_raw$volunteer_req,
                         "quarantine" = req_raw$quarantine,
                         "grocery" = "",
                         "food_donation" = "",
                         "med_att" = "",
                         "childcare" = "",
                         "shelter" = "",
                         "transport" = "",
                         "remote" = "",
                         "employment" = "",
                         "others_asst" = if_else(req_raw$other_assist != "No", req_raw$other_assist, "N"),
                         "p2p" = req_raw$p2p,
                         "long_term" = req_raw$long_term,
                         "fin_cont" = req_raw$fin_cont,
                         "notes" = req_raw$notes,
                         "needs" = req_raw$needs,
                         stringsAsFactors = FALSE)
  
  needs_list <- c("Grocery/supply delivery", "Food donation", "Medical attention", "Childcare", "Shelter", "Transportation", "Remote learning assistance", "Employment")
  
  for (i in 1:nrow(req_tidy)) {
    for(j in 1:length(needs_list)) {
      if (grepl(needs_list[j], req_tidy[i, "needs"])) {
        req_tidy[i, j + 11] <- "Y"
      }
      else {
        req_tidy[i, j + 11] <- "N"
      }
    }
  }
  
  req_tidy[grepl("I will contribute.", req_tidy$fin_cont), "fin_cont"] <- "Can Probably Donate"
  req_tidy[grepl("I cannot contribute, but will help share this link.", req_tidy$fin_cont), "fin_cont"] <- "Amplify"
  req_tidy[grepl("I am not able to help in this regard.", req_tidy$fin_cont), "fin_cont"] <- "Cannot Help"
  
  req_tidy[grepl("Yes", req_tidy$p2p), "p2p"] <- "Y"
  req_tidy[grepl("No", req_tidy$p2p), "p2p"] <- "N"
  req_tidy[grepl("Maybe", req_tidy$p2p), "p2p"] <- "M"
  
  req_tidy <- req_tidy %>%
    select(-c("needs"))
  
  sheet_append(url, req_tidy, "Request Assistance")
}


