# Libraries for the data selector tool
library(shiny) # duh
library(shinyjs) # I forget why I needed this, and maybe don't anymore
library(shinyWidgets) # for pickerInput, which comes with select/deselect all buttons. Very useful!
library(here) # for file path names
library(DT) # for fancy and interactive data table to display in the app
library(zip) # for zipping data and parameters for download
library(tidyverse) # because I love the tidyverse and want those functions handy
library(dplyr) # pipes etc, and because I was having some namespace conflicts with select
library(DBI) # dependency for RSQLite
library(RSQLite) # for loading the database

AGEBIN <- function(age) {
  if (18 <= age && age <= 30) {
    return("18-30")
  }
  if (31 <= age && age <= 40) {
    return ("31-40")
  }
  if (41 <= age && age <= 50) {
    return ("41-50")
  }
  if (51 <= age && age <= 60) {
    return ("51-60")
  }
  if (61 <= age && age <= 70) {
    return ("61-70")
  }
  if (71 <= age && age <= 80) {
    return ("71-80")
  }
  if (81 <= age && age <= 90) {
    return ("81-90")
  }
  if (91 <= age && age <= 100) {
    return ("91-110")
  }
  return("Invalid age")
}