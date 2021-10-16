library(dplyr)
library(data.table)
library(feather)
library(geosphere)
library(leaflet)
library(DT)
library(rgeos)
library(sf)
library(fasterize)
library(raster)
library(spData)
source(here::here('functions.R'))

test_that("CSV to feather works", {
  DatasetToFeather('sampleData')
  sampleFiles <- gsub("\\..*","",list.files(pattern = 'sample'))
  expect_equal(sampleFiles[1], sampleFiles[2])
})

test_that("A feather can be loaded", {
  data <- feather::read_feather('sampleData.feather')
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame") )
})

data <- feather::read_feather('sampleData.feather')

test_that("Check if the distance column is created, getDistance", {
 distTest <- getDistance(data, targetcolumns = c("LON", "LAT"))
 expect_equal( "DISTANCE" %in% colnames(distTest), TRUE )
})

test_that("Is distance numeric?", {
  distTest <- getDistance(data, targetcolumns = c("LON", "LAT"))
  expect_equal( class(distTest$DISTANCE), 'numeric'  )
})

test_that("Assess whether only two columns are being generated", {
  distTest <- summarizeDistance(data, ShipType = "Cargo", VesselName="KAROLI", linear = 'Yes')
  expect_equal( nrow(distTest), 2  )
})

test_that("Check if the distance column is created, summarizeDistance", {
  distTest <- summarizeDistance(data, ShipType = "Cargo", VesselName="KAROLI", linear = 'Yes')
  expect_true( "DISTANCE" %in% colnames(distTest) )
})


test_that("Assess whether only two columns are being generated, non-linear", {
  distTest <- summarizeDistance(data, ShipType = "Cargo", VesselName="KAROLI", linear = 'No')
  expect_equal( nrow(distTest), 2  )
})

test_that("Check if the distance column is created, summarizeDistance, non-linear", {
  distTest <- summarizeDistance(data, ShipType = "Cargo", VesselName="KAROLI", linear = 'No')
  expect_true( "DISTANCE" %in% colnames(distTest) )
})
