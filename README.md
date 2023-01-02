#US NorthEastern City Imperviousness Analysis

##Dataset Sources
1. https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_DAYMET_V4#bands
2. https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_DAYMET_V4

##Dataset Description
The data is 2000--2021, 6.01--8.31, a total of 2024 days.
Each observation in a has a valid unique CODE as a primary key. CODE for each observation is constant in different files.
For detailed variable description, please refer to the data source

##Project Workflow
1. Using ArcGis to create the fishnet(1*1 km resolution) of six US NorthEastern cities  
2. Extracted Dataset by using Google Earth Engine(JavaScript) with the fishnet
3. Analysis the data with R and create visualization
4. Calculate the WBGT related variable in R

##User Guide
1. Download the fishnet file and connected to Google Earth Engine to extract the data
2. Using R to run the code
