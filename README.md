# **Relict Forest Mapping**

Relict Forest Mapping is a remote sensing project aimed at identifying and classifying relict forest patches in the Pampa del Tamarugal, Atacama Desert (Chile). The study explores the spatial relationship between forest remnants and archaeological sites using satellite imagery and spatial analysis techniques.

## **Overview**

This project applies supervised classification methods on Sentinel-2 imagery (IR and SWIR indices) to detect forest remnants. It also integrates distance, visibility, and accessibility analyses to assess their relation to known archaeological sites in three study areas: Salar de Llamara, Pampa Iluga, and Salar Sur Viejo.

## **Contents**

data/: Training areas (ROIs) and classified rasters used in the analysis

scripts/: RStudo/QGIS processing scripts and model implementation

results/: Output maps, recall metrics, and spatial analysis results

docs/: Figures and supporting material for publication

## **Methods**

Supervised Classification: Multispectral classification using IR and SWIR bands.

Recall Analysis: Quantitative evaluation of model performance using known ROIs.

Distance & Visibility: Euclidean distance and viewshed analysis to understand spatial dynamics.

Forest-Archaeology Links: Interpretation of spatial patterns between forest patches and archaeological sites.

## **Requirements**

QGIS 3.28+

GDAL

RStudio (2023.06.0-421)

Plugins: Semi-Automatic Classification Plugin, Visibility Analysis

