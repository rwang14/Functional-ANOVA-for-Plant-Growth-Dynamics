# Functional-ANOVA-for-Plant-Growth-Dynamics
This project aims to analyse plant growth dynamics using functional analysis. The data are some plant images and therefore, we have several functions and corresponding pipeline to pre-process these images including: To find the region of interest,  segment the plant from the image and binarize it into black and white. Then we do mophorlogical operations on the segmented image and summarize the phenotypic parameters from these images. Image Processing needed to follow the precedure: "Visible.txt"-"Pipeline_Processing.R".  "Other Functions" is a file about some other functions such as finding the largest connection of a segmented plant, exchanging color of the background and the object of interest of an image, which may be helpful for image processing.

Segmentation methods including double-critierion thresholding method, k-means method and Hidden Markov Random Field and EM algorithm(HMRF-EM) method. File "Input" is the demo of running the HMRF_EM framework, and this file needs to source another three files: "EM", "kmeans" and "MAP".

Functional ANOVA model are written in the file: Fanova.R.
