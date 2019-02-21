# Functional-ANOVA-for-Plant-Growth-Dynamics
This project aims to analyse plant growth dynamics by using functinoal analysis. The data are some plant images and therefore, we have several functions and corresponding pipeline to pre-process these images including: To find the region of interest,  segment the plant from the image and binarize it into black and white. Then we do mophorlogical operations on the segmented image and summarize the phenotypic parameters from these images.

Segmentation methods including thresholding method, k-means method and Hidden Markov Random Field and EM algorithm(HMRF-EM) method. File "Input" is the demo of running the HMRF_EM framework, and this file needs to source another three files: "EM", "k-mean" and "MAP".

Functional ANOVA model are written in the file: fanova.
