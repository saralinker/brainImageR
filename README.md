---
output:
  html_document: default
  pdf_document: default
---

# brainImageR
BrainImageR is a ultility that plots gene enrichment over maps of the developing and adult human brain. Below you will find basic usage instructions. Please see the vignette for detailed information on running the package.

## Installation
`install.packages("devtools")`

`library(devtools)` 

`install_github("saralinker/brainImageR") `

*Please note that there is a lot of raw data associated with this package that is required to make the final tiff images. This may result in a slow initial download.*

## Usage
**Load brainImageR**

`library(brainImageR)` 

**1. Compare gene set versus all genes in the Allen Brain Atlas dataset.** 

The object vth contains a list of genes that are highly expressed in the ventral thalamus of the developing brain. 

`data(vth)`

#run the spatial enrichment

We've set the reps to 20 here which can calcualte up to a raw p-value of 1/20 = 0.05. Low reps as this provide an accurate visualization for plotting. However, to obtain accurate p-values from the boot function the number of iterations will need to be increased. For example, running 100 iterations will allow you to examine up to a p-value of 1/100 = 0.01. If you would like to assess all regions after multiple-testing correction you should set reps = 1 / (0.05 / 327 regions) tests = 6540. 

`composite <- SpatialEnrichment(vth , reps = 20, refset = "developing")`

**2. Bootstrap the significance of the spatial gene set enrichment** 

#calculate significance

`boot <- Boot(composite)`

save this information

`write.table(boot,"vth_boot.csv",sep = ",")`

`PlotBoot(boot,topcut = 6,botcut = 0.1)`

<img src="https://user-images.githubusercontent.com/5818535/32693131-5adc6bfa-c6f3-11e7-9198-90725167de74.jpg" alt="alt text" width="300" height="220">


**3. Select which section of the brain you would like to image.** 

If you're working with the "developmental" dataset:
Sections to choose from...  

<img src="https://user-images.githubusercontent.com/5818535/32693098-b01cce9e-c6f2-11e7-8c9e-6c0a8526d83b.jpg" alt="alt text" width="500" height="120">

If you're working with the "adult" dataset:
Sections to choose from...  

<img src="https://user-images.githubusercontent.com/5818535/32693074-4e8fd1e4-c6f2-11e7-90af-cd70984059e4.jpg" alt="alt text" width="450" height="250">


**4. Create the Brain Map**

`composite <- CreateBrain(composite, boot,slice =5 ,pcut = 0.5)`

**5. Plot the brain!** 

Changing Breaks changes the color range for plotting. This should be kept between 10 and 18.

`PlotBrain(composite, Breaks = 18)` 

<img src="https://user-images.githubusercontent.com/5818535/32693169-25eb4744-c6f4-11e7-94bc-e656c1142463.jpg" alt="alt text" width="125" height="250">   

**6. Predict developmental time** 

To predict developmental time simply provide a normalized dataset where the columns are samples and the rows are genes (Official Human Gene Symbol Format Required)

`time <- predict_time(dat)` 

**6b. Increase the resolution of your prediction**

With our pre-calculated models you can restrict your predictions so that they will only use certain samples. The list of all available datasets can be found using the availabledatasets function. For example, a useful approach for iPS-derived neurons is to restrict the analysis to only prenatal samples. This will increase the resolution of your model by removing genes that are associated with old age. 

`availabledatasets()` 

`time <- predict_time(dat,dataset = "prenatal")` 

**Manual Predictions: dataset = "none"**

If you want to know whether certain samples are older than others based only on a subset of genes you can also limit the prediction to those genes by setting dataset = "none". For example if the object kv_genes contains all potassium channels, you can run the following code with datset = "none" and genelist = kv_genes to see how your samples are differentially aged based only on potassium channels.

`time_kv <- predict_time(dat, dataset = "none", genelist = kv_genes)`

You can continue to refine the manual predictions by restricting the age range or even tissue type.

`time_kv_prenatal <- predict_time(dat, dataset = "none", genelist = kv_genes, minage = 0, maxage = 40)`

`time_kv_prenatal_hip <- predict_time(dat, dataset = "none", genelist = kv_genes, minage = 0, maxage = 40, tissue = "HIP")`

Don't worry about putting in as many restrictions as you want. If you've gone too far and have restricted the dataset to a point where none of the genes are associated with time, the program will warn you and stop.

Happy predicting!

### Additional Functions

**Look at what genes in your dataset are detected within a given section**

`regiongenes <- GetGenes(genes,composite@tissueExp1,composite@tissueExp2,region.name="SZ")` 



## Credits
brainImageR was written by Sara B. Linker in R. The image sections were coded by Jon Hsu, Adela Pfaff, Lydia Ko, and Sarah Voter.
## License
brainImageR is released under the Creative Commons CC BY-SA 4.0 license.
