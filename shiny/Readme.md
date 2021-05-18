## Ambulance Quality Indicators - Shiny Apps <br/>
#### UI for running the code with a click button interface
##### The code performs 9 checks on the data:
* SummedFalse is “summed” columns for each trust compared against the England figure
* MeanedFalse is “meaned” columns for each trust compared against the England figure
* WeightedFalse is “weighted” columns for each trust compared against the England figure
* RegionSummedFalse is the same as SummedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* RegionMeanedFalse is the same as MeanedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* RegionWeightedFalse is the same as WeightedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* SingleFalse is checking for regions with only one trust that the trust figures match the region – no calculations just matching trust against region for all columns
* OtherValFalse is a custom check for ensuring metrics which are meant to add up do or that numerators are not larger than denominators
* Dashes is checking for dashes in the data for that period
#### Example Output from web app:
![AmbwebOutput](https://github.com/LPulle/NHSE-Analytics/blob/master/AmbWebOutput.jpg)


