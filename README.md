# PAsaltscreen

Code for analysis of high throughput omni-tray screening. 

A 52 microtiter plate library of transposon mutants was screened for growth on high salt media, which was photographed and analyzed by Pyphe. 

## Scripts

`EDTgenerator.R` was used to generate the EDT file necessary for Pyphe analysis (and adding metadata).

`Generatereport.R` was used to analyze the Pyphe output, and outputs reports for different growth outcomes (outcome 1 corresponds to growth on LBA, but no growth on LBA+Salt, etc).

`Colonyextract.R` reads the outcome 1 report from the previous script, and extracts pictures of specific colonies from the plate pictures. 

## Folder content

`
images 
`
which contains the cropped (with ImageJ) microtiter plates.

`
pyphe_quant 
` 
which is generated by pyphe-quantify

`
qc_images
` 
which is generated by pyphe-quantify

`
Colonyextract
` 
which is generated by Colonytextract.R. Contains a plate-by-plate extraction of colonies that grow on LBA but not LBA+Salt