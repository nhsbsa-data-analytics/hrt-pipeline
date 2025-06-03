# hrt-pipeline

This code is published as part of the NHSBSA Official Statistics team's commitment to open code and transparency in how we produce our publications. The Hormone Replacement Therapy (HRT) reproducible analytical pipeline (RAP) is owned and maintained by the Official Statistics team.

# Introduction

This RAP aims to bring together all code needed to run a pipeline in R to produce the hrt publication. It includes accompanying documentation in line with RAP best practice. 

The RAP will produce an HTML report and accompanying HTML background and methodology document. This RAP makes use of many R packages, including several produced internally at the NHSBSA. Therefore, some of these packages cannot be accessed by external users. 

This RAP cannot be run in it's entirety by external users. However it should provide information on how the Official Statistics team extract the data from the NHSBSA data warehouse, analyse the data, and produce the outputs released on the NHSBSA website as part of this publication.

This RAP is a work in progress and may be replaced as part of updates and improvements for each new release of the hrt publication. The functions in the `functions` folder do not contain unit testing, although we will investigate adding this in future. These functions have undergone an internal peer review process.

The functions used in the pipeline are currently stored on the [hrtR package](https://github.com/nhsbsa-data-analytics/hrtR).


## Getting started

You can clone the repository containing the RAP through [GitHub](https://github.com/) using the following steps.

In RStudio, click on "New project", then click "Version Control" and select the "Git" option.

Click "Clone Git Repository" then enter the URL of the hrt GitHub repository (https://github.com/nhsbsa-data-analytics/hrt-pipeline). You can click "Browse" to control where you want the cloned repository to be saved in your computer.

You will also need to create a [PAT key](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).

You can view the [source code for the hrt RAP](https://github.com/nhsbsa-data-analytics/hrt-pipeline) on GitHub.

## Running this RAP

Users outside of the Official Statistics team may not have the required access permissions to run all parts of this RAP. The following information is included to document how this pipeline is run by members of the Official Statistics team during production.

Once the repository has been cloned, open the `pipeline.R` file and run the script from start to finish. You will be prompted to enter your username and password into your .Renviron file, if not already there, to connect to the data warehouse. All other code in this script should require no other manual intervention.

The code should handle installing and loading any required packages and external data. It should then get data extracts from the fact table, perform data manipulations, then save this data into spreadsheet outputs. The pipeline will then render the statistical summary narrative and background document as HTML files for use in web publishing.

Publication details such as financial year and publication date can be manually changed in the `config.yml` file if this pipeline is rerun in future. Running the pipeline for a different time period may require users to change some function arguments, such as the fact table to extract data from.


## Contributing

Contributions are not currently being accepted for this package. If this changes, a contributing guide will be made available.

## Contact Information

If you wish to contact our team to make suggestions, or have ideas you would like to share, please contact us by email via: statistics@nhsbsa.nhs.uk

## License

The `hrtR` package, including associated documentation, is released under the MIT license. Details can be found in the `LICENSE` file.
