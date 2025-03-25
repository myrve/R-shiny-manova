**MANOVA Analysis Shiny Application**

**Overview**

This Shiny application provides an interactive interface for performing Multivariate Analysis of Variance (MANOVA) with customizable factor interactions. The application allows users to upload data, select variables, specify interactions between factors, and visualize the results of the MANOVA analysis.

**Features**

Data Upload: Upload Excel files (xlsx, xls) with automatic variable type detection

Variable Selection: Separate selection for categorical factors and numeric dependent variables

Dynamic Interaction Generation: Automatically generates all possible interactions between selected factors

Customizable Analysis: Select specific interactions to include in the MANOVA model

Comprehensive Results: View multivariate tests, between-subjects effects, post-hoc tests, and descriptive statistics

Export Options: Download results in CSV, Excel, or HTML format

**Installation**

Prerequisites
R (version 4.4.2 or higher)

Required R packages:

- shiny

- shinydashboard

- readxl

- dplyr

- tidyr

- jmv

- DT

- writexl

**Setup Instructions**

Clone this repository:
```
git clone https://github.com/myrve/R-shiny-manova.git
```
Install the required packages:
```
install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "tidyr", "jmv", "DT", "writexl"))
```
Run the application:
```
shiny::runApp("shiny-manova")
```
**Usage Guide**

1. Data Upload
Click on the "Data Upload" tab in the sidebar

Use the "Upload Excel File" button to select and upload your data

Preview your data in the table below

2. Variable Selection
Navigate to the "Variable Selection" tab

Select categorical variables as factors from the first dropdown

Select numeric variables as dependent variables from the second dropdown

Use "Advanced Variable Type Detection" to identify numeric variables with few unique values as potential factors

3. Interactions
Go to the "Interactions" tab to view all possible interactions between selected factors

Use checkboxes to select which interactions to include in the analysis

Use "Select All" or "Deselect All" buttons for quick selection

4. MANOVA Analysis
Navigate to the "Analysis" tab

Configure analysis options:

Include Post-Hoc Tests

Select Post-Hoc Correction Method

Include Descriptive Statistics

Include Assumption Checks

Click "Run Analysis" to perform the MANOVA with selected interactions

5. Results
View results in the "Results" tab

Navigate between different result sections:

Multivariate Tests

Between-Subjects Effects

Post-Hoc Tests

Descriptive Statistics

Download results in your preferred format (CSV, Excel, or HTML)

**Implementation Approach**

The application is structured using the Shiny framework with a modular design:

*Data Processing:*

Automatic detection of variable types using statistical properties

Conversion of factors to appropriate data types for analysis

*Interaction Generation:*

Dynamic generation of all possible interactions between selected factors

Custom UI for selecting specific interactions to include in the model

*MANOVA Implementation:*

Using the jmv package to perform MANOVA analysis

Dynamic model term construction based on user selections

Only selected interactions are included in the analysis

*Results Presentation:*

Organized display of statistical output in tabbed format

Interactive tables with sorting and filtering capabilities

Multiple export options for further analysis

**AI Tools Used**

ChatGPT: Used for code review, debugging assistance, and documentation generation

Perplexity AI: Used for researching statistical concepts and implementation approaches

Gemini: Used for UI design suggestions and code optimization

**File Structure**

app.R: Main application file that loads dependencies and sources other files

ui.R: User interface definition

server.R: Server logic implementation

README.md: Documentation file

**Acknowledgments**
This application was developed as part of a technical assessment for an R Developer position, focusing on statistical analysis and interactive web application development.
