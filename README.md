# Activity-Nature-Sleep Dashboard

## Project Overview
This Shiny dashboard visualizes relationships between physical activity, nature exposure, and sleep patterns among physically active adolescents. The project is based on data collected from the NatureDose Teen Study conducted in the greater Eugene-Springfield, Oregon area during 2022-2023.

## Key Features
- Interactive visualizations of demographics and behavioral patterns
- Distribution analysis of key variables
- Relationship exploration between different behavioral metrics
- Predictive model for sleep duration based on activity levels

## Prerequisites
Before running this dashboard, ensure you have:
- R (version 4.1.0 or higher)
- RStudio (recommended for the best experience)
- Git (for cloning the repository)

## Installation Guide

1. Clone the repository:

```{bash}
git clone https://github.com/ZF-MPH/shiny-activity-nature-sleep.git
cd shiny-activity-nature-sleep
```

2. Open the project in RStudio:
- Launch RStudio
- Go to File > Open Project
- Navigate to the cloned repository
- Select the .Rproj file

3. Install required packages:
- The project uses renv for package management
- When you open the project, renv will automatically activate
- Run the following commands in the R console:

```{r}
install.packages("renv")  # if you haven't installed renv before
renv::restore()
```

4. Launch the application:

```{r}
shiny::runApp()
```

## Data Description
The dashboard uses data from 326 adolescents (ages 12-17) who participated in the NatureDose Teen Study. Key measurements include:
- Physical activity levels (MVPA, Light PA)
- Sedentary behavior
- Screen time
- Nature exposure
- Sleep duration

## Usage Instructions
The dashboard contains multiple interactive tabs:
1. **Dashboard Information**: Overview and context
2. **Study Information**: Detailed study methodology
3. **Demographics**: Population characteristics
4. **Distributions**: Variable distributions
5. **Relationships**: Correlation analyses
6. **Predict Active Adolescent's Sleep**: Interactive prediction tool

## Important Notes
- Data represents a specific population of physically active adolescents from the Eugene-Springfield area
- Findings may not be generalizable to all adolescent populations
- The dashboard is intended for educational and research purposes

## Authors
- Research Team
  - Co-PIs: Dr. Elizabeth Budd & Dr. Nichole Kelly
  - Research Coordinators: Zach Farley & Esmeralda Castro

## License
This project is licensed under the MIT License - see the LICENSE file for details.

## Issues and Contributions
Feel free to submit issues and enhancement requests using GitHub's issue tracker.

## Acknowledgments
Special thanks to all study participants and the research team who made this project possible.