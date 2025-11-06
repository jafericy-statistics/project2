#Banking Data Application

Overview
This Shiny application in R provides an interactive user interface (UI) for exploring and summarizing the banking dataset on consumers and their financial/demographic information.

With this tool, users can subset data by categorical and numeric variables, view tables, and download the filtered results locally.

It is designed to explore data cleaning, dynamic UI creation, and reactive filters in R Shiny that update in real time relative to the filter selections.

#Application Features
•	Data Loading: This feature automatically reads the datafile, regardless of file type .csv, .txt, or .rds files using the load_data() function.
•	Dynamic UI Elements: Dropdowns and filters update based on the variable types (categorical or numeric).
•	Filtering System:  Users can filter data by category or numeric values and then apply those filters interactively.


#How to Run the App
•	Open the project in RStudio.
•	Ensure all required packages are installed: please refer to requirements.txt file for package list and release versions of each.
•	Open app.R and click Run App.
•	Use the sidebar controls to:
•	Choose categorical and numeric variables.
•	Adjust filters and click Apply Filters.


#Application Issues
•	Data should be pulled from a relational DB or cloud storage. Flat file data importing is typically violatle on performace in larger orgs.
• Code should be more optimized on the data filtering and importing.
