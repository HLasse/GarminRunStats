# GarminRunStats
A ShinyR app for visualizing running progress using your data from Garmin Connect.

Try the app in action [here](https://hlasse.shinyapps.io/GarminRunStats/).

## Main Features
* Interactive graphs of development of distance, time, and pace over time and with dynamic groupings.
* Dynamic tables for a quick overview
* Calender showing which days you ran, how far, and time spent

The repository also includes a Python scraper using Selenium (and Helium) to automatically download your data from Garmin Connect. Simply fill in the "garmin_login.txt" with your username and password and run the script "parser.py".
