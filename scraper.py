"""
Scrape garmin connect
"""

import os
import time
import re
from selenium import webdriver

from helium import *


## Set working directory and get login credentials from file
WORKING_DIR = os.getcwd()

with open("garmin_login.txt") as f:
    CREDENTIALS = f.read().splitlines()

# Setting firefox options
opts = webdriver.FirefoxOptions()
opts.set_preference("browser.download.folderList", 2) # dont use default downloads dir
opts.set_preference("browser.download.dir", WORKING_DIR) # set the new download dir
opts.set_preference("browser.download.manager.showWhenStarting", False) # turns off download progress bar
opts.set_preference("browser.helperApps.neverAsk.saveToDisk","text/csv") # save without asking
opts.headless = True

# Open firefox and navigate to page
browser = webdriver.Firefox(options = opts)
set_driver(browser)
get_driver()
go_to("https://connect.garmin.com/signin")

# Fill in boxes
write(CREDENTIALS[0], into="Email")
write(CREDENTIALS[1], into="Password")
# click log in button
browser.find_element_by_xpath('//*[@id="login-btn-signin"]').click()
print("Logging in ...")
# wait for page to load
time.sleep(5)

# open sidebar
click("Widgets")
time.sleep(2)
# click activites
browser.find_element_by_css_selector(".icon-activities").click()
# click all activiites
browser.find_element_by_css_selector("ul.main-nav-group:nth-child(2) > li:nth-child(1) > ul:nth-child(2) > li:nth-child(1) > a:nth-child(1)").click()
# filter running
time.sleep(1)
browser.find_element_by_xpath("/html/body/div[1]/div[3]/div[2]/div[3]/div/div/div[1]/div[2]/div[2]/div/div/button[2]").click()
# click an element to be able to scroll down

time.sleep(1)
# Identify box to scroll in
to_scroll = browser.find_element_by_class_name("row-fluid.list-items.flexItemAutoHeight")

# How many pixels to scroll down
pixels = [2000, 4000, 6000, 8000, 10000]
for i in range(1,4):
    browser.execute_script(f"arguments[0].scrollTop={pixels[i]}", to_scroll)
    time.sleep(1)

# Click export
browser.find_element_by_class_name("export-btn").click()
print("Done!")




