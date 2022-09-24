"""
Scrape running activities from garmin connect
"""
from argparse import RawDescriptionHelpFormatter
import os
import time
from selenium import webdriver

from helium import set_driver, get_driver, go_to, write, click


def rename_most_recent_file(newname: str, folder: str):
    filename = max(
        list(os.listdir(folder)),
        key=lambda xa: os.path.getctime(os.path.join(folder, xa)),
    )
    if ".part" in filename:
        time.sleep(1)
        os.rename(os.path.join(folder, filename), os.path.join(folder, newname))
    else:
        os.rename(os.path.join(folder, filename), os.path.join(folder, newname))


if __name__ == "__main__":
    ## Set working directory and get login credentials from file
    WORKING_DIR = os.getcwd()

    with open("garmin_login.txt", encoding="utf-8") as f:
        CREDENTIALS = f.read().splitlines()

    # Setting browser options
    opts = webdriver.ChromeOptions()
    # opts.add_argument("--headless")
    opts.add_argument("--disable-gpu")
    opts.add_argument("--no-sandbox")
    opts.add_argument("--disable-dev-shm-usage")
    opts.add_experimental_option("excludeSwitches", ["enable-automation"])
    prefs = {
        "download.default_directory": WORKING_DIR,
        "download.prompt_for_download": False,
        "download.directory_upgrade": True,
        "profile.password_manager_enabled": False,
        "credentials_enable_service": False,
    }
    opts.add_experimental_option("prefs", prefs)

    # Open chrome and navigate to page
    browser = webdriver.Chrome(options=opts)
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
    # click("Widgets")
    time.sleep(2)
    # click activites
    browser.find_element_by_css_selector(".icon-activities").click()
    # click all activiites
    browser.find_element_by_css_selector(
        "ul.main-nav-group:nth-child(2) > li:nth-child(1) > ul:nth-child(2) > li:nth-child(1) > a:nth-child(1)"
    ).click()
    # filter running
    time.sleep(1)
    browser.find_element_by_xpath(
        "/html/body/div[1]/div[3]/div[2]/div[3]/div/div/div[1]/div[2]/div[2]/div/div/button[2]"
    ).click()
    # click an element to be able to scroll down

    time.sleep(1)
    # Identify box to scroll in
    to_scroll = browser.find_element_by_class_name(
        "row-fluid.list-items.flexItemAutoHeight"
    )
    print("Getting activities ...")
    # How many pixels to scroll down
    times_to_scroll = 10
    pixels = [2000 * i for i in range(times_to_scroll)]
    for i in range(times_to_scroll):
        browser.execute_script(f"arguments[0].scrollTop={pixels[i]}", to_scroll)
        time.sleep(1)

    # Click export
    browser.find_element_by_class_name("export-btn").click()
    print("Done!")

    time.sleep(2)
    rename_most_recent_file("activities.csv", WORKING_DIR)
