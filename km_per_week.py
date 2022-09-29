"""Tiny little script to calculate km per week."""
import datetime

# set starting data
start_date = datetime.date(2022, 10, 3) - datetime.timedelta(days=7)
cur_km = 15
percent_increase_per_week = 0.10

DELOAD = True

n_weeks = 20

deload_every_n_weeks = 6

cur_date = start_date

deload_options = (
    f"deloading by 20% every {deload_every_n_weeks} weeks" if DELOAD else "no deloading"
)

print(
    f"""If you start on {start_date} with {cur_km} km per week and increase by {percent_increase_per_week * 100}% per week, 
    {deload_options} you may run the following distances: """
)
for week in range(1, n_weeks + 1):
    if DELOAD:
        if week % deload_every_n_weeks == 0:
            cur_km *= 0.8
        elif (week - 1) % deload_every_n_weeks == 0:
            cur_km *= 1.05
    cur_km = cur_km * (1 + percent_increase_per_week)
    cur_date = cur_date + datetime.timedelta(days=7)
    print(f"Week {week}: {cur_date.strftime('%d-%m-%y')}. You may run {cur_km:.2f} km")

# get current date
