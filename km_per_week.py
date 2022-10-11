"""Tiny little script to calculate km per week."""
import datetime
from typing import Union

# pylint: disable=invalid-name


def format_time(minutes: Union[int, float]) -> str:
    hours_total = minutes // 60
    # Get additional minutes with modulus
    minutes_total = minutes % 60
    # Format the time
    return f"{int(hours_total)}:{int(minutes_total):02d}"


def print_run_times(minutes: Union[int, float]) -> tuple:
    """Estimate how long each run should be assuming `n_runs_per_week` and
    the long run taking up `percent_time_long_run`"""
    # Calculate how much time should be spent on the long run
    time_long_run = minutes * percent_time_long_run
    # Calculate how much time should be spent on the other runs
    time_other_runs = minutes - time_long_run
    # Calculate how much time should be spent on each run
    time_per_run = time_other_runs / n_runs_per_week

    print(
        f"\tLong run: {format_time(time_long_run)}, {n_runs_per_week - 1} runs of {format_time(time_per_run)}"
    )


# set starting data
start_date = datetime.date(2022, 10, 10) - datetime.timedelta(days=7)
cur_time_per_week = 32 + 35 + 45
percent_increase_per_week = 0.10
n_runs_per_week = 3
percent_time_long_run = 0.4

DELOAD = True

n_weeks = 12

deload_every_n_weeks = 6

cur_date = start_date

deload_options = (
    f"deloading by 20% every {deload_every_n_weeks} weeks" if DELOAD else "no deloading"
)

print(
    f"""If you start on {start_date} with {format_time(cur_time_per_week)} per week and increase by {percent_increase_per_week * 100}% per week, 
    {deload_options} you may run the following times: """
)

print(
    f"Week 0: {cur_date.strftime('%d-%m-%y')}. You ran {format_time(cur_time_per_week)}"
)
for week in range(1, n_weeks + 1):
    if DELOAD:
        if week % deload_every_n_weeks == 0:
            cur_time_per_week *= 0.8
        elif (week - 1) % deload_every_n_weeks == 0 and week != 1:
            cur_time_per_week *= 1.05
    cur_time_per_week = cur_time_per_week * (1 + percent_increase_per_week)
    cur_date = cur_date + datetime.timedelta(days=7)
    print(
        f"Week {week}: {cur_date.strftime('%d-%m-%y')}. You may run {format_time(cur_time_per_week)}"
    )
    print_run_times(cur_time_per_week)

# get current date
