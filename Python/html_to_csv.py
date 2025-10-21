# package imports
import pandas as pd

# function imports
from pathlib import Path

season = "2025"
week = "07"

# html folder path
html_path = Path(f"{Path(__file__).resolve().parent.parent}/html")

# stat categories
categories = ["passing", "receiving", "rushing"]

# drop maps
drops = {
    "passing": ["Rk", "Team", "EPA/DB", "YPA"],
    "receiving": ["Rk", "Team", "Yds/Rec"],
    "rushing": ["Rk", "Team", "YPC", "RYOE/Att", "YACo/Att"]
}

# column maps
columns = {
    "passing": {
        "Player": "player",
        "Pos": "position",
        "GP": "games_played",
        "GS": "games_started",
        "Cmp": "completions",
        "Att": "attempts",
        "Yds": "passing_yards",
        "TD": "passing_touchdowns",
        "INT": "interceptions",
        "Rating": "rating",
        "TTT": "time_to_throw",
        "Sack": "sack",
        "QBP": "pressures",
        "QBP %": "pressure_pct",
        "Cmp %": "completion_pct",
        "CPOE": "completion_pct_oe",
        "DB": "dropbacks",
        "EPA": "passing_epa",
        "AY/Att": "air_yards_per_attempt",
        "Deep %": "deep_pct",
        "Blitz %": "blitz_pct",
        "PA %": "play_action_pct",
        "Avg. Sep": "average_separation",
        "TW %": "tight_window_pct"
    },
    "receiving": {
        "Player": "player",
        "Pos": "position",
        "GP": "games_played",
        "GS": "games_started",
        "Rts": "routes",
        "Tgt": "targets",
        "Rec": "receptions",
        "Yds": "receiving_yards",
        "TD": "receiving_touchdowns",
        "Catch %": "catch_pct",
        "CROE": "catch_rate_oe",
        "Rec EPA": "receiving_epa",
        "YAC": "receiving_yards_after_catch",
        "YACOE": "receiving_yards_after_catch_oe",
        "AY/Tgt": "air_yards_per_target",
        "Avg. Sep": "average_separation"
    },
    "rushing": {
        "Player": "player",
        "Pos": "position",
        "GP": "games_played",
        "GS": "games_started",
        "Att": "carries",
        "Yds": "rushing_yards",
        "TD": "rushing_touchdowns",
        "RYOE": "rushing_yards_oe",
        "Rush EPA": "rushing_epa",
        "Fum": "fumbles",
        "Lost": "fumbles_lost",
        "YACo": "rushing_yards_after_contact",
        "10+ Yds": "over_ten",
        "15+ MPH": "fifteen_mph",
        "20+ MPH": "twenty_mph"
    }
}

# function to convert % columns to decimal columns
def pct_to_decimal(column):
    if column.str.endswith("%").any():
        return (column.str.rstrip("%").astype(float) / 100).round(3)
    
    return column

# read .html files and save .csv files
for category in categories:
    dfs = []

    for html_file in sorted(html_path.glob(f"{week}_{category}_*.html")):
        df = pd.read_html(html_file)[0]
        dfs.append(df)

    if not dfs:
        continue

    # passing dfs need to be merged
    if category == "passing":
        dfs[1].drop(columns=["Rk", "Team", "GP", "GS", "Cmp", "Att", "Cmp %", "CPOE", "DB", "EPA", "TTT", "QBP %", "EPA/DB"], inplace=True)
        df = pd.merge(left=dfs[0], right=dfs[1], on="Player", how="outer")
    
    # receiving and rushing dfs can be concatenated
    else:
        df = pd.concat(dfs, ignore_index=True)

    df.drop(columns=drops[category], inplace=True)
    df.rename(columns=columns[category], inplace=True)

    df[df.select_dtypes(include="object").columns] = df.select_dtypes(include="object").apply(pct_to_decimal)

    df.to_csv(f"{Path(__file__).resolve().parent.parent}/data/nfl/{category}/{season}_{week}.csv", index=False)