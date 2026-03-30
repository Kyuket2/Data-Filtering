import pandas as pd
import numpy as np

file_path = "csv/PS_2025.09.10_12.52.37.csv"
df = pd.read_csv(file_path, comment="#", low_memory=False)

df = df[df["default_flag"] == 1]

# numeric conversions
df["pl_eqt"] = pd.to_numeric(df["pl_eqt"], errors="coerce")
df["pl_insol"] = pd.to_numeric(df["pl_insol"], errors="coerce")

# Compute fallback equilibrium temperature from insolation when pl_eqt is missing
# Teq ≈ 278.5 * insol^(1/4)
df["pl_eqt_est"] = df["pl_eqt"]
mask_missing_eqt = df["pl_eqt_est"].isna() & df["pl_insol"].notna() & (df["pl_insol"] > 0)
df.loc[mask_missing_eqt, "pl_eqt_est"] = 278.5 * np.power(df.loc[mask_missing_eqt, "pl_insol"], 0.25)

print("Default rows:", len(df))
print("Has pl_eqt:", df["pl_eqt"].notna().sum())
print("Has pl_insol:", df["pl_insol"].notna().sum())
print("Has pl_eqt_est (eqt or computed):", df["pl_eqt_est"].notna().sum())

# Keep anything with an observed or estimated temperature
df_temp = df[df["pl_eqt_est"].notna()].copy()

cols_to_keep = [
    "pl_name","hostname","discoverymethod","default_flag",
    "pl_eqt","pl_insol","pl_eqt_est",
    "pl_orbeccen","st_met","st_teff","st_rad","st_mass"
]
cols_to_keep = [c for c in cols_to_keep if c in df_temp.columns]
df_temp = df_temp[cols_to_keep].reset_index(drop=True)

df_temp.to_csv("exoplanets_temp_eqt_or_est.csv", index=False)
print("Saved: exoplanets_temp_eqt_or_est.csv  rows:", len(df_temp))