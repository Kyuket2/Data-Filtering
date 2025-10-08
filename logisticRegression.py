import pandas as pd


file_path = "exoplanets_clean.csv"


df = pd.read_csv(file_path)
print("Loaded dataset:", df.shape[0], "rows,", df.shape[1], "columns")

df["has_water"] = (
    (df["pl_eqt"].between(200, 350)) &
    (df["v_esc_kms"] >= 8) &
    (df["density_gcc"] >= 3) &
    (df["pl_orbeccen"] <= 0.3) &
    (df["st_met"] >= 0)
).astype(int)
