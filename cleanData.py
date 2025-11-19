import pandas as pd

# Load dataset
file_path = "PS_2025.09.10_12.52.37.csv"
df = pd.read_csv(file_path, comment="#", low_memory=False)
print("Loaded dataset:", df.shape[0], "rows,", df.shape[1], "columns")

# Keep default parameter
df = df[df["default_flag"] == 1]
print("Filtered to default parameters only:", len(df), "rows")

# Keeps only planets with needed parameters 
df = df[
    (df["pl_bmasse"] > 0) &       # mass (Earth masses)
    (df["pl_rade"] > 0) &         # radius (Earth radii)
    (df["pl_eqt"] > 0)            # equilibrium temperature (K)
]

print("Filtered dataset size:", len(df), "rows remaining")

# Keeps main columns for analysis
cols_to_keep = [
    "pl_name",        # Planet name
    "hostname",       # Host star
    "discoverymethod",# Method of discovery (Validity)
    "default_flag",   # Default flag (set to 1)
    "pl_bmasse",      # Planet mass (Earth masses)
    "pl_rade",        # Planet radius (Earth radii)
    "pl_eqt",         # Equilibrium temperature (K)
    "pl_orbeccen",    # Orbital eccentricity
    "st_met",         # Stellar metallicity
    "st_teff",        # Stellar temperature (K)
    "st_rad",         # Stellar radius (solar)
    "st_mass",        # Stellar mass (solar)
    "pl_insol"        # Insolation flux (Earth units)
]

# Keep only columns
cols_to_keep = [c for c in cols_to_keep if c in df.columns]
df_clean = df[cols_to_keep].copy()

# Converts all numeric columns to numbers
for c in df_clean.columns:
    if c not in ["pl_name", "hostname", "discoverymethod"]:
        df_clean[c] = pd.to_numeric(df_clean[c], errors="coerce")

# Reset index (for pandas)
df_clean = df_clean.reset_index(drop=True)

print("\nMissing values per column:")
print(df_clean.isna().sum())

# Preview
print("\n🔹 Sample data:")
print(df_clean.head(10).to_string(index=False))

# Save filtered data
df_clean.to_csv("exoplanets_filtered_default.csv", index=False)
print("Saved filtered data")

