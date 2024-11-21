import pandas as pd
from scipy.stats import pearsonr
df = pd.read_csv('Churn_Modelling.csv')

# Calculate the Pearson correlation coefficient and p-value
corr_coefficient, p_value = pearsonr(df['Age'], df['Balance'])

print(f"Pearson correlation coefficient: {corr_coefficient}")
print(f"P-value: {p_value}")

alpha = 0.05
if p_value < alpha:
    print(f"The correlation is statistically significant (p < {alpha}).")
else:
    print(f"The correlation is not statistically significant (p >= {alpha}).")
