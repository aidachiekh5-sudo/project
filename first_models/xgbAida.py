import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix, classification_report

import pandas as pd
from sklearn.model_selection import train_test_split
import xgboost as xgb
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix, classification_report
from sklearn.preprocessing import MinMaxScaler
import matplotlib.pyplot as plt
from imblearn.over_sampling import SMOTE



df = pd.read_csv("C:/Users/Asma/Desktop/b2b_data4_updated.csv")

categorical_features = ['COUNTRY', 'INDUSTRY']
df = pd.get_dummies(df, columns=categorical_features, drop_first=False)
df['PLAN'] = df['PLAN'].map({
    'Pro': 1,
    'Growth': 2,
    'Enterprise': 3
})
df['SEGMENT'] = df['SEGMENT'].map({
    'SMB': 1,
    'Mid-Market': 2,
    'Enterprise': 3
})

df = df.drop(columns=['CUSTOMER_ID', 'CUSTOMER_STATUS','PREDICTED_CHURN', 'CHURN_PROBABILITY'])

print(df.columns.tolist())



from sklearn.preprocessing import MinMaxScaler
# Initialize the scaler
#scaler = MinMaxScaler()

# Normalize continuous features to range [0, 1]
#df[['MRR_norm', 'SUBSCRIPTION_DURATION_norm', 'AVG_MONTHLY_LOGINS_norm', 'AVG_TICKETS_RAISED_norm','AVG_FEATURE_USAGE_norm']] = scaler.fit_transform(
#    df[['MRR', 'SUBSCRIPTION_DURATION', 'AVG_MONTHLY_LOGINS', 'AVG_TICKETS_RAISED','AVG_FEATURE_USAGE']]
#)


X = df.drop(columns=['CHURN_LABEL'])  # Features
y = df['CHURN_LABEL']                # Target


X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42, stratify=y
)

# Apply SMOTE only on training data
smote = SMOTE(random_state=42)
X_train_resampled, y_train_resampled = smote.fit_resample(X_train, y_train)

model = xgb.XGBClassifier(random_state=42)
model.fit(X_train_resampled, y_train_resampled)

# Make predictions
y_pred = model.predict(X_test)



y_proba_rf = model.predict_proba(X_test)[:, 1]



print("XGBoost Performance Metrics")
print(f"Accuracy:  {accuracy_score(y_test, y_pred):.4f}")
print(f"Precision: {precision_score(y_test, y_pred):.4f}")
print(f"Recall:    {recall_score(y_test, y_pred):.4f}")
print(f"F1 Score:  {f1_score(y_test, y_pred):.4f}")
print("\nConfusion Matrix:")
print(confusion_matrix(y_test, y_pred))


print("\nClassification Report:")
print(classification_report(y_test, y_pred))

# View probabilities
results_rf = pd.DataFrame({
    'Actual': y_test.values,
    'Predicted': y_pred,
    'Churn_Probability': y_proba_rf
})
print("\n Sample predictions with probabilities:")
print(results_rf.head(10))




