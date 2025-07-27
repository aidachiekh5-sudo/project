import pandas as pd
from sklearn.model_selection import train_test_split
import random



df = pd.read_csv("C:/Users/Asma/Desktop/b2b_data4.csv")

new_rows = []


for i in range(5001, 10001):
    customer_id = i

    options = ["SMB", "Mid-Market", "Enterprise"]
    segment = random.choice(options)

    options = ["Financial Services/Fintech", "Professional Services", "Education", "Healthcare", "Media", "Technology", "Logistics", "Retail", "Manufacturing", "Real Estate"]
    industry = random.choice(options)

    options = ["France", "Japan", "Singapore", "Canada", "UK", "USA", "Brazil", "Australia", "India", "Germany"]
    country = random.choice(options)

    options = ["Pro", "Growth", "Enterprise"]
    plan = random.choice(options)

    mrr = random.randint(75, 9900)

    subscription_duration = random.randint(30, 1108)

    avg_monthly_logins = round(random.uniform(115, 1476), 6)

    avg_tickets_raised = round(random.uniform(63.67, 739.00), 6)

    avg_feature_usage = round(random.uniform(0.002423, 99.959840), 6)

    churn_probability = round(random.uniform(0, 1), 2)

    predicted_churn = 1 if random.random() < churn_probability else 0

    if subscription_duration <= 377 :
        customer_status = "Churned"
        churn_label = 1
    else:
        churn_label = 0 if random.random() < 0.85 else 1
        if churn_label == 0:
            customer_status = "Active"
        else:
            customer_status = "Churned"

    # Add this row as a dictionary to the list
    new_rows.append({
        "CUSTOMER_ID": customer_id,
        "SEGMENT": segment,
        "INDUSTRY": industry,
        "COUNTRY": country,
        "PLAN": plan,
        "MRR": mrr,
        "SUBSCRIPTION_DURATION": subscription_duration,
        "CUSTOMER_STATUS": customer_status,
        "CHURN_LABEL": churn_label,
        "AVG_MONTHLY_LOGINS": avg_monthly_logins,
        "AVG_TICKETS_RAISED": avg_tickets_raised,
        "AVG_FEATURE_USAGE": avg_feature_usage,
        "PREDICTED_CHURN": predicted_churn,
        "CHURN_PROBABILITY": churn_probability
    })

# Convert list of dicts to DataFrame
new_df = pd.DataFrame(new_rows)

# Append to original dataframe
df = pd.concat([df, new_df], ignore_index=True)

df.to_csv("C:/Users/Asma/Desktop/b2b_data4_updated.csv", index=False)

print("New data successfully added to DataFrame!")


print(df.head())



