SELECT 
    Policy_ID,
    City,
    Safety_Package_Level,
    Exposure,
    Claim_Count,
    Claim_Amount,
    
    CASE 
        WHEN Driver_Age < 25 AND Gender = 'Male' THEN 'Young_Male_HighRisk'
        WHEN Driver_Age < 25 AND Gender = 'Female' THEN 'Young_Female'
        WHEN Driver_Age > 65 THEN 'Senior_Driver'
        WHEN NCD_Level <= 3 THEN 'Risky_History'
        ELSE 'Adult_Standard' 
    END AS Driver_Profile,

    CASE 
        WHEN Vehicle_Segment = 'Sport' THEN 'Performance_Car'
        WHEN Vehicle_Brand IN ('BMW', 'Mercedes', 'Volvo') THEN 'Luxury_Comfort'
        WHEN Vehicle_Segment = 'SUV' THEN 'SUV_Family'
        ELSE 'Standard_Sedan' 
    END AS Vehicle_Class,

    CASE
        WHEN Traffic_Density > 8 THEN 'High_Stress_Zone'
        WHEN Traffic_Density BETWEEN 5 AND 8 THEN 'Medium_Density'
        ELSE 'Quiet_Zone'
    END AS Traffic_Zone

INTO Final_Level3_Data
FROM ham_data_final
WHERE Exposure > 0.05;

SELECT * FROM Final_Level3_Data;