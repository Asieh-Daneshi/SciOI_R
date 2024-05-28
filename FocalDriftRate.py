import numpy as np
import pandas as pd
# column 1: drift rate; column 2: threshold; column 3: noise; column 4: non-decision time
FocalLow4=np.load('Focal_low_NA4.npy')
FocalHigh4=np.load('Focal_high_NA4.npy')
FocalLow7=np.load('Focal_low_NA7.npy')
FocalHigh7=np.load('Focal_high_NA7.npy')
MeanClose4=[]
STDClose4=[]
MeanFar4=[]
STDFar4=[]
MeanClose7=[]
STDClose7=[]
MeanFar7=[]
STDFar7=[]
Mean4=[]
Mean7=[]
MeanFocal=[]
MeanSparse=[]
for a in range(0,FocalLow4.shape[1]):
    MeanClose4.append(np.mean(FocalLow4[:,a,0]))
    STDClose4.append(np.std(FocalLow4[:,a,0]))
    MeanFar4.append(np.mean(FocalHigh4[:,a,0]))
    STDFar4.append(np.std(FocalHigh4[:,a,0]))
    MeanClose7.append(np.mean(FocalLow7[:,a,0]))
    STDClose7.append(np.std(FocalLow7[:,a,0]))
    MeanFar7.append(np.mean(FocalHigh7[:,a,0]))
    STDFar7.append(np.std(FocalHigh7[:,a,0]))
    
    Mean4.append(np.mean(FocalLow4[:,a,0]))
    Mean4.append(np.mean(FocalHigh4[:,a,0]))
    
    Mean7.append(np.mean(FocalLow7[:,a,0]))
    Mean7.append(np.mean(FocalHigh7[:,a,0]))
    
    MeanFocal.append(np.mean(FocalHigh4[:,a,0]))
    MeanFocal.append(np.mean(FocalHigh7[:,a,0]))
    
    MeanSparse.append(np.mean(FocalLow4[:,a,0]))
    MeanSparse.append(np.mean(FocalLow7[:,a,0]))
    
Participant=[]
for a1 in range(0,4):
    for a2 in range(0,84):
        Participant.append(a2)
    
Condition=np.repeat([1,2], 84*2, axis=0)
DriftRateNumberAgents=[]
DriftRateNumberAgents+=Mean4
DriftRateNumberAgents+=Mean7
DriftRateFocality=[]
DriftRateFocality+=MeanFocal
DriftRateFocality+=MeanSparse

df1 = pd.DataFrame({
    'Participant': Participant,
    'Condition': Condition,
    'DriftRate': DriftRateNumberAgents
})

df2 = pd.DataFrame({
    'Participant': Participant,
    'Condition': Condition,
    'DriftRate': DriftRateFocality
})
# ANOVA =======================================================================
from scipy.stats import f_oneway

# Drift Rate ==================================================================
anova_results1 = f_oneway(df1[df1['Condition'] == 1]['DriftRate'],
                         df1[df1['Condition'] == 2]['DriftRate'])
print(anova_results1)

anova_results2 = f_oneway(df2[df2['Condition'] == 1]['DriftRate'],
                         df2[df2['Condition'] == 2]['DriftRate'])
print(anova_results2)
# Linear mixed-effect model ===================================================
import statsmodels.api as sm
from statsmodels.formula.api import mixedlm

model1 = mixedlm("DriftRate ~ Condition", df1, groups=df1["Participant"])
result1 = model1.fit()
print(result1.summary())

model2 = mixedlm("DriftRate ~ Condition", df2, groups=df2["Participant"])
result2 = model2.fit()
print(result2.summary())
# Friedman ====================================================================
from scipy.stats import friedmanchisquare
df1_wide = pd.DataFrame({
    'condition1': Mean4,
    'condition2': Mean7
})
# Assuming data is structured in a wide format dataframe `df_wide` with columns for each condition
friedman1_results = friedmanchisquare(df_wide['condition1'], 
                                     df_wide['condition2'])
print(friedman1_results)