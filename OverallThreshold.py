import numpy as np
import pandas as pd
# column 1: drift rate; column 2: threshold; column 3: noise; column 4: non-decision time
OverallClose4=np.load('Overal_Radius3_NA4.npy')
OverallFar4=np.load('Overal_Radius3_NA7.npy')
OverallClose7=np.load('Overal_Radius9_NA4.npy')
OverallFar7=np.load('Overal_Radius9_NA7.npy')
MeanClose4=[]
STDClose4=[]
MeanFar4=[]
STDFar4=[]
MeanClose7=[]
STDClose7=[]
MeanFar7=[]
STDFar7=[]
meanClose=[]
meanFar=[]
mean4=[]
mean7=[]
for a in range(0,OverallFar7.shape[1]):
    MeanClose4.append(np.mean(OverallClose4[:,a,1]))
    STDClose4.append(np.std(OverallClose4[:,a,1]))
    MeanFar4.append(np.mean(OverallFar4[:,a,1]))
    STDFar4.append(np.std(OverallFar4[:,a,1]))
    MeanClose7.append(np.mean(OverallClose7[:,a,1]))
    STDClose7.append(np.std(OverallClose7[:,a,1]))
    MeanFar7.append(np.mean(OverallFar7[:,a,1]))
    STDFar7.append(np.std(OverallFar7[:,a,1]))
    
    meanClose.append(np.mean(OverallClose4[:,a,1]))
    meanClose.append(np.mean(OverallClose7[:,a,1]))
    
    meanFar.append(np.mean(OverallFar4[:,a,1]))
    meanFar.append(np.mean(OverallFar7[:,a,1]))
    
    mean4.append(np.mean(OverallClose4[:,a,1]))
    mean4.append(np.mean(OverallFar4[:,a,1]))
    
    mean7.append(np.mean(OverallClose7[:,a,1]))
    mean7.append(np.mean(OverallFar7[:,a,1]))
Participant=[]
for a1 in range(0,2):
    for a2 in range(0,87*2):
        Participant.append(a2)
    
Condition=np.repeat([1,2], 87*2, axis=0)
ThresholdNumberAgents=[]
ThresholdNumberAgents+=mean4
ThresholdNumberAgents+=mean7
ThresholdDensity=[]
ThresholdDensity+=meanClose
ThresholdDensity+=meanFar

df1 = pd.DataFrame({
    'Participant': Participant,
    'Condition': Condition,
    'Threshold': ThresholdNumberAgents
})

df2 = pd.DataFrame({
    'Participant': Participant,
    'Condition': Condition,
    'Threshold': ThresholdDensity
})
# ANOVA =======================================================================
from scipy.stats import f_oneway

# Drift Rate ==================================================================
anova_results1 = f_oneway(df1[df1['Condition'] == 1]['Threshold'],
                          df1[df1['Condition'] == 2]['Threshold'])
print(anova_results1)

anova_results2 = f_oneway(df2[df2['Condition'] == 1]['Threshold'],
                          df2[df2['Condition'] == 2]['Threshold'])
print(anova_results1)
# Linear mixed-effect model ===================================================
import statsmodels.api as sm
from statsmodels.formula.api import mixedlm

model1 = mixedlm("Threshold ~ Condition", df1, groups=df1["Participant"])
result1 = model1.fit()
print(result1.summary())

model2 = mixedlm("Threshold ~ Condition", df2, groups=df2["Participant"])
result2 = model2.fit()
print(result2.summary())
# Friedman ====================================================================
from scipy.stats import friedmanchisquare
df1_wide = pd.DataFrame({
    'condition1': mean4,
    'condition2': mean7,
})

df2_wide = pd.DataFrame({
    'condition1': meanClose,
    'condition2': meanFar,
})
# Assuming data is structured in a wide format dataframe `df_wide` with columns for each condition
friedman_results1 = friedmanchisquare(df1_wide['condition1'], 
                                     df1_wide['condition2'])
print(friedman_results1)

friedman_results2 = friedmanchisquare(df2_wide['condition1'], 
                                     df2_wide['condition2'])
print(friedman_results2)