import numpy as np
import pandas as pd
import scipy.optimize as opt
import matplotlib.pyplot as plt
import csv

NP=87       # number of participants (always 1 more than the number of participants)
NT=300      # number of trials
NR=100      # number of times that I repeat the parameter extimation algorithm
fitted_params=np.zeros((NR,NP,4))  
for r1 in range(0, NR):
    for n1 in range(0,NP-1):
        RT_Data = pd.read_csv('RT_Overall.csv')
        participantHand_Data = pd.read_csv('participantHand_Overall.csv')
        Radius_Data = pd.read_csv('Radius_Overall.csv')
        NumberOfAgents_Data = pd.read_csv('NumberOfAgents_Overall.csv')
        # here, I take care of the conditions
        # condition = (Radius_Data==3.3) & (NumberOfAgents_Data==4)
        cond1=(Radius_Data==3.3) & (NumberOfAgents_Data==4)
        cond2=(Radius_Data==3.3) & (NumberOfAgents_Data==7)
        cond3=(Radius_Data==9) & (NumberOfAgents_Data==4)
        cond4=(Radius_Data==9) & (NumberOfAgents_Data==7)
        Condition_Data = pd.DataFrame(np.where(cond1, '4', 
                                               np.where(cond2, '1', 
                                                        np.where(cond3, '2', '3'))),
                          index=Radius_Data.index, columns=Radius_Data.columns)
    
        condition_indices=[]
        rts=[]
        choices=[]
        print(n1)
        rts=pd.DataFrame(RT_Data.iloc[n1])
        choices=pd.DataFrame(participantHand_Data.iloc[n1])
        Condition_Data=pd.DataFrame(Condition_Data.iloc[n1])
        Condition_Data.columns = ['condition']
        target_condition = '1'
        condition_indices = Condition_Data.index[Condition_Data['condition'] == target_condition]
        # .tolist()
        
        # Filter df1 and df2 using these indices
        rts = rts.loc[condition_indices].to_numpy()
        choices = choices.loc[condition_indices].to_numpy()
        choices = choices[choices != 0]
        rts = rts[rts != 0]
        # choices[22]=1
        # choices[39]=1
        # rts[22]=rts[21]
        # rts[39]=rts[38]
        # rts = np.array([0.5, 0.6, 0.7, 1.2, 1.3, 0.8, 0.9, 1.1, 1.5, 1.0])
        # choices = np.array([1, 1, 0, 1, 0, 1, 1, 0, 1, 1])
        
        def ddm_pdf(rt, drift_rate, threshold, noise, t0):
            if rt <= t0:
                return 0
            adjusted_rt = rt - t0
            k = threshold / noise
            mu = k * drift_rate
            lam = k**2
            return (lam / (2 * np.pi * adjusted_rt**3))**0.5 * np.exp(-lam * (adjusted_rt - mu)**2 / (2 * mu**2 * adjusted_rt))
        
        def negative_log_likelihood(params, rts, choices):
            drift_rate, threshold, noise, t0 = params
            likelihoods = [ddm_pdf(rt, drift_rate, threshold, noise, t0) for rt in rts]
            
            max_likelihood = np.max(likelihoods)
            normalized_likelihoods = likelihoods / max_likelihood
            log_likelihoods = np.log(np.maximum(normalized_likelihoods, 1e-10))
            # log_likelihoods = np.log(np.maximum(likelihoods, 1e-10))
            return -np.sum(log_likelihoods)
        
        initial_params = [1, 0.2, 0.5, 0.2]  # Initial guesses for drift rate, threshold, noise, and non-decision time
        bounds = [(0, None), (0, None), (0, None), (0, None)]  # Bounds to prevent non-physical values
        
        result = opt.minimize(negative_log_likelihood, initial_params, args=(rts, choices), bounds=bounds, method='L-BFGS-B')
        
        fitted_drift_rate, fitted_threshold, fitted_noise, fitted_t0 = result.x
        
        print("Fitted drift rate:", fitted_drift_rate)
        print("Fitted threshold:", fitted_threshold)
        print("Fitted noise:", fitted_noise)
        print("Fitted non-decision time:", fitted_t0)
        
        # Check if the optimization was successful
        if result.success:
            print("Optimization successful!")
        else:
            print("Optimization failed. Message:", result.message)
        
        # plt.hist(rts, bins=50, alpha=0.5, label='Observed RTs')
        # plt.xlabel('Response Time')
        # plt.ylabel('Frequency')
        # plt.legend()
        # plt.show()
        
        mean_rt = np.mean(rts)
        accuracy = np.mean(choices)
        
        print("Mean RT:", mean_rt)
        print("Accuracy:", accuracy)
        fitted_params[r1, n1, :] = [fitted_drift_rate, fitted_threshold, fitted_noise, fitted_t0]
np.save('Overal_Radius9_NA7.npy', fitted_params)
# Overal_Radius3_NA4=np.load('Overal_Radius3_NA4.npy')