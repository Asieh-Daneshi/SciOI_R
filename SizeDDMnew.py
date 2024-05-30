import numpy as np
import pandas as pd
import scipy.optimize as opt
import matplotlib.pyplot as plt
import csv

NP = 154  # number of participants (always 1 more than the number of participants)
NT = 300  # number of trials
NR = 100  # number of times that I repeat the parameter estimation algorithm
fitted_params = np.zeros((NR, NP, 4))

for r1 in range(NR):
    for n1 in range(NP - 1):
        # Load data
        RT_Data = pd.read_csv('RT_Size.csv')
        participantHand_Data = pd.read_csv('participantHand_Size.csv')
        TotalNumberOfAgents_Data = pd.read_csv('TotalNumberOfAgents_Size.csv')
        NumberOfAgents_Data = pd.read_csv('NumberOfAgents_Size.csv')

        # Assign conditions
        cond1 = (TotalNumberOfAgents_Data == 10) & (NumberOfAgents_Data == 1)
        cond2 = (TotalNumberOfAgents_Data == 10) & (NumberOfAgents_Data == 4)
        cond3 = (TotalNumberOfAgents_Data == 10) & (NumberOfAgents_Data == 7)
        cond4 = (TotalNumberOfAgents_Data == 10) & (NumberOfAgents_Data == 10)
        cond5 = (TotalNumberOfAgents_Data == 14) & (NumberOfAgents_Data == 1)
        cond6 = (TotalNumberOfAgents_Data == 14) & (NumberOfAgents_Data == 4)
        cond7 = (TotalNumberOfAgents_Data == 14) & (NumberOfAgents_Data == 7)
        cond8 = (TotalNumberOfAgents_Data == 14) & (NumberOfAgents_Data == 10)

        Condition_Data = pd.DataFrame(np.select(
            [cond1, cond2, cond3, cond4, cond5, cond6, cond7, cond8],
            ['1', '2', '3', '4', '5', '6', '7', '8'],
            default='0'
        ), index=NumberOfAgents_Data.index)
        # Filter for the current participant
        rts = RT_Data.iloc[n1]
        choices = participantHand_Data.iloc[n1]
        Condition_Data = Condition_Data.iloc[:, 0]
        # Select only trials with the target condition
        target_condition = '1'
        condition_indices = Condition_Data.index[Condition_Data == target_condition]
        rts = rts.iloc[condition_indices]
        choices = choices.iloc[condition_indices]
        # Remove trials where choice is 0
        non_zero_indices = choices != 0
        rts = rts[non_zero_indices]
        choices = choices[non_zero_indices]

        # Debugging prints
        print(f"Participant {n1 + 1}/{NP - 1}")
        print(f"Filtered RTs: {rts}")
        print(f"Filtered choices: {choices}")

        if len(rts) == 0 or len(choices) == 0:
            print("No valid data for this participant under the selected condition.")
            continue

        def ddm_pdf(rt, drift_rate, threshold, noise, t0):
            if rt <= t0:
                return 0
            adjusted_rt = rt - t0
            k = threshold / noise
            mu = k * drift_rate
            lam = k ** 2
            return (lam / (2 * np.pi * adjusted_rt ** 3)) ** 0.5 * np.exp(
                -lam * (adjusted_rt - mu) ** 2 / (2 * mu ** 2 * adjusted_rt))

        def negative_log_likelihood(params, rts, choices):
            drift_rate, threshold, noise, t0 = params
            likelihoods = [ddm_pdf(rt, drift_rate, threshold, noise, t0) for rt in rts]
            max_likelihood = np.max(likelihoods)
            normalized_likelihoods = likelihoods / max_likelihood
            log_likelihoods = np.log(np.maximum(normalized_likelihoods, 1e-10))
            return -np.sum(log_likelihoods)

        initial_params = [1, 0.2, 0.5, 0.2]
        bounds = [(0, None), (0, None), (0, None), (0, None)]

        result = opt.minimize(negative_log_likelihood, initial_params, args=(rts, choices), bounds=bounds, method='L-BFGS-B')

        fitted_drift_rate, fitted_threshold, fitted_noise, fitted_t0 = result.x

        print("Fitted drift rate:", fitted_drift_rate)
        print("Fitted threshold:", fitted_threshold)
        print("Fitted noise:", fitted_noise)
        print("Fitted non-decision time:", fitted_t0)

        if result.success:
            print("Optimization successful!")
        else:
            print("Optimization failed. Message:", result.message)

        mean_rt = np.mean(rts)
        accuracy = np.mean(choices)

        print("Mean RT:", mean_rt)
        print("Accuracy:", accuracy)
        fitted_params[r1, n1, :] = [fitted_drift_rate, fitted_threshold, fitted_noise, fitted_t0]

np.save('Size_10_1.npy', fitted_params)