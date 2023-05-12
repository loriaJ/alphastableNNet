# based on code from https://github.com/Harry24k/bayesian-neural-network-pytorch/blob/master/demos/Bayesian%20Neural%20Network%20Regression.ipynb 
import numpy as np

import torch
import torch.nn as nn
import torch.optim as optim

import torchbnn as bnn
import matplotlib.pyplot as plt
import pandas as pd

# needs full directory of the file
dir0 = './pytorch_results/'
name0 = '1D_example.csv'
input_names = 'input_data_'+ name0
output_names = 'pytorch_result_' + name0
y0 = pd.read_csv(dir0 + input_names)

x_grid7 = np.array(y0[['V1']].copy())
y0 = pd.DataFrame.to_numpy(y0[['V2']]).flatten()
# d0 = tf.data.Dataset.from_tensor_slices((x_grid7,y0))
x_grid9 = pd.DataFrame.to_numpy(
pd.read_csv(dir0 + 'pred_input_' + name0)
)


x_grid_t = torch.from_numpy(x_grid7).to(torch.float32)

model = nn.Sequential(
    bnn.BayesLinear(prior_mu=0, prior_sigma=0.1, in_features=1, out_features=100),
    nn.Tanh(),
    bnn.BayesLinear(prior_mu=0, prior_sigma=0.1, in_features=100, out_features=1)
)


import time
startTime = time.time()
mse_loss = nn.MSELoss()
kl_loss = bnn.BKLLoss(reduction='mean', last_layer_only=False)
kl_weight = 0.02

optimizer = optim.Adam(model.parameters(), lr=0.01)
y0 = torch.unsqueeze(torch.from_numpy(y0).to(torch.float32),1)

for step in range(3000):
    pre = model(x_grid_t)
    mse = mse_loss(pre, y0)
    kl = kl_loss(model)
    cost = mse + kl_weight*kl
    
    optimizer.zero_grad()
    cost.backward()
    optimizer.step()
    
print('- MSE : %2.2f, KL : %2.2f' % (mse.item(), kl.item()))

executionTime = (time.time() - startTime)
with open(dir0 + 'timing_' + name0[0:3] +'.txt', 'w') as f:
    f.write('Execution time in seconds: ' + str(executionTime))

preds = np.zeros((1000,100))
pred_x = torch.from_numpy(x_grid9).to(torch.float32)
for j in range(1000) :
    preds[j,:] = model(pred_x).detach().squeeze().numpy()


pred_mean = np.mean(preds,0)
pred_std = np.std(preds,0)

preds_fins = np.asarray([x_grid9.transpose()[0], 
            pred_mean, 
            pred_std]).transpose()
np.savetxt(dir0 + output_names, preds_fins, delimiter=",")