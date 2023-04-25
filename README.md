# Posterior Inference on Infinite Width Limits of Neural Networks under Weights with Unbounded Variance

**"Posterior Inference on Infinite Width Limits of Neural Networks under Weights with Unbounded Variance" Authors: Jorge Loría, Anindya Bhadra.** 
Preprint: []()

The functions to replicate the simulations are in the files: 
- 

These are used several times, using bash calls, to perform the simulations shown in the preprint.

The main functions to run the prediction are in: `qmat_modif.R`. Specifically, the main function to use is: `sample_2d`. This function takes 3 required parameters:

- `x`: the observed locations matrix,
- `y`: the observed responses,
- `xstar`: the locations for predictions,
 
It can also receive the optionals parameters: 
- `sigma2`: the assumed variance, predefined to be 1,
- `alpha0`: the index parameter of the $\alpha$-stables,
- `known_sigma`: if the assumed variance is given by the `sigma2` parameter,
- `nu`: scale of the $\alpha$-stable variables,
- `n_sim`: the number of simulations, predefined to be 100,
- `return_qs_only`: boolean to identify if the function should compute the partitions and their probabilities only or if it should perform the complete inference process, false by default,
- `q_list_lambda`: by default an empty list, used to separate the procedure in two parts: obtaining the partitions and their respective probabilities, and the Markov chain Monte Carlo process which depends on the empty list.

We include a small example in the file `example.R`.

