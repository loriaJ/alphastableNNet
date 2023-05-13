# Posterior Inference on Infinitely Wide Bayesian Neural Networks under Weights with Unbounded Variance

**"Posterior Inference on Infinitely Wide Bayesian Neural Networks under Weights with Unbounded Variance" Authors: Jorge Loría, Anindya Bhadra.** 

Preprint: []()

To replicate the simulations use the files: 
- `example_1D.R`, and
- `example_2D.R`.
These files require the R packages: `mlegp`, `tgp`, and `tidyverse` . Using, for example, `install.packages('mlegp')` makes the package `mlegp` available in the local computer.

The main functions to run the predictions are in: `qmat_functions.R` file. Specifically, the main function used is: `sample_2d`. This function takes 3 required parameters:

- `x`: the observed locations matrix,
- `y`: the observed responses, and
- `xstar`: the locations for predictions.
 
It can also receive the optional parameters:
- `sigma2`: the assumed variance, predefined to be 1, must be positive,
- `alpha0`: the index parameter of the $\alpha$-stables, assumed to be 1, must be in the open interval $(0,2)$,
- `known_sigma`: `TRUE` if the assumed variance is given by the `sigma2` parameter, or `FALSE` if it is not known, predefined to be `TRUE`,
- `nu`: scale of the $\alpha$-stable variables, predefined to be 1, must be positive
- `n_sim`: the number of simulations, predefined to be 1000, must be a positive integer,
- `return_qs_only`: boolean to identify if the function should compute the partitions and their probabilities only or if it should perform the complete inference process, false by default,
- `q_list_lambda`: by default an empty list, used to separate the procedure in two parts: obtaining the partitions and their respective probabilities, and the Markov chain Monte Carlo process which depends on the empty list.

We include the main examples used in the paper in the files `example_1D.R` and `example_2D.R` for one and two dimensions. The results using the air quality data from York, are in the folder `york_air_quality`. There are three scripts to run the python code: `pytorch_1d.py`, `pytorch_2d.py`, and `tubes_torch.py`; these need the locations on line 13, as a directory location (e.g. `/usr/location/alphastableNNet/`), but their output is already in the repository. To run the python code the libraries needed are: numpy, torch, torch.nn, torch.optim, torchbnn, matplotlib, and pandas.


To obtain the Table 1 of the paper, you can simply run the script `./york_air_quality/compare_validation_results.R`. To obtain the `.RData` file used for that script, run the `./york_air_quality/samps_valid_tubes.R` , which uses output (i.e., $q_\ell$ and $\Lambda$) from the file `./york_air_quality/splits_valid_tubes.R` .
