# Log for Prompt 8

**Task**: Modify the `simulate_for_different_beta` function such that for each value of beta, `n_runs` runs are made, and the results are the average and standard deviation of the recovered fraction. Use `mc.lapply` to parallelize the `n_runs`. Output the new function in `make_runs.R`.

**Modifications**:
- Created `make_runs.R`.
- Added the `parallel` library.
- Refactored `simulate_for_different_beta` to take `n_runs` and `mc.cores` as arguments.
- Replaced the inner execution loop with `mclapply(1:n_runs, ...)`.
- Calculated the mean and standard deviation from the runs and returned them along with the beta value.
