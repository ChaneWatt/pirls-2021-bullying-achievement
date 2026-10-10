# Importing the regression tables into the essay

The essay uses LaTeX. Add any of these commands to
`scripts/paper/sections/results.tex`:

```tex
\input{../../output/regressions/main_models.tex}
\input{../../output/regressions/interaction_models.tex}
\input{../../output/regressions/common_sample_models.tex}
\input{../../output/regressions/robustness_model.tex}
```

The main paper already loads the required `booktabs` and `graphicx` packages.
Usually the main and interaction tables belong in the paper; the common-sample
and robustness tables can go in an appendix if space is limited.
