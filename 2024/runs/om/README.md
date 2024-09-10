#Simple directory containing the bare minimum files to start a run using pm(.tpl)

## Folder "OM"

Changes:
  No covariance matrix (crashes)

Steps to use:
    1) condition the OM
    2) run OM by invoking -iseed 123 , command might be: "pm -binp orig.bar -phase 22 -nohess -iseed 123"
    3) ensure that "catch.dat" is used/updated at each time step
    4) link to MCMC run?




