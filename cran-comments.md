## Test environments
* ubuntu 10.04.1 (JupyterHub), R 4.0.3
* local windows install, R 4.0.3

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

- checking R code for possible problems ... NOTE
  load_mnist: no visible global function definition for ‘read_csv’

  load_mnist is used for testing and is not included in the build
