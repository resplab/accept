# accept 1.0.2
* 'accept' now defaults to `accept2`. 
* 'accept' now checks `FEV1` range.


# accept 1.0.1
* 'accept' now compatible with the `predict` method from `stats`
* 'accept' now compatible with `vetiver` for deployment and monitoring


# accept 1.0.0

## New features
* `accept` now automatically calls latest version of prediction model available, while giving the user an option to choose which version to call.
* Now handles missing columns and missing values. 

# accept 0.9.1

## Bug fixes
* A bug that prevented reduced models with `accept2` to execute properly has now been fixed. 


# accept 0.9.0

## Bug fixes
* A bug in projecting the number of severe exacerbations using `accept2` has now been fixed. 


# accept 0.8.3

## Bug fixes
* `accept2` can now receive vector input and produce vector output. 

# accept 0.8.2
This is a minor release with new and optimized functionality. Importantly, the package now includes the updated model `accept 2.0` which is fine-tuned to provide better predictions for individuals with no prior exacerbation history. Users can use the `accept()` function to produce predictions from the original publication, while `accept2()` provides predictions from the updated model.  We have also optimized the code for speed and the code for both models now runs about an order of magnitude faster.

## New features
* new prediction model accept2 added. 
* functions optimized for speed and performance. Model predictions are now calculated approximately 10 times faster.

## Breaking changes
* `predictACCEPT()` is now replaced with `accept()` and `accept2()`.

# accept 0.7.1
* doi updated as the manuscript is now published in Lancet Respiratory Medicine.

# accept 0.7.0
* New release.
