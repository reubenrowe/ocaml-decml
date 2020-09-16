I found some ways to tweak the network to improve performance by a reasonable margin (For a MSE threshold of 0.03, you can get a 61% accuracy prediction rather consistently. If you decide to take 250 or so neurons in the hidden layer, you can reliably boost this to over 86%!).
I also tidied up the array implementation by nesting methods and using modularity, as well as the abstract interface
