# cis-561 project a0
to build/run this project

```
- cd a0
- runhaskell lambda.hs <test-file>
```


(I have included lambda.test as an example of a "test-file")
(The implementation is one of a lambda calculus-esque tiny syntax)
(The program will generate a list of tokens depending on what is included in the file)

I have closely followed the implementation of the tiger language using alex
found [here](https://github.com/simonmar/alex/blob/master/examples/tiger.x)

to run alex on the lambda.x file do

`alex lambda.x`

This will generate lambda.hs
