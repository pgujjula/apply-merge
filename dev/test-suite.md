# Test suite for applyMerge implementations

## Things to test

* Edge cases
* A variety of merging functions
* Laziness

## Thoughts

* full test coverage, i.e., each thing is tested
* want minimal overlap of test case coverage, i.e., each thing is tested
  in only one place
* I'm worried that property tests won't generate edge case inputs, which
  is why I want to write unit tests as well.
* It's harder to write property tests than unit tests
* Need to test finite and infinite inputs
* I also don't understand how property testing libraries work, so I'm
  worried about making a mistake and leaving some code uncovered.

## applyMerge properties to test

* edge cases
  * one or both inputs are empty
  * function is constant in one or both inputs, or only increasing
    sometimes
* a variety of integer functions
* finite and infinite lists
* texts, where the function is lexicographic merging
* input is not ordered, but is ordered according to function

## applyMergeOn properties

* everything in applyMerge tests, for different projection functions

## applyMergeBy properties

* everything in applyMerge tests, for different comparison functions

## NonEmpty applyMerge
* everything in applyMerge tests, except for singleton instead of empty

## drawMergePattern

* correct merge pattern for a variety of functions

## doublyLinkedList

* unit tests for each function
* some property tests for integration

## mutable priority queue

* unit tests for each function
* some property tests for integration
