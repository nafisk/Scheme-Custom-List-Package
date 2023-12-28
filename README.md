<!-- 
[Shared Google Document](https://docs.google.com/document/d/19h2Z1moOObMAWEGFSmHPZzvxmRgOOb_M6cIg8BmYzPU/edit?usp=sharing)
<br />
[Proofs Document](https://docs.google.com/document/d/1lrjlgidH_ZaTF97SX-55sWJw0NCZB9y4pVJWF5rTglg/edit?usp=sharing)
<br />
[Nafis's Hand Notes](https://drive.google.com/file/d/1VzWXACGPSuJKUavgdihDE_2O1HJlvjbw/view?usp=sharing)
<br/>
[Iterative Proof](https://drive.google.com/file/d/1jh60dOvVJRmbsl99a6O3IAPBf7RvlIe-/view?usp=sharing)

-->

# Scheme Custom List Package

The Scheme-Custom-List-Package is an innovative software package designed for advanced list manipulation and set operations in Scheme. It uniquely represents lists and sets as numbers, utilizing prime number indexing. This package allows for complex operations on lists and sets without relying on Scheme's standard list manipulation functions.

## Features
- **List Representation as Numbers**: Lists are represented by numbers, with elements indexed by prime numbers.
- **Set Operations**: Manipulate finite sets of positive integers represented as numbers.
- **Comprehensive List Operations**: Includes functions like `myequal?`, `head`, `ref`, `tail`, `insert-at-head`, `len`, `snoc`, `last`, `insert-at`, `myappend`, `myreverse`, and `palin?`.
- **Sorting Capability**: Function `sort` for ordering list elements.
- **Advanced Set Functions**: Functions like `element-of?`, `subset-of?`, `equal-sets?`, `union-set`, and `intersection-set`.

## Usage
- Install a Scheme interpreter.
- Clone the repository:
  ```
  git clone https://github.com/nafisk/Scheme-Custom-List-Package
  ```
- Load the package in your Scheme environment:
  ```
  (load "path-to-scheme-custom-list-package.scm")
  ```

## Examples
- Representing a list `(5, 2, 8, 2)` as a number and performing operations.
- Manipulating sets represented as numbers.

## Testing
Includes comprehensive tests to validate the functionality of all operations.

## Contributors
- Nafis Khan
- Deepankar Chakraborty

## About
This package explores innovative list and set representations in Scheme, pushing the boundaries of conventional list processing by representing lists and sets as numbers. It employs a novel approach to handle complex data structures without relying on standard list manipulation functions in Scheme.

The package is a perfect example of Literate Programming, intertwining code and documentation to create a readable and maintainable codebase.

© 2023 Nafis Khan. All Rights Reserved.

---

*Note: This README is a basic guide for the Scheme-Custom-List-Package. For detailed documentation and examples, refer to the source code and accompanying documentation.*
