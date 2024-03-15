# Property Testing API in OCaml

This project implements a simplified Property Testing API in OCaml. Property testing is a method for testing software that checks whether certain properties hold for a wide range of inputs. The goal is to identify edge cases and potential bugs in the software.

## Overview

The project consists of four main modules:

1. **Property**: Manages properties to be tested.
2. **Generator**: Generates pseudo-random data for testing.
3. **Reduction**: Provides strategies for simplifying counterexamples.
4. **Test**: Conducts the actual tests by generating data and checking properties.

## Features

- **Property Module**: Defines properties to be tested, including specific cases like always true or always false.
- **Generator Module**: Generates random data for testing, including basic types like boolean, integer, float, as well as strings and lists.
- **Reduction Module**: Implements strategies for simplifying counterexamples, such as for integers, floats, strings, and lists.
- **Test Module**: Executes tests by generating random data using generators and then checking if the properties hold true. It also attempts to simplify counterexamples using reduction strategies.
