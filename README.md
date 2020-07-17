# Neighbourhood Choices

> Datta, Laura, 2020, "Understanding Housing Relocation at the Neighbourhood-level", Tilburg University Thesis.


## Dependencies

Please follow the installation guide on http://tilburgsciencehub.com/.

- R. [Installation Guide](http://tilburgsciencehub.com/setup/r/).


## How to run it

Open your command line tool:

- Check whether your present working directory is  `neighbourhood_choice` by typing `pwd` in terminal

  - if not, type `cd yourpath/neighbourhood_choice` to change your directory to `neighbourhood_choice`

- Each stage contains a makefile which executes the scripts in the right order. Type `make RULE` in the command line.

## Directory Structure

```txt
├── data
├── gen
│   ├── analysis
│   │   ├── input
│   │   ├── output
│   │   └── temp
│   ├── data-preparation
│   │   ├── input
│   │   ├── output
│   │   └── temp
│   └── paper
│       ├── input
│       ├── output
│       └── temp
└── src
    ├── analysis
    ├── data-preparation
    └── paper
```
