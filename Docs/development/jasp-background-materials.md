# Background materials for developers

## In a nutshell
The diagram below summarizes the technical skills required for contributing to the JASP code base.
Anything outside the `Minimum` box is advanced and, to a certain extent, optional.

```mermaid
graph TD

subgraph Minimum
	git --> github

	R --> Functions
end

Template --> QML
Functions --> Packaging

```
## Learning materials
Below we give you a curated list of high-quality materials to acquire these skills.

### git and GitHub
- Software carpentry [lessons](https://swcarpentry.github.io/git-novice/)
	- 1-6 for learning git
	- 7-9 for learning GitHub
	- 10-14 optional
		- 14, _Using git from RStudio_, is particularly relevant

### R and Functions
- Software carpentry [R for reproducible data analysis](https://swcarpentry.github.io/r-novice-gapminder/)
	- 1-9 cover the basics
	- 10 covers functions
	- 11-15 are quite good, but optional

### R packaging
- Software carpentry [lessons](https://carpentries-incubator.github.io/lesson-R-packaging/)
	- 1-7 cover the basics, including unit testing and dependency management
	- 9-10 are quite good, but optional

### Interface building with QML files
- The best way to learn this is by following [the tutorial](jasp-modules-tutorial.md), which includes a ready to use [template](https://github.com/jasp-stats/jaspModuleTemplate)