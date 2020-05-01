## Contribution guidelines:
We always welcome contributions to the package. If you would like to propose a new functionality and/or report an issue, please use GitHub's tracker called [Issues](https://github.com/bfast2/bfast/issues).

For development we use the [GitHub Flow](https://guides.github.com/introduction/flow/) branching model.

Key steps:
1. Create a GitHub issue in this repository with description of the work that you plan to do.
2. Assign yourself to the GitHub issue you are working on, to inform other developers that you are working on it.
3. Create your own working fork or branch based on the `dev` branch.
4. Make your changes in that branch.
5. Commit your changes to your working branch as long as you are not finished with your development.
6. Make sure that all tests pass (e.g., in Travis or Circle CI).
7. Once your work is finished, create a pull request so that another developer can review your changes before merging them with the `dev` (or `main`) branch.

Additional steps for [preparing a new release](https://guide.esciencecenter.nl/best_practices/releases.html):
8. Update the `NEWS.Rd` file with most notable changes.
9. Add new contributors to the `DESCRIPTION` file if applicable.
10. Release the package and make it citable (add `CITATION.cff` including DOI).
