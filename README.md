## A basic app for exploring candidate/proposed species data.

[Test the app here](https://defend-esc-dev.org/shiny/open/at-risk_species_v0-1/)

This very incomplete app builds off of a Microsoft Access database of at-risk species created by U.S. Fish and Wildlife Service in the Southeast. See [their site](https://www.fws.gov/southeast/candidateconservation/finder.html) for more information about the database. Feel free to follow the Contributing guidelines below or [contact us](mailto:esa@defenders.org).

#### Instructions

You may clone this repository and run the app locally or run it at the address above.

1. `git clone git@github.com:Defenders-ESC/at-risk_species_v0-1.git`
2. Open either `ui.R` or `server.R` in RStudio then choose `Run App` _or_ run `R` from a terminal and use `shiny::runApp()`
3. Explore.

#### Contributing

We welcome bug reports and feedback. If you find a bug then submit an issue [here](https://github.com/Defenders-ESC/at-risk_species_v0-1/issues). 

If you want to propose a code change then submit a [pull request](https://github.com/Defenders-ESC/at-risk_species_v0-1/pulls). In general, read through the code base and use the app a bit, so you understand how the bits-and-pieces are connected. Then, if you would like to suggest a change or update to the project, please follow these steps:

 - Open an issue to discuss your ideas with us.
 - Please limit each pull request to less than 500 lines.
 - Please encapsulate all new work in **short** functions (less than 50 lines each). We currently do not have unit tests for our functions (that will change!), but please include tests with the pull request.
 - Please ensure all tests (new and old) pass before signing off on your contribution.
 - Do something nice for yourself! You just contributed to this research, and we really appreciate you taking the time to check it out and get involved.

The most important step is the first one: open that issue to start a conversation, and we can offer help on any of the other points if you get stuck. 

#### Thanks

Thanks to [Bill Mills](https://github.com/BillMills) for the great Contributing suggestions and for the pointers on adding release information to this README.
