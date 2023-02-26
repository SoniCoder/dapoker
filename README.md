# EMURGO Academy Haskell Course: Solo Project

## Create Your Environment

1. Fork this repository
2. Copy the link to your new repository and prefix it with "https://gitpod.io/#" in your browser
3. Click `Continue with GitHub` and `Authorize gitpod-io`
4. Wait for the environment to build. This can take a while the first time.
5. Select "VS Code Browser" as your editor.

## Build Your Project

This template contains a basic structure for a simple Haskell project. Add your project code to the empty `*.hs` files:

* `src/Lib.hs`: this module is intended to contain the core business logic of your program - it should consist of pure functions (not IO actions). You can rename this or create additional modules as needed (be sure to adjust the `other-modules` section of `dapoker.cabal` to reflect any changes/additions)
* `src/Types.hs`: use this module to define any custom types and synonyms that you'll use in other modules
* `src/Actions.hs`: this module should contain helper IO actions that will be used inside the `main` action of `app/Main.hs`. Most of your effectful code should live in this module.
* `app/Main.hs`: compose a minimal `main` action using helper actions defined in `src/Actions.hs` to run your application (this is what will be used if you )

Add any additional packages you need for your project below `base` in the `build-depends` section of `dapoker.cabal`. Follow instructions if you encounter any errors due to a "hidden package": these refer to packages that are part of the standard library but aren't imported into a Haskell project by default. The editor tooling will identify the name of the package you need to add to `build-depends` in such cases.

Use `cabal repl` in the terminal and the `:l` command followed by a specific module name (`Lib`, `Types`, `Actions`, etc.) to test your code.

Use `cabal run` to run your completed program.

As you complete the assignments, stage, commit and push your changes to Github using the `Source Control` tab in the left panel.

## DaPoker Rules

Welcome to DaPoker!
This version of poker supports only 2 human players
The humans will play their turn one by one on the same machine
The humans will have to enter their names at the beginning
Each player will be given 100 ADA tokens to start with
At the beginning of each round, a total of 5 community cards will be dealt
along with 2 cards to each player at random
The game will start with a small blind of 1 ADA and a big blind of 2 ADA
For simplicity the first player will be the small blind and the second player will be the big blind
This means that the game will start with the first player's turn
Each player has the option to fold, call, check, raise or perform allin
In this primitive version, there are no side pots, the winner takes all