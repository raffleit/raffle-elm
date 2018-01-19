# Raffle It! Elm &middot; [![CircleCI](https://circleci.com/gh/raffleit/raffle-elm.svg?style=shield)](https://circleci.com/gh/raffleit/raffle-elm) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/raffleit/raffleit-elm/blob/master/LICENSE)

This is an implementation of Raffle It! in Elm, a purely functional compile-to-javascript language built to easily create webapps.

## Pros and Cons (Purely Subjective)

Pros | Cons
---- | --------
Purely Functional | Purely Functional (Side effects are sometimes nice)
Great Language | Few conventions
State Handling like redux, but easier | 


## Known Bugs
- When adding a participant, the focus isn't moved to the name input.

## Command Line Stuff

### Running Locally
* Install [create-elm-app](https://www.npmjs.com/package/create-elm-app)
* Run `elm-app install`
* Run `elm-app start`

### Creating a Production Build
* elm-app install
* elm-app build

### Running Tests
If there were any tests, you could run them with `elm-app test`

## License

Raffle It! is [MIT licensed](./LICENSE).
