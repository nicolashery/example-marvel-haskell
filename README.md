# Example Marvel Haskell

A Haskell port of the JavaScript [Example Marvel App](https://github.com/nicolashery/example-marvel-app).

## Quick start

Make sure you have [Stack](www.haskellstack.org) installed. Clone this repository then download the appropriate GHC with:

```bash
$ stack setup
```

Create an `.env` file containing necessary configuration variables:

```
export PORT=3000
export MARVEL_PUBLIC_KEY=your_public_key
export MARVEL_PRIVATE_KEY=your_private_key
```

Build the app with:

```bash
$ stack build
```

Run the app with:

```bash
$ source .env
$ stack exec app
```

For development, launch the REPL:

```bash
$ source .env
$ stack ghci
```

Inside the REPL, run the server with:

```
> :l Main.hs
> main
```

After making changes, quit the server with `Ctrl+C` and reload the code:

```
> :r
> main
```

Use `Ctrl+D` to quit the REPL.

## Deployment

The app is ready to be deployed with [Heroku Docker](https://devcenter.heroku.com/articles/docker).

Make sure you have the prerequisites installed:

- [Docker](https://docs.docker.com/engine/installation/)
- [Docker Compose](https://docs.docker.com/compose/install/)
- [Heroku Toolbelt](https://toolbelt.heroku.com/)
- `heroku plugins:install heroku-docker`

If not already done, create a new Heroku app with:

```bash
$ heroku create example-marvel-haskell
```

Or add the remote for an existing Heroku app with:

```bash
$ heroku git:remote -a example-marvel-haskell
```

To deploy a new version of the app, run:

```bash
$ heroku docker:release
```
