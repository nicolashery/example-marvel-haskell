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
$ stack exec app
```

For development, launch the REPL:

```bash
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
