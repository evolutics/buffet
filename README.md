# Buffet 🍜

[![Build](https://img.shields.io/travis/evolutics/buffet)](https://travis-ci.org/evolutics/buffet)
[![License](https://img.shields.io/github/license/evolutics/buffet)](LICENSE)
[![Package](https://img.shields.io/hackage/v/buffet)](https://hackage.haskell.org/package/buffet)

Assemble many Dockerfiles in a single Dockerfile. This gives you the convenience of one Docker image with your favorite tools while keeping the modularity of a separate Dockerfile per tool.

See [Code Cleaner Buffet](https://github.com/evolutics/code-cleaner-buffet) for an application of this.

## Installation

Run

```bash
stack install buffet
```

or

```bash
cabal install buffet
```

You are ready when

```bash
buffet --help
```

works.

## Usage example

Say we work on a simple website with HTML code that we would like to format, clean up, and validate. For this purpose, we choose the tools Prettier and HTML Tidy, which we plan use in continuous integration via a Docker image.

For the following, we assume you first run

```bash
git clone https://github.com/evolutics/buffet.git
cd buffet
```

### Building

In the subfolders of the [`example`](example) folder, you see a [Dockerfile for Prettier](example/prettier/Dockerfile) and another [Dockerfile for HTML Tidy](example/tidy/Dockerfile). These Dockerfiles (called "dishes") are the modular toy blocks, which we now automatically combine to one Dockerfile (called "buffet") by running

```bash
buffet build example
```

This prints a Dockerfile based on the subfolders of `example`. From this, we can then build a Docker image `mona_linta` with

```bash
buffet build example | docker build \
  --build-arg prettier=1.19.1 --build-arg tidy=1 --tag mona_linta -
```

Note how we pass a `--build-arg` per tool, assigning each a nonempty value. In case of Prettier, we use this to parameterize the tool version. If we do not mention a tool in a `--build-arg`, it is not available in the image.

### Testing

As a demo that our Docker image works as expected, run

```bash
docker run -it --rm mona_linta
prettier --version
tidy --version
```

To integrate a check like `prettier --version` as a test of the tool installation, add a `HEALTHCHECK` instruction as you see in the [Dockerfile for Prettier](example/prettier/Dockerfile). The exit status of such a command is then reported when you run our example test suite with

```bash
buffet test --arguments example/test_arguments.yaml example
```

The file [`test_arguments.yaml`](example/test_arguments.yaml) provides the `--build-arg` configuration for this test run.

If you like, try adding a test for HTML Tidy.

### Documenting

You can generate documentation with

```bash
buffet document --template example/document_template.md.mustache example
```

This renders the template [`document_template.md.mustache`](example/document_template.md.mustache). To print the raw template context, omit this option as in

```bash
buffet document example
```

Among others, data from `LABEL` instructions is integrated in the template context.

## Terminology

- **Buffet:** A Dockerfile automatically assembled based on many dishes.
- **Dish:** A Dockerfile, usually providing a specific command-line tool.
- **Menu:** Configuration with a list of dishes to be assembled in a buffet.
