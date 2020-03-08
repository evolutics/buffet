# Buffet 🍜

[![Build](https://img.shields.io/travis/evolutics/buffet)](https://travis-ci.org/evolutics/buffet)
[![License](https://img.shields.io/github/license/evolutics/buffet)](LICENSE)
[![Package](https://img.shields.io/hackage/v/buffet)](https://hackage.haskell.org/package/buffet)

Assemble many Dockerfiles in a single Dockerfile. This gives you the convenience of one Docker image with your favorite tools while keeping a separate Dockerfile per tool.

<details>
<summary>Example</summary>

A Dockerfile for Prettier

```dockerfile
FROM alpine:3.11.0
RUN apk add --no-cache yarn~=1.19 && yarn global add prettier@1.19.1
WORKDIR /workdir
```

plus a Dockerfile for HTML Tidy

```dockerfile
FROM alpine:3.11.0
RUN apk add --no-cache tidyhtml~=5.6
WORKDIR /workdir
```

are automatically assembled in a single Dockerfile

```dockerfile
FROM alpine:3.11.0
RUN apk add --no-cache yarn~=1.19 && yarn global add prettier@1.19.1 \
  && apk add --no-cache tidyhtml~=5.6
WORKDIR /workdir
```

You can try this yourself by running

```bash
buffet assemble examples/minimal_demonstration
```

</details>

See [Code Cleaner Buffet](https://github.com/evolutics/code-cleaner-buffet) for an application of this.

## Installation

For a **quick start,** use the Docker image [`evolutics/buffet`](https://hub.docker.com/r/evolutics/buffet). I recommend a Bash alias like

```bash
alias buffet='docker run --rm --volume "$(pwd)":/workdir evolutics/buffet'
```

You are ready when

```bash
buffet --help
```

works.

Alternatively, you can do a **native** installation with `stack install buffet` or `cabal install buffet`.

## Usage example

Say we work on a simple website with HTML code that we would like to format, clean up, and validate. For this purpose, we choose the tools Prettier and HTML Tidy, which we plan use in continuous integration via a Docker image.

For the following, we assume you first run

```bash
git clone https://github.com/evolutics/buffet.git
cd buffet
```

The source code for this and other examples is in the **[`examples`](examples) folder.**

### Assembling

In the subfolders of [`examples/quick_start`](examples/quick_start), you see a [Dockerfile for Prettier](examples/quick_start/prettier/Dockerfile) and another [Dockerfile for HTML Tidy](examples/quick_start/tidy/Dockerfile). These Dockerfiles (called "dishes") are the modular toy blocks, which we now **automatically combine** to one Dockerfile (called "buffet") by running

```bash
buffet assemble examples/quick_start
```

This prints a Dockerfile based on the subfolders of `examples/quick_start` to stdout. From this, we can then build a Docker image `mona_linta` with

```bash
buffet assemble examples/quick_start | docker build --tag mona_linta -
```

Note the hyphen `-` at the end that makes Docker read the Dockerfile from stdin.

### Testing

As a demo that our Docker image works as expected, run

```bash
docker run -it --rm mona_linta
prettier --version
tidy --version
```

To integrate a check like `prettier --version` as a test of the tool installation, add a `HEALTHCHECK` instruction as you see in the [Dockerfile for Prettier](examples/quick_start/prettier/Dockerfile). The exit status of such a command is then reported when you run our example **test suite** with

```bash
buffet test --arguments examples/quick_start/test_arguments.yaml \
  examples/quick_start
```

This builds a Docker image to then run the tests. Only the dishes referred in the file [`test_arguments.yaml`](examples/quick_start/test_arguments.yaml) are tested.

**Note:** As `buffet test …` executes Docker commands, a [native installation](#installation) is required here.

If you like, try adding a test for HTML Tidy.

### Documenting

You can generate documentation with

```bash
buffet document --template examples/quick_start/document_template.md.mustache \
  examples/quick_start
```

This renders the template [`document_template.md.mustache`](examples/quick_start/document_template.md.mustache). To print the raw **template context,** omit this option as in

```bash
buffet document examples/quick_start
```

Among others, data from `LABEL` instructions is integrated in the template context.

### API usage

You may like to programmatically process the parsed source Dockerfiles. To print an intermediate representation in JSON, run

```bash
buffet parse examples/quick_start
```

## Terminology

- **Buffet:** A Dockerfile automatically assembled based on many dishes.
- **Dish:** A Dockerfile, usually providing a specific command-line tool.
- **Menu:** Configuration with a list of dishes to be assembled in a buffet.
