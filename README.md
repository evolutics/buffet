# Buffet 🍜

[![Build](https://img.shields.io/travis/evolutics/buffet)](https://travis-ci.org/evolutics/buffet)
[![License](https://img.shields.io/github/license/evolutics/buffet)](LICENSE)

Assemble many Dockerfiles in a single Dockerfile. This gives you the convenience of one Docker image with your favorite tools while keeping the modularity of a separate Dockerfile per tool.

See [Code Cleaner Buffet](https://github.com/evolutics/code-cleaner-buffet) for an application of this.

## Terminology

- **Buffet:** A Dockerfile automatically assembled based on many dishes.
- **Dish:** A Dockerfile, usually providing a specific command-line tool.
- **Menu:** Configuration with a list of dishes to be assembled in a buffet.
