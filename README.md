# phpfmt

`phpfmt` is a highly opinionated code formatter for PHP. It intentially provides very little control over the format of your code and sticks close to the recommendations found in [PSR-12](https://www.php-fig.org/psr/psr-12/).

> **Warning** `phpfmt` is still in development and does not format code perfectly just yet.

## Installation

The recommended installation method at the moment is building from source. 

1. Clone this repository.

```sh
git clone git@github.com:ryangjchandler/phpfmt.git
```

2. Use `cargo` to build and install the binary.

```sh
cargo install --path .
```

3. Initialise your project.

```sh
phpfmt init
```

4. Configure your project inside of `phpfmt.toml`.
5. Format your code.

```sh
phpfmt run
```

## Roadmap

`phpfmt` still has some issues and things missing. Below is a list of items and bugs that I'd like to get ticked off before tagging an beta release.

* [ ] Support printing all statements and expressions.
* [ ] Ensure formatted strings maintain escape sequences and line breaks.
* [ ] Make method chains more readable across multiple lines.
* [ ] Provide configuration options for concatenation spacing.

## Contributing

All contributions are welcome. Please consult the [Issues](/issues) page to see what we need help with.