# Print package startup message

The function is called when the package is loaded through the `library`
or `require` functions. It prints a message to the console.

## Usage

``` r
.onAttach(libname, pkgname)
```

## Arguments

- libname:

  The library name

- pkgname:

  The package name

## Value

Nothing
