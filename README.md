# match

This is my attempt at solving Sortable's coding challenge.

## Usage

When run with no arguments, the `match` program will read a list of
products from the `products.txt` and a list of listings from
`listings.txt` in the current working directory.  You can change the
input files by specifying them on the command line as follows:

`match path/to/products.txt path/to/listings.txt`

The result will be printed to standard output, but it can be piped to
a file if desired.

e.g:

`match >output.txt`

## Compiling

This is a standard cabal package, as such, it can be complied and
installed using `cabal install`.

If you perefer to build without installing, you can use:

```
cabal configure
cabal build
```

The complied binary will be found in `dist/build/match/match`.

## Vagrant

I have not made the assumption that the user has a Haskell build
system on their machine, but if they have
[Vagrant](http://vagrantup.com) installed, they can compile and
install this program to a vagrant VM by simply running `vagrant up` in
the project's root directory.

After which time, they can connect to the VM by with the command
`vagrant ssh`.

Once the VM is no longer required, it ca be stopped with `vagrant
halt` or removed from the system entirely with `vagrant destroy`.
