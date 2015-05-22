#!/bin/sh

cabal update
cd /vagrant
cabal install

cat vagrant/profile >> ~/.profile
