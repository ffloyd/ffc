# ffc.el [![Build Status](https://travis-ci.org/ffloyd/ffe-config.svg?branch=master)](https://travis-ci.org/ffloyd/ffe-config) [![Coverage Status](https://coveralls.io/repos/github/ffloyd/ffe-config/badge.svg?branch=master)](https://coveralls.io/github/ffloyd/ffe-config?branch=master)

A simple Emacs configuration microframework. Make your init.el and life elegant.

UNDER HEAVY DEVELOPMENT. THIS README IS NO MORE THAN QUICK DRAFT. 

## Basic concepts

### Features

Using `ffc` we define our configuration as set of relitively small configuration blocks. We call this blocks _configs_.

In elisp terms feature is a simple alist:

``` emacs-lisp
'((name . name-symbol)
  (docstring . "Short feature description")
  (on-define . (lambda () (ignore)))
  (on-load . (lambda () (ignore))))
```

Feature lifecycle has two steps: definition and loading.

### Defining features

Definition is a merely creating new feature alist and push it to global feature storage. There are no execution of user code. Two ways provided:

* 'private', low-level way - using `ffc-define` function
* 'public' way - by `ffc` macros. It built around `ffc-define` function.

Why so? Because `ffc-define` has only four arguments:

* `name` - config name, elisp symbol
* `docstring` - short human-readable description of config
* `on-define` - callback which called after successfull config defining, function
* `on-load` - callback which called when we load config

And it's all what we need. But it's not convinient. But `ffc` macro has different approach:

* has `name` and `docstring` arguments too
* has several keyword arguments
* builds two lambdas based on data from keyword arguments
* uses built lambdas as `on-define` and `on-load` functions 
* allowed keyword list is extensible by _config adapters_

### Config adapters

There are no enabled config adapters by default. You should activate it by calling `ffc-use` macro. Order of activation defines order of execution inside callbacks. Built-in adapters:

* `deps` - checks for config dependencies
* `init` - just executes code in `on-load` callback
* `packs` - installs and requires packages
* `config` - just executes code in `on-load` callback

Example of activation:

``` emacs-lisp
(ffc-use '(deps init packs config))
```

