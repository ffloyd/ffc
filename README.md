# ffc.el - Ffloyd's Furious Configurations [![Build Status](https://travis-ci.org/ffloyd/ffc.svg?branch=master)](https://travis-ci.org/ffloyd/ffc) [![Coverage Status](https://coveralls.io/repos/github/ffloyd/ffc/badge.svg?branch=master)](https://coveralls.io/github/ffloyd/ffc?branch=master)

A simple Emacs configuration microframework. Make your init.el and life elegant in a way you choose.

UNDER HEAVY DEVELOPMENT. THIS README IS NO MORE THAN QUICK DRAFT. 

## Basic concepts

* _feature_ - abstraction over _configuration section_.
* _pipeline_ - ordered set of _features_. Order in this set defines _features'_ application order.
* _config_ - isolated set of configuration sections (defined by _pipeline_).

## Public API

Short list (everything in this list is a macros, except `ffc-apply`):

* `ffc-feature` - define a configuration _feature_
* `ffc-pipeline` - define a set of used _features_ and application order
* `ffc` - define a _config_ using _features_ from _pipeline_
* `ffc-apply` - load all unloaded _configs_

## Private API

Short list (everything in this list is an function):

* `ffc--define-feature` - feature definer
* `ffc--setup-pipeline` - pipline definer
* `ffc--define-config` - config definer
* `ffc--define-config-from-features` - config definer based on features
* `ffc--load-config` - load config by name

## Prdefined features library (ffc-features.el)

Short list:

* `:deps` - dependencies between configs
* `:init` - just a named codeblock to execute while loading. Meant to be placed before packages loading.
* `:packs` - load and require packages (via straight.el)
* `:conf` - just a named codeblock to execute while loading. Meant to be placed before packages loading.
