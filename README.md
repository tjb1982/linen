![flax banner](https://theveganecofibrecast.files.wordpress.com/2012/04/flax_spindle.jpg)

# __Flax__ #

Flax is a domain specific language for modeling coordinated, distributed processes. It was designed to help with 
situations that require provisioning (potentially complex) systems and/or real integrations with external services 
in a production or production-like environment. It's similar to Ansible, but designed to provide more low-level transparency
and polyglot friendliness. I.e., for Flax, expressiveness is a higher priority than simplicity, and flexibility a higher priority
than a low barrier to entry and/or user-friendliness.

The Flax language is implemented as a Clojure library; however, its source code is most idiomatically written
in YAML. That decision was made for (relatively) easy source code generation from any
language that can serialize data structures into JSON (since YAML is basically a superset of JSON).

## Installation
With [Leiningen](http://leiningen.org), add this to your project's `project.clj` file:

    [co.nclk/flax "0.1.8"]


> N.B. the best place to look for the current version is the top of the `project.clj` file.


## Getting started
A Hello World in Flax might look something like this:

```clojure
(ns my-project.main
  (:require [flax.core :as flax]))

(defn -main
  [& _]
  (flax/run
    {:program
     {:main
      [{:module
        {:checkpoints
         [[{:invocation "echo Hello World"}]]}}]}}))
```

or in YAML:

```clojure
(flax/run (clj-yaml.core/parse-string (slurp "path/to/program.yaml")))
```

```yaml
program:
  main:
  - module:
      checkpoints:
      - - invocation: echo Hello World
```

The above is a minimal configuration required to run a Flax program.

## What is it?
This documentation is a work in progress. If you didn't find the above description very satisfying, you might find a more satisfying exposition
[here](https://youtu.be/ZG_k5CSYKhg?t=158) for now.
