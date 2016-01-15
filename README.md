![flax banner](https://theveganecofibrecast.files.wordpress.com/2012/04/flax_spindle.jpg)

# __Flax__ #

Flax is a domain specific language for modeling coordinated, distributed processes that
require provisioning (potentially complex) systems and/or real integrations with external services in a production
or production-like environment.

Flax is implemented as a Clojure library. It uses a declarative, domain specific language with source code written
in YAML, which allows for easy source code generation from any
language that can serialize map- or dictionary-like data structures into JSON (because YAML is a superset of JSON).

## Install
With [Leiningen](http://leiningen.org), add this to your project's `project.clj` file:

    [co.nclk/flax "0.1.0"]

## Getting started
Flax follows the so-called Hollywood principle (i.e., "don't call us; we'll call you") to encourage modularity
and decoupling. But Flax also emulates modular synthesis [patches](https://en.wikipedia.org/wiki/Synthesizer#Patch)
from the domain of audio signal processing.

A Hello World in Flax would look something like this:

```clojure
(ns my-project.core
  (:require [flax.core :as flax]))
  
(defn -main
  [& argv]
  (let [articulation {:data-connector "co.nclk.flax.data"
                      :program "~/path/to/my/program.yaml"
                      :env {:FOO "World"}}]
    (System/exit (flax/run articulation))))
```

```yaml
# ~/path/to/my/program.yaml <-- a "patch program"

main:
- module: ~/path/to/my/modules/greet.yaml
  in: { NAME: ~@FOO }
```

```yaml
# ~/path/to/my/modules/greet.yaml <-- a "module"

requires:
- key: NAME
- key: FILENAME
  default: ~/greetings.txt
checkpoints:
- - invocation: echo "Hello, ~{NAME}\n" >> ~{FILENAME}
```

Above there are three different components: an _articulation_, a _patch program_, and a _module_. These are terms 
borrowed from modular synthesis in the domain of audio signal processing, a high-level analogy for
the [Inversion of Control](http://martinfowler.com/bliki/InversionOfControl.html) pattern that Flax employs. In 
modular synthesis, the analogous components might be a MIDI keyboard (i.e., a controller device capable of making 
an _articulation_ comprised of various parameters) and a set of _modular_ generators and/or filters, which are 
_patched_ together to form a _program_.

In the above, we provided an _articulation_ that indicated that we wanted to:

- resolve filenames into _programs_ and _modules_ `[:connector "flax-file-connector"]`
- use a particular file as the source code for our _patch program_ `[:program "~/path/to/my/program.yaml"]`
- initialize the environment to contain a single variable "FOO" `[:env {:FOO "World"}]`

The _patch program_ in this caseis comprised of a single property, `:main` that consists of a list of branches that 
will be articulated in parallel. The single branch we've provided _patches_ the current state into a
_module_ at the filename indicated.

The _module_ itself just appends "Hello, " and the given `NAME` to a given `FILENAME` or its default **on the local
system**. In this case, the default path is used.

<!-- Here go other examples
1. Append a greeting to a file on a different node (other than localhost).
2. Append a greeting to a file on N nodes concurrently.
3. Create three nodes, install some software, start the service and see that it's running.
-->

## What is it?
This documentation is a work in progress. If you didn't find the above description very satisfying, you might find a more satisfying exposition
[here](https://youtu.be/ZG_k5CSYKhg?t=158) for now.
