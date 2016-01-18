![flax banner](https://theveganecofibrecast.files.wordpress.com/2012/04/flax_spindle.jpg)

#__Flax__

[TOC]

Flax is a domain specific language for modeling coordinated, distributed processes. It was designed to help with situations that require provisioning and interacting with (potentially complex) systems and/or real integrations with external services in a production or production-like environment. Similar to Ansible, Flax is designed to provide more low-level transparency and polyglot friendliness, making expressiveness a higher priority than simplicity, and flexibility a higher priority than a low barrier to entry.

The Flax language is implemented as a Clojure library; however, its source code is most idiomatically written in [YAML](http://yaml.org), which also enables source code generation from any language that can serialize data structures into JSON (since YAML is basically a superset of JSON).

## Installation
Because Flax is a library, it doesn't come with a command-line interface, but it's not complicated to make one yourself. (The first example under [Getting started](#getting-started) shows a minimal example.)

Within a [Leiningen](http://leiningen.org) project, add this to your `project.clj` file:

    [co.nclk/flax "0.1.8"]


> _N.B., the best place to look for the current version is the top of the `project.clj` file._


## Getting started
Using the Clojure library directly, a Hello World in Flax might look something like this:

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
         [[{:source "echo Hello World"}]]}}]}}))
```

or for the same thing, with source in YAML (in this case using [clj-yaml](https://github.com/circleci/clj-yaml) for a parser):

```clojure
(ns my-project
  (:require [co.nclk.flax.core :as flax]
            [clj-yaml.core :as yaml]))
  
(defn -main [& argv]
  (flax/run (-> argv first yaml/parse-string))
```

```yaml
program:
  main:
  - module:
      checkpoints:
      - - source: echo Hello World
```

```bash
$ lein run path/to/program.yaml
```

Running this doesn't print "Hello World" to the console. Instead, it prints it to the stdout of a child process, which is captured in the `returns` entry of the data structure that's returned from the interpreter. Each return captures stdout, as well as stderr, the exit code and whether or not the checkpoint was deemed a success. The interpreter also returns a snapshot of the environmental changes yielded by running the module; these will be merged into the old environment before running any [dependent modules](#dependent-modules). You have to explicitly provide entries for the environment in the module definition, and in this case there aren't any.

```clojure
({:module {:checkpoints [[{:source "echo Hello World"}]]},
  :returns
  (({:source "echo Hello World",
     :runid #uuid "56915511-dd4b-4b6a-91e9-bfbd3f083a39",
     :out {:keys nil, :value "Hello World"},
     :err {:keys nil, :value ""},
     :exit {:keys nil, :value 0},
     :started 1453119225087,
     :finished 1453119225242,
     :success {:value true}})),
  :env {}})
```

The above is a minimal configuration required to [`run`](https://github.com/tjb1982/flax/blob/master/src/co/nclk/flax/core.clj#L536) a Flax program. The `:source` is [spit](https://clojuredocs.org/clojure.core/spit) into a temporary file, which is made executable, and run by the host operating system. On my system, the default interpreter is bash, but it's natural to use interpreter directives in a module to switch languages, because it's just an executable file that runs on the host system:

```yaml
checkpoints:
- - source: |
      #!/usr/bin/env python2
      from __future__ import print_function
      import sys
      print("Hello Python", file=sys.stderr)
```

```yaml
checkpoints:
- - source: |
    #!/usr/bin/env tclsh
    puts {Hello Tcl}
```

You can also mix and match interpreters in the same module:

```yaml
checkpoints:
- - source: |
      #!/usr/bin/env php
      <?php echo "Hello "."PHP";
  - source: |
      #!/usr/bin/env dart
      void main() {
          print('Hello Dart');
      }
```

Above, both sources will run concurrently; i.e., the first process may return after the second process returns. In the next example, the first process exits before the second process starts (notice the extra `-`, making two group-lists of one element each, instead of one group-list of two elements):

```yaml
checkpoints:
- - source: |
      #!/usr/bin/env php
      <?php echo "Hello "."PHP";
- - source: |                      # <== the extra '-'
      #!/usr/bin/env dart
      void main() {
          print('Hello Dart');
      }
```

You can use this to manage concurrency in modules:

```yaml
checkpoints:
- - source: |
      sleep 2
      echo "Two" >> /tmp/test
  - source: echo "One" >> /tmp/test
- - source: echo "Three" >> /tmp/test
```
```bash
$ cat /tmp/test
One
Two
Three
```

Interpreter directives aren't always portable, though, so you can instead provide a `invocation` template for Flax to use for your script:

```yaml
checkpoints:
- - invocation:
      template: "gcc % -o foo"
      match: "%"
    source: |
      int main(int argc, char **argv) {
          printf("Hello World\n");
          return 0;
      }
```
The `%` above is swapped out for the name of the temporary file that is created. Any regex can be used to match the place where you want the filename to be swapped instead. Or you can provide a plain string, to which the filename will be appended, instead of providing a map with a template:
```yaml
checkpoints:
- - invocation: python
    source: print "Hello"
```

##Design philosophy
Flax is designed to emulate modular synthesizers in the signal processing domain. Modules come in two basic varieties: generators and filters. Once a signal is generated (such as a sine wave), it's generally passed on to filters (such as amplifiers, equalizers, phase-vocoders, etc.) that can be patched together to create a patch program.

By analogy, Flax programs are like synth patch programs and Flax modules are like generators or filters. Each module might have inputs that come from the outputs of other modules (or the environment at large), and/or outputs that lead to the inputs of other modules. Each module may affect the state of the signal (i.e., the environment) in any way; modules are _not_ required to be idempotent (but there's no reason they couldn't be designed that way).

## Features
### Parameterization
#### The __`requires`__ module entry
We can design modules to _require_ input parameters like this:

```yaml
requires:
- key: filename
- key: append
  default: true
checkpoints:
- - source: >
      echo hostname -I ~{#append}>~{/append}> ~{filename}
```
And then patch that module into our program (instead of composing it inline, as was done in previous examples):
```yaml
program:
    main:
    - module: ~{HOME}/path/to/above/cluster-append.yaml
      in:
        filename: ~@cluster-file
env:
    cluster-file: ~{HOME}/foo.txt
```

Here, the `~{HOME}` notation is used to interpolate the $HOME environment variable assumed to exist in the process's environment. For interpolation, Flax uses [mustache](https://mustache.github.io/) templates, with default parameters such that `~{` and `}` denote open and close tags, respectively. Bearing that in mind, the `~{#append}`/`~{/append}` notation above was used to toggle an extra `>` which causes bash to append to the file instead of overwriting it. It could also have been written like:

```yaml
source: |
  ~{#append}
  echo $(hostname -I) >> ~{filename}
  ~{/append}
  ~{^append}
  echo $(hostname -I) > ~{filename}
  ~{/append}
```

#### Interpolation vs. dereferencing
Interpolation always results in a string, whereas the `~@` notation dereferences the actual value of the symbol (which might be a string, int, float, map, vector, etc.). Symbols from the process's environment prior to being incorporated with Flax's environment will usually (always?) represent strings, but symbols declared in Flax's `env` above represent whatever types are delivered by the YAML parser. YAML also has a concept of [dereferencing](http://www.yaml.org/spec/1.2/spec.html#&%20anchor//), and the two are compatible. The YAML parser runs first, dereferencing its own symbols, then Flax evaluates its symbols based on the current environment before running the checkpoints. E.g., the following works:

```yaml
checkpoints:
- - nodes: &default-nodes
    - name: ~@node-name-var
    - name: literal-node-name
    source: foo
  - nodes: *default-nodes
    source: bar
```
#### The __`provides`__ module entry
Output parameters are _provided_ in two ways. Since each checkpoint can be run on multiple nodes concurrently, the various outputs of all of them can be conjoined to one collection by placing a directive entry (comprised of `out`, `err`, `exit`, and `success`) in the checkpoint map:

```yaml
provides:
- key: secondses
checkpoints:
- - nodes:
    - name: foo
    - name: bar
    out: secondses
    source: date +%s
```

They can also be gathered _per node_ by placing the directives into the node map instead of the checkpoint map:

```yaml
provides:
- key: seconds-foo
- key: seconds-bar
checkpoints:
- - nodes:
    - name: foo
      out: seconds-foo
    - name: bar
      out: seconds-bar
    source: date +%s
```


### Clojure interop
As a Lisp, Clojure uses a code-as-data approach, which lends itself to extension by and interoperation with other languages written in data serialization formats such as YAML. Because of that, and since Flax is hosted by Clojure, it's possible to use any Clojure functions directly in Flax code by using a special syntax. A very short list of special forms and macros are also supported (`if`, `or`, `and`, `fn`, and `#`) and a few Flax-specific macros (`parallelize`, `upmap`, and `log`).

Examples:
```yaml
source:
  ~(if:
  - ~@private-network
  - hostname -I
  - dig +short myip.opendns.com @resolver1.opendns.com
```
```yaml
checkpoints:
- - nodes:
    - name:
        ~(or:
        - ~@maybe-node-name
        - ~@definitely-node-name
```
```yaml
~(and:
- ~(string?:
  - ~@maybe-string-or-blank
- ~(not:
  - ~(clojure.string/blank?:
    - ~@maybe-string-or-blank
```

There are also special notations for keywords and symbols:

```yaml
~(get:
- ~(or:
  - ~@some-associative-thing
  - { foo: bar, baz: quux }
- ~:some-key
```

### Dependent modules

## What is it?
This documentation is a work in progress. If you didn't find the above description very satisfying, you might find a more satisfying exposition
[here](https://youtu.be/ZG_k5CSYKhg?t=158) for now.
