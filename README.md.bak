
# yar
-------

![alt tag](https://40.media.tumblr.com/tumblr_lopphnsL821qz874do1_500.jpg)

**Yar** is a declarative DSL for modeling process pipeline scenarios that require provisioning complex distributed systems and/or real integrations with external services in a production or production-like environment. The framework is designed around inversion of control/dependency injection, so that tests use the so-called [Hollywood principle](https://en.wikipedia.org/wiki/Hollywood_principle) (i.e., "don't call us, we'll call you") to maintain a high degree of modularity and decoupling.

The primary components that make up any model are the **profile** and the **scenario**. The source code for both is written in YAML (n.b., JSON is valid YAML). A _profile_ models a particular state, whereas a _scenario_ describes the hierarchical relationship between possible profile branches. A good analogy is vertices and edges in a directed graph. Each profile is intended to serve as a logical snapshot from where any number of other profiles can extend. A branch potentially _requires_ certain data from any given profile and _provides_ certain data that other branches can use as inputs. These inputs and outputs are designed to be similar to patches in audio synth modules, or filter pipelining in the Digital Signal Processing domain generally.

A simple profile that creates a cluster, installs a service, and runs a smoketest to verify that it's responding to requests might look like this:

```yaml
requires:
    - key: REPO_HOME
      description: the path to the repository where the smoketest lives
provides:
    - key: MY_CLUSTER_PUBLIC_IP
      description: the public ip-addr for the node where the service is running
checkpoints:
    groups:
        - group:
            - cluster-name: my-cluster
              exec: dig +short myip.opendns.com @resolver1.opendns.com
              out: MY_CLUSTER_PUBLIC_IP
        - group:
            - description: >
                A smoke test that pings the api base n times every m seconds to
                verify that the service has started
              exec: >
                $REPO_HOME/tests/smoketests/smoketest.sh 10 2
                    $MY_CLUSTER_PUBLIC_IP:8080/api/
clusters:
    groups:
        - group:
            - cluster-name: my-cluster
              num-nodes: 1
              products:
                groups:
                    group:
                        - product: my-service
                          version-or-branch: some-github-branch
                          install-type: source
                          start: true
              
```

A scenario might look something like this:
```yaml
profile: /path/to/a/profile.yaml
next:
    - profile: /path/to/another/profile.yaml
      in:
        REQUIRED_FOO: PROVIDED_FOO
    - profile: /path/to/yet/another/profile.yaml
      isolate: true
      in:
        REQUIRED_BAR: PROVIDED_BAR
        REQUIRED_BAZ: PROVIDED_BAZ
      next:
        - profile: /another/profile.yaml
          in:
            REQUIRED_QUUX: PROVIDED_QUUX
```
### Using [leiningen](http://leiningen.org):

```bash
lein run /path/to/some/scenario.yaml
```


