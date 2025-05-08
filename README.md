# Stacker

[Take me to a running stacker](https://smol-tutor.xyz/stacker)

This tool presents how programs work. It focuses on presenting on the
(call) stack, environments, and the heap. Such program must be written
in SMoL, an educational language that support many language features
commonly found in modern programming languages. SMoL has *three*
surface syntaxes: Python-like, JavaScript-like, and Racket-like.
Although programs must be written in the Racket-like syntax, this tool
is able to *present* programs in two other, possibly more favorable
syntaxes.

## What is SMoL?

SMoL = *Standard Model of Languages* It includes (mutable) variables,
mutable vectors (i.e., arrays), and first-class function.

See [this document](https://docs.google.com/document/d/e/2PACX-1vTMVCrUYliicrunyxftDwv6HVmBeKaRW9-VF9Xh1GUFoHMmomOczz_RRIZXPJoH8WB66x-d4GlRvwuy/pub) for more details.

## API

All parameters are optional.

| (URL) Parameter | Type | Meaning |
| - | - | - |
| `program` | String | the program to run, written in the Lispy syntax |
| `syntax` | `Lispy` or `JavaScript` (or `JS`) or `Python` (or `PY`) | the syntax in which to present everything |
| `nNext` | Non-negative Integer | simulate clicking the `Next` button `nNext` times |
| `randomSeed` | String | the seed used to generate heap addresses |
| `readOnlyMode` | Anything | when this parameter is set, stacker will present a much simpler UI that is just enough for reading the trace |
| `printOnRequest` | Anything | when this parameter is set, stacker will *only* print values as requested by `(print v)` (rather than printing all values returned to the top-level) |

To see examples of parameters, you can run programs in the stacker and then click the share buttons.

## Limitations

### Potential Problems: limitations that might exist, but we are not sure

Programs that can be presented in Python-like syntax might or might
not have a Python-equivalent semantics.

Programs that can be presented in JavaScript-like syntax might or
might nor have a JavaScript-equivalent semantics.

### To-do: limitations that we plan to remove

See the GitHub Issue page.

### Know Issues: limitations that we don't plan to (or simply can't) resolve

Some programs can't be presented in Python-syntax. This restriction is
mostly caused by the facts that Python have no let expressions, and
that Python's `lambda` only allow exactly one expression as its body.

Python uses the same syntax for variable definition and variable
mutation. Although this design might work well most of the time, there
are programs, especially tricky programs, where disambiguation is
needed. Python disambiguates with `local` and `global` keywords. We
made some effect to insert the keywords properly, but there is likely
a few corner-cases where the Python mode doesn't faithfully represent
Python or where a SMoL program can't be translated to Python. For
example, if a `lambda` mutates a free variable, we can't translate
that to Python.

## Development

### For every fresh clone

```
npm install
```

This will install dependency packages.

### For every (local or remote) update

```sh
npm run rescript
```

This will start a ReScript program that keeps `.res` programs and
`.js` programs in sync.


```sh
npm run webpack
```

This will run a Webpack program that keeps `.css`, `.js`, etc. and the
final webpage in sync.

## File Types

- `Xyz.res`: a React component
- `xyz.res`: a module
- `webpack.config.js`: the webpack configuration

## Accessibility Consideration

### No ARIA?

https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA

> "No ARIA is better than bad ARIA."

### No tooltips?

https://inclusive-components.design/tooltips-toggletips/

> Tooltips are a last resort, where space really is at a premium

## Contributions

This software application is developed by Kuang-Chen Lu (me) and
[Shriram Krishnamurthi](https://cs.brown.edu/~sk/).

The design is originally based on the systems designed by Shriram and
[John Clements](https://www.brinckerhoff.org/). The original design
can be found in the following publication:

> Clements, John, and Shriram Krishnamurthi. "Towards a Notional
Machine for Runtime Stacks and Scope: When Stacks Don’t Stack Up."
Proceedings of the 2022 ACM Conference on International Computing
Education Research-Volume 1. 2022.

During the revision of the design, many people have contributed
feedback, notably:

- Students who attended [CSCI1730 in
  2022](https://cs.brown.edu/courses/cs173/2022/)
- Kuang-Chen's friends and/or peers: [Elijah
  Rivera](https://www.elijahrivera.com/), [Siddhartha
  Prasad](https://www.siddharthaprasad.com), [Yanyan
  Ren](https://yanyanr.github.io/), [Yanru
  Liao](https://www.linkedin.com/in/yanru-liao-7780b2243/)
- Several audiences of [the Stacker
  talk](https://youtu.be/y42WZS4spfo) in the 12th
  [RacketCon](https://con.racket-lang.org/), notably
  - [Ben Greenman](https://cs.brown.edu/people/bgreenma/)
  - [Jay McCarthy](https://jeapostrophe.github.io/)
  - [Robert Findler](https://users.cs.northwestern.edu/~robby/)
  - [Ryan Culpepper](https://www.ccs.neu.edu/home/ryanc/)
