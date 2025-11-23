open SExpression
open! SMoL
open! Primitive

type primitive = Primitive.t

@module("./random") external make_random: string => unit => float = "make_random"

let randomIntOfRandom = random => {
  let f = (start, end) => {
    let delta = end - start
    let offset = Js.Math.round(random() *. Int.toFloat(delta))->Int.fromFloat
    start + offset
  }
  f
}

let makeRandomInt = seed => {
  randomIntOfRandom(make_random(seed))
}

let randomInt = ref(makeRandomInt(Js.Math.random()->Float.toString))

let used_addresses = ref(Set.make())

let gc = ref(false)

type pile<'topping, 'base> = {topping: list<'topping>, base: 'base}
let new_pile = base => {topping: list{}, base}
let add_pile = (new_topping, {topping, base}) => {topping: list{new_topping, ...topping}, base}

type bodyBaseNode =
  | BDef(annotated<symbol, printAnn>, (unit, sourceLocation), block<printAnn>)
  | BExp((unit, sourceLocation), block<printAnn>)
  | BRet((unit, sourceLocation))
type bodyBaseBase = annotated<bodyBaseNode, printAnn>
type programBaseNode =
  | PDef(annotated<symbol, printAnn>, (unit, sourceLocation), program<printAnn>)
  | PExp((unit, sourceLocation), program<printAnn>)
type programBase = annotated<programBaseNode, printAnn>

let holeOfBodyBase = (bodyBaseNode: bodyBaseNode): sourceLocation => {
  switch bodyBaseNode {
  | BDef(_, ((), sourceLocation), _) => sourceLocation
  | BExp(((), sourceLocation), _) => sourceLocation
  | BRet(((), sourceLocation)) => sourceLocation
  }
}
let holeOfProgramBase = (programBaseNode: programBaseNode): sourceLocation => {
  switch programBaseNode {
  | PDef(_, ((), sourceLocation), _) => sourceLocation
  | PExp(((), sourceLocation), _) => sourceLocation
  }
}

module EnvironmentID = {
  type t =
    | Primordial
    | TopLevel
    | Extended(int)
  let toString = envID => {
    switch envID {
    | Primordial => "primordial-env"
    | TopLevel => "top-level"
    | Extended(i) => `${Int.toString(i)}`
    }
  }
}

type rec environmentFrame = {
  id: EnvironmentID.t,
  useful: ref<bool>,
  content: array<(annotated<symbol, printAnn>, ref<(bool, option<value>)>)>,
}
and generatorStatus =
  | Fresh(block<printAnn>, environment)
  | Suspended(pile<contextFrame, bodyBaseBase>, environment)
  | Running
  | Done
and bodyBase = {isGen: option<generator>, base: bodyBaseBase}
and environment = list<environmentFrame>
and vector = {
  id: int,
  useful: ref<bool>,
  contents: array<(bool, value)>,
}
and function = {
  id: int,
  useful: ref<bool>,
  isGen: bool,
  name: ref<option<string>>,
  sourceLocation: kindedSourceLocation,
  xs: array<annotated<symbol, printAnn>>,
  body: block<printAnn>,
  env: environment,
  print: print<kindedSourceLocation>,
}
and generator = {
  id: int,
  useful: ref<bool>,
  status: ref<generatorStatus>,
}
and value =
  // Constants
  | Con(constant)
  // Functions
  | VFun(function)
  | VGen(generator)
  // Vectors
  | Vec(vector)
and contextFrameNode =
  | Yield1((unit, sourceLocation))
  | Set1(annotated<symbol, printAnn>, (unit, sourceLocation))
  | App1((unit, sourceLocation), list<expression<printAnn>>)
  | App2(
      (value, sourceLocation),
      list<(value, sourceLocation)>,
      (unit, sourceLocation),
      list<expression<printAnn>>,
    )
  | AppPrm1(
      SMoL.Primitive.t,
      list<(value, sourceLocation)>,
      (unit, sourceLocation),
      list<expression<printAnn>>,
    )
  | Let1(
      list<(annotated<symbol, printAnn>, (value, sourceLocation))>,
      (annotated<symbol, printAnn>, (unit, sourceLocation)),
      list<bind<printAnn>>,
      block<printAnn>,
    )
  | If1((unit, sourceLocation), expression<printAnn>, expression<printAnn>)
  | Cnd1(
      (unit, sourceLocation),
      block<printAnn>,
      list<(expression<printAnn>, block<printAnn>)>,
      option<block<printAnn>>,
    )
  | Bgn1((unit, sourceLocation), list<expression<printAnn>>, expression<printAnn>)
  | While1((unit, sourceLocation), list<expression<printAnn>>, expression<printAnn>)
and contextFrame = annotated<contextFrameNode, printAnn>

type frame<'base> = {ctx: pile<contextFrame, 'base>, env: environment}

let rmSrcLoc = ((it, _): ('a, sourceLocation)): 'a => {
  it
}

let holeOfFrame = (f: contextFrame): sourceLocation => {
  switch f.it {
  | Yield1((_, srcLoc)) => srcLoc
  | Set1(_, ((), srcLoc)) => srcLoc
  | App1(((), srcLoc), _) => srcLoc
  | App2(_, _, ((), srcLoc), _) => srcLoc
  | AppPrm1(_, _, ((), srcLoc), _) => srcLoc
  | Let1(_, (_, (_, srcLoc)), _, _) => srcLoc
  | If1(((), srcLoc), _, _) => srcLoc
  | Cnd1(((), srcLoc), _, _, _) => srcLoc
  | Bgn1(((), srcLoc), _, _) => srcLoc
  | While1(((), srcLoc), _, _) => srcLoc
  }
}

let valuesOfFrame = (f: contextFrame): list<(value, sourceLocation)> => {
  switch f.it {
  | Yield1(_) => list{}
  | Set1(_, _) => list{}
  | App1(_, _) => list{}
  | App2(v, vs, _, _) => list{v, ...vs}
  | AppPrm1(_, vs, _, _) => vs
  | Let1(xvs, _, _, _) => List.map(xvs, ((_, v)) => v)
  | If1(_, _, _) => list{}
  | Cnd1(_, _, _, _) => list{}
  | Bgn1(_, _, _) => list{}
  | While1(_, _, _) => list{}
  }
}

let printCon = constant => SMoLPrinter.printOutput(list{SMoL.OVal(Con(constant))})

// This is the honest display of values
let printValue = (v: value) => {
  switch v {
  | Con(constant) => printCon(constant)
  | VFun({id}) => `@${Int.toString(id)}`
  | VGen({id}) => `@${Int.toString(id)}`
  | Vec({id}) => `@${Int.toString(id)}`
  }
}

let makePrimitiveName = name => {
  {
    it: name,
    ann: {
      print: Print.dummy(Plain(name)),
      sourceLocation: {begin: {ln: 0, ch: 0}, end: {ln: 0, ch: 0}},
    },
  }
}

let initialEnv: environment = list{
  {
    id: Primordial,
    useful: ref(true),
    content: [],
  },
}

type result =
  | Err
  | Val(value)

module RTArity = {
  type t =
    | AtLeast(int)
    | Exactly(int)
  let toString = arity => {
    switch arity {
    | AtLeast(i) => `${Int.toString(i)} or more`
    | Exactly(i) => `${Int.toString(i)}`
    }
  }
  let minimalArity = arity => {
    switch arity {
    | AtLeast(i) => i
    | Exactly(i) => i
    }
  }
}
type arity = RTArity.t

type runtimeError =
  | UnboundIdentifier(symbol)
  | RedefinedIdentifier(symbol, string) // the string is the environment id
  | UsedBeforeInitialization(symbol)
  | ExpectButGiven(string, value)
  | ArityMismatch(arity, int)
  | OutOfBound(int, int)
  | DivisionByZero
  | AnyError(string)
  | WIP(string)
exception RuntimeError(runtimeError)

let raiseRuntimeError = e => raise(RuntimeError(e))

// This is the pretty presentation of values
let outputletOfValue = (v: value): outputlet => {
  let hMap = Map.make()
  let rec p = (visited: list<int>) => (v: value): val => {
    switch v {
    | Con(constant) => Con(constant)
    | VFun(_) => raise(RuntimeError(AnyError("Can't print functions")))
    | VGen(_) => raise(RuntimeError(AnyError("Can't print generators")))
    | Vec({id, contents: es}) =>
      if List.has(visited, id, (a, b) => a == b) {
        let r = switch Map.get(hMap, id) {
        | None => {
            let r = Map.size(hMap)
            Map.set(hMap, id, r)
            r
          }
        | Some(r) => r
        }
        Ref(r)
      } else {
        let p = p(list{id, ...visited})
        let es = List.fromArray(Array.map(es, ((_recentlyChanged, v)) => p(v)))
        Struct(Map.get(hMap, id), Vec(es))
      }
    }
  }
  OVal(p(list{})(v))
}

let xsOfDef = (d: definition<printAnn>) => {
  switch d.it {
  | Var(x, _e) => list{x}
  | Fun(x, _ys, _b) => list{x}
  | GFun(x, _ys, _b) => list{x}
  }
}

let xsOfTerms = (ts: list<term<printAnn>>) => {
  open Js.List

  toVector(flatten(map(trm =>
        switch (trm.it: termNode<printAnn>) {
        | SMoL.Def(d) => xsOfDef(d)
        | SMoL.Exp(_e) => list{}
        }
      , ts)))
}

// let extend_block = (b: block<printAnn>, e: expression<printAnn>): blockNode<printAnn> => {
//   let (ts, e0) = b.it
//   (list{...ts, SMoL.Exp(e0)}, e)
// }

let first_not_used_addressed = (gen: unit => int) => {
  let rec loop = () => {
    let addr = gen()
    if Set.has(used_addresses.contents, addr) {
      loop()
    } else {
      Set.add(used_addresses.contents, addr)
      addr
    }
  }
  () => loop()
}

let newHavId = first_not_used_addressed(() => randomInt.contents(100, 1000))
let newEnvId = first_not_used_addressed(() => randomInt.contents(1000, 10000))

let firstState = ref(true)

let allEnvs = ref(list{})
let unmarkEnvs = () => {
  allEnvs.contents->List.forEach(env => {
    env
    ->List.head
    ->Option.forEach(frame => {
      frame.content->Array.forEach(
        ((_x, box)) => {
          let (_recentlyChanged, v) = box.contents
          box.contents = (false, v)
        },
      )
    })
  })
}

type heap = ref<list<value>>
// hav = Heap-Allocated Values
let allHavs: ref<list<value>> = ref(list{})
let unmarkHavs = () => {
  allHavs.contents->List.forEach(v => {
    switch v {
    | Vec(v) => {
        let v = v.contents
        let changed = v->Array.map(((recentlyChanged, _v)) => recentlyChanged)
        changed->Array.forEachWithIndex((changed, i) => {
          if changed {
            let (_, e) = v[i]->Option.getExn
            v[i] = (false, e)
          }
        })
      }
    | _ => ()
    }
  })
}

let printTopLevel = ref(true)

// the bool tells whether the message is an error
let stdout: ref<output> = ref(list{})
let printStdout = s => {
  stdout := list{s, ...stdout.contents}
}

let rec appear_in = (x, ys) => {
  switch ys {
  | list{} => false
  | list{y, ...ys} => x == y || appear_in(x, ys)
  }
}

let rec has_duplicates = (xs: list<symbol>) => {
  switch xs {
  | list{} => None
  | list{x, ...xs} =>
    if appear_in(x, xs) {
      Some(x)
    } else {
      has_duplicates(xs)
    }
  }
}

let check_duplicate = (xs, env_id) => {
  switch has_duplicates(List.fromArray(xs)) {
  | Some(x) => raise(RuntimeError(RedefinedIdentifier(x, EnvironmentID.toString(env_id))))
  | None => ()
  }
}

let makeTopLevel = (env, xs): environment => {
  // Like extend but the id is hard-coded to be top-level
  let id = EnvironmentID.TopLevel
  let frm = {
    id,
    useful: ref(true),
    content: Js.Array.map(x => (x, ref((false, None))), xs),
  }
  let env = list{frm, ...env}
  allEnvs := list{env, ...allEnvs.contents}
  check_duplicate(xs->Array.map(x => x.it), id)
  env
}

let extend = (env, xs): environment => {
  // if Array.length(xs) == 0 {
  // env
  // } else {
  let id = EnvironmentID.Extended(newEnvId())
  let frm = {
    id,
    useful: ref(true),
    content: Js.Array.map(x => (x, ref((false, None))), xs),
  }
  let env = list{frm, ...env}
  allEnvs := list{env, ...allEnvs.contents}
  check_duplicate(xs->Array.map(x => x.it), id)
  env
  // }
}

let ifNone = (thunk: unit => 'X, o: option<'X>): 'X => {
  switch o {
  | Some(x) => x
  | None => thunk()
  }
}

let rec lookup = (env: environment, x) => {
  switch env {
  | list{} => raise(RuntimeError(UnboundIdentifier(x)))
  | list{frm, ...env} =>
    let {content} = frm
    // Js.log(`Looking up ${x} in ${id}`)

    ifNone(
      () => lookup(env, x),
      Js.Option.map(((_x, v)) => v, Js.Array.find(((y, _v)) => x == y.it, content)),
    )
  }
}

let doRef = (env, x) => {
  let (_, v) = lookup(env, x).contents
  switch v {
  | None => raise(RuntimeError(UsedBeforeInitialization(x)))
  | Some(v) => v
  }
}

let doSetAbs = (mark: bool, env: environment, x: annotated<symbol, printAnn>, v: value) => {
  switch v {
  | VFun({name}) =>
    switch name.contents {
    | None => name := Some(x.it)
    | _ => ()
    }

  | _ => ()
  }
  lookup(env, x.it).contents = (mark, Some(v))
}
let doSetArg = (env: environment, x: annotated<symbol, printAnn>, v: value) => {
  // Setting function call arguments does not need to mark the new binding as
  // users have seen the value when stacker pauses right before entering the
  // function body
  doSetAbs(false, env, x, v)
}
let doSetVar = (env: environment, x: annotated<symbol, printAnn>, v: value) => {
  doSetAbs(true, env, x, v)
}

type stack = pile<frame<bodyBase>, frame<programBase>>

let current_env = (stk: stack): environment => {
  switch stk.topping->List.head->Option.map(f => f.env) {
  | Some(env) => env
  | None => stk.base.env
  }
}

let consCtx = (f: contextFrame, stk: stack) => {
  let {topping, base} = stk
  switch topping {
  | list{} => {
      let base = {...base, ctx: add_pile(f, base.ctx)}
      {topping: list{}, base}
    }
  | list{bodyFrame, ...topping} => {
      let bodyFrame = {...bodyFrame, ctx: add_pile(f, bodyFrame.ctx)}
      {topping: list{bodyFrame, ...topping}, base}
    }
  }
}

type terminated_state =
  | Err(runtimeError)
  | Tm
type entrance =
  | Let
  | App
let string_of_entrace = entrance =>
  switch entrance {
  | Let => "a `let` body"
  | App => "a function body"
  }
type redexNode =
  | Applying(value, list<value>)
  | Setting(annotated<symbol, printAnn>, value)
  | VecSetting(vector, int, value)
  | Printing(value)
  | Yielding(value)
  | Nexting(generator)
type redex = annotated<redexNode, printAnn>
type continuing_state =
  // no env and no ctx
  | Returning(value, stack)
  // no env
  | Entering(entrance, block<printAnn>, environment, stack)
  // all in
  | Reducing(redex, stack)
// a state always includes the heap. So we manage the heap as a global reference.
type state =
  | Terminated(terminated_state)
  | Continuing(continuing_state)

let makeFun = (isGen, xs, b, env, sourceLocation, print) => {
  let id = newHavId()
  let v: value = VFun({
    id,
    isGen,
    useful: ref(true),
    xs,
    body: b,
    env,
    name: ref(None),
    sourceLocation,
    print,
  })
  allHavs := list{v, ...allHavs.contents}
  v
}

let asNum = (v: value) =>
  switch v {
  | Con(Num(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("number", v)))
  }
let asStr = (v: value) =>
  switch v {
  | Con(Str(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("string", v)))
  }
let asLgc = (v: value) =>
  switch v {
  | Con(Lgc(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("boolean", v)))
  }
and asFun = (v: value) =>
  switch v {
  | VFun(f) => f
  | _else => raise(RuntimeError(ExpectButGiven("function", v)))
  }
and asGen = (v: value) =>
  switch v {
  | VGen(udg) => udg
  | _else => raise(RuntimeError(ExpectButGiven("generator", v)))
  }
and asVec = v =>
  switch v {
  | Vec(v) => v
  | _else => raise(RuntimeError(ExpectButGiven("vector", v)))
  }
and asPair = v =>
  switch v {
  | Vec(v) => {
      let {contents: es} = v
      if Array.length(es) != 2 {
        raise(RuntimeError(ExpectButGiven("pair", Vec(v))))
      }
      v
    }
  | _else => raise(RuntimeError(ExpectButGiven("vector", v)))
  }

let deltaNum1 = (f, v, vs): value => {
  open Js.List
  let v = asNum(v)
  and vs = map(v => asNum(v), vs)
  Con(Num(Js.List.foldLeft(f, v, vs)))
}
let deltaNum2 = (f, v1, v2, vs): value => {
  open Js.List
  let v1 = asNum(v1)
  and v2 = asNum(v2)
  and vs = map(v => asNum(v), vs)
  Con(Num(Js.List.foldLeft(f, f(v1, v2), vs)))
}

type acc = {
  v1: float,
  vvs: (float, float),
}
let deltaCmp = (cmp, v, vs): value => {
  open Js.List
  let v = asNum(v)
  and vs = map(v => asNum(v), vs)
  let rec loop = (v1, vs) => {
    switch vs {
    | list{} => true
    | list{v2, ...vs} => cmp(v1, v2) && loop(v2, vs)
    }
  }
  Con(Lgc(loop(v, vs)))
}

let eqv2 = (u, v) => {
  u == v
}

let equal2 = (u: value, v: value) => {
  // Core.Set is reliable on strings but not on tuples
  let visited = Set.make()
  let fmt = (i, j) => `${Int.toString(i)}|${Int.toString(j)}`
  let rec f = (u, v) => {
    switch (u, v) {
    | (Vec(u), Vec(v)) =>
      if u.id === v.id || Set.has(visited, fmt(u.id, v.id)) {
        true
      } else {
        Set.add(visited, fmt(u.id, v.id))
        let u = u.contents
        let v = v.contents
        if Array.length(u) != Array.length(v) {
          false
        } else {
          Array.everyWithIndex(u, (ue, i) => {
            let ve = Option.getExn(v[i])
            let (_, ue) = ue
            let (_, ve) = ve
            f(ue, ve)
          })
        }
      }
    | _ => u == v
    }
  }
  f(u, v)
}

let deltaEq = (cmp, v, vs): value => {
  let rec loop = (v1, vs) => {
    switch vs {
    | list{} => true
    | list{v2, ...vs} => cmp(v1, v2) && loop(v2, vs)
    }
  }
  Con(Lgc(loop(v, vs)))
}

let arityOf = p =>
  switch p {
  | Maybe => RTArity.Exactly(0)
  | Arith(Add) => AtLeast(1)
  | Arith(Sub) => AtLeast(2)
  | Arith(Mul) => AtLeast(1)
  | Arith(Div) => AtLeast(2)
  | Cmp(_) => AtLeast(2)
  | VecNew => AtLeast(0)
  | VecRef => Exactly(2)
  | VecSet => Exactly(3)
  | VecLen => Exactly(1)
  | Err => Exactly(1)
  | Not => Exactly(1)
  | PairNew => Exactly(2)
  | PairRefLeft | PairRefRight => Exactly(1)
  | PairSetLeft | PairSetRight => Exactly(2)
  | Print => Exactly(1)
  | Next => Exactly(1)
  | Cons => Exactly(2)
  | ZeroP => Exactly(1)
  | StringAppend => AtLeast(0)
  | List => AtLeast(0)
  | EmptyP => Exactly(1)
  | First => Exactly(1)
  | Rest => Exactly(1)
  }

exception Impossible(string)
// The current function call finished. We are returning
// a value.
let rec return = (v: value) => (s: stack): state => {
  switch s {
  | {topping: list{}, base: {ctx, env}} => continueTopLevel(v, ctx, env)
  | {topping: list{{ctx, env}, ...topping}, base} => continueBody(v, ctx, env, {topping, base})
  }
}
and yield = (v: value, s: stack): state => {
  switch s {
  | {topping: list{}, base: _} => raise(RuntimeError(AnyError("yielding from the top-level block")))
  | {topping: list{{ctx, env}, ...topping}, base} =>
    switch ctx.base.isGen {
    | None => raise(RuntimeError(AnyError("yielding from a non-generative function")))
    | Some({status: r}) =>
      switch r.contents {
      | Running => {
          r := Suspended({topping: ctx.topping, base: ctx.base.base}, env)
          return(v)({topping, base})
        }
      | _ => raise(RuntimeError(AnyError("Internal error, please contact the developer")))
      }
    }
  }
}
and make_vector = (vs: list<value>) => {
  let id = newHavId()
  let v = Vec({id, useful: ref(true), contents: vs->List.map(v => (false, v))->List.toArray})
  allHavs := list{v, ...allHavs.contents}
  return(v)
}
and delta = (ann, p, vs) =>
  switch (p, vs) {
  | (Maybe, list{}) => return(Con(Lgc(Math.random() >= 0.5)))
  | (Arith(Add), list{v, ...vs}) => return(deltaNum1((a, b) => a +. b, v, vs))
  | (Arith(Sub), list{v1, v2, ...vs}) => return(deltaNum2((a, b) => a -. b, v1, v2, vs))
  | (Arith(Mul), list{v, ...vs}) => return(deltaNum1((a, b) => a *. b, v, vs))
  | (Arith(Div), list{v1, v2, ...vs}) => return(deltaNum2((a, b) => {
        if b == 0. {
          raise(RuntimeError(DivisionByZero))
        } else {
          a /. b
        }
      }, v1, v2, vs))
  | (Cmp(Lt), list{v, ...vs}) => return(deltaCmp((a, b) => a < b, v, vs))
  | (Cmp(NumEq), list{v, ...vs}) => return(deltaEq(eqv2, v, vs))
  | (Cmp(Eq), list{v, ...vs}) => return(deltaEq(eqv2, v, vs))
  | (Cmp(Equal), list{v, ...vs}) => return(deltaEq(equal2, v, vs))
  | (Cmp(Gt), list{v, ...vs}) => return(deltaCmp((a, b) => a > b, v, vs))
  | (Cmp(Le), list{v, ...vs}) => return(deltaCmp((a, b) => a <= b, v, vs))
  | (Cmp(Ge), list{v, ...vs}) => return(deltaCmp((a, b) => a >= b, v, vs))
  | (Cmp(Ne), list{v, ...vs}) => return(deltaCmp((a, b) => a != b, v, vs))
  | (VecNew, vs) => {
      let id = newHavId()
      let v = Vec({id, useful: ref(true), contents: vs->List.map(v => (false, v))->List.toArray})
      allHavs := list{v, ...allHavs.contents}
      return(v)
    }

  | (VecRef, list{v_vec, v_ind}) => {
      let {contents: vs} = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      switch vs[v_ind] {
      | None => raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
      | Some((_, v)) => return(v)
      }
    }

  | (VecLen, list{v}) => {
      let {contents: vs} = asVec(v)
      return(Con(Num(vs->Array.length->Int.toFloat)))
    }

  | (VecSet, list{v_vec, v_ind, v_val}) => {
      let v_vec = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      stk => Continuing(
        Reducing(
          {
            it: VecSetting(v_vec, v_ind, v_val),
            ann,
          },
          stk,
        ),
      )
    }

  | (Err, list{v}) => {
      let v = asStr(v)
      _stk => Terminated(Err(AnyError(v)))
    }

  | (Not, list{v}) => {
      let v = asLgc(v)
      return(Con(Lgc(!v)))
    }

  | (PairNew, list{v1, v2}) => make_vector(list{v1, v2})

  | (PairRefLeft, list{v}) => {
      let {contents: vs} = asPair(v)
      let v_ind = 0
      switch vs[v_ind] {
      | None => raise(Impossible("we have checked that this value is a pair"))
      | Some((_, v)) => return(v)
      }
    }

  | (PairRefRight, list{v}) => {
      let {contents: vs} = asPair(v)
      let v_ind = 1
      switch vs[v_ind] {
      | None => raise(Impossible("we have checked that this value is a pair"))
      | Some((_, v)) => return(v)
      }
    }

  | (PairSetLeft, list{v_vec, v_val}) => {
      let v_vec = asPair(v_vec)
      let v_ind = 0
      stk => Continuing(
        Reducing(
          {
            it: VecSetting(v_vec, v_ind, v_val),
            ann,
          },
          stk,
        ),
      )
    }

  | (PairSetRight, list{v_vec, v_val}) => {
      let v_vec = asPair(v_vec)
      let v_ind = 1
      stk => Continuing(
        Reducing(
          {
            it: VecSetting(v_vec, v_ind, v_val),
            ann,
          },
          stk,
        ),
      )
    }

  | (Print, list{v}) =>
    stk => Continuing(
      Reducing(
        {
          it: Printing(v),
          ann,
        },
        stk,
      ),
    )

  // Js.Console.log(outputletOfValue(v))
  // return(Con(Uni))

  | (Next, list{v}) =>
    stk => {
      Continuing(
        Reducing(
          {
            it: Nexting(asGen(v)),
            ann,
          },
          stk,
        ),
      )
    }

  | _otherwise => {
      let wantedArity = arityOf(p)
      let actualArity = List.length(vs)
      if RTArity.minimalArity(wantedArity) != actualArity {
        raise(RuntimeError(ArityMismatch(wantedArity, actualArity)))
      } else {
        raise(RuntimeError(AnyError(`Internal error with ${SMoL.Primitive.toString(p)}`)))
      }
    }
  }
and doVecSet = ({contents: vs}, v_ind, v_val, stk: stack) => {
  if 0 <= v_ind && v_ind < Array.length(vs) {
    vs[v_ind] = v_val
    // The update is successful.
    return(Con(Uni))(stk)
  } else {
    raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
  }
}
and setting = (x, v, stk: stack) => {
  doSetVar(current_env(stk), x, v)
  return(Con(Uni))(stk)
}
and continueTopLevel = (v: value, ctx: pile<contextFrame, programBase>, env: environment) => {
  let {topping, base} = ctx
  switch topping {
  | list{} =>
    switch base.it {
    | PDef(x, ((), _srcLoc), p) =>
      doSetVar(env, x, v)
      transitionPrg(p, env)
    | PExp(((), _srcLoc), p) =>
      if printTopLevel.contents {
        switch v {
        | Con(Uni) => transitionPrg(p, env)
        | v =>
          Continuing(
            Reducing(
              {
                it: Printing(v),
                ann: base.ann,
              },
              new_pile({env, ctx}),
            ),
          )
        }
      } else {
        transitionPrg(p, env)
      }
    }
  | list{ctxFrame, ...topping} => {
      let stk: stack = {topping: list{}, base: {ctx: {topping, base}, env}}
      handleCtxFrame(v, ctxFrame, stk)
    }
  }
}
and continueBody = (v: value, ctx, env, stk): state => {
  let {topping, base: {isGen, base}} = ctx
  switch topping {
  | list{} =>
    switch base.it {
    | BRet((), _srcLoc) => {
        isGen->Option.forEach(({status}) => {
          status := Done
        })
        Continuing(Returning(v, stk))
      }
    | BExp(((), _srcLoc), b) => transitionBlock(b, isGen, env, stk)
    | BDef(x, ((), _srcLoc), b) => {
        doSetVar(env, x, v)
        transitionBlock(b, isGen, env, stk)
      }
    }
  | list{ctxFrame, ...topping} =>
    handleCtxFrame(v, ctxFrame, add_pile({ctx: {topping, base: {isGen, base}}, env}, stk))
  }
}
and handleCtxFrame = (v: value, ctxFrame, stk: stack) => {
  switch ctxFrame.it {
  | Set1(x, ((), _srcLoc)) =>
    Continuing(
      Reducing(
        {
          it: Setting(x, v),
          ann: ctxFrame.ann,
        },
        stk,
      ),
    )
  | Let1(xvs, (x, ((), srcLoc)), xes, b) =>
    transitionLet(ctxFrame.ann, list{(x, (v, srcLoc)), ...xvs}, xes, b, stk)
  | App1(((), srcLoc), exps) =>
    let fun = v
    and vals = list{}
    transitionApp(ctxFrame.ann, (fun, srcLoc), vals, exps, stk)
  | App2(fun, vals, ((), srcLoc), exps) =>
    let vals = list{(v, srcLoc), ...vals}
    transitionApp(ctxFrame.ann, fun, vals, exps, stk)
  | AppPrm1(fun, vals, ((), srcLoc), exps) =>
    let vals = list{(v, srcLoc), ...vals}
    transitionAppPrm(ctxFrame.ann, fun, vals, exps, stk)
  | Cnd1(((), _srcLoc), b, ebs, ob) =>
    switch asLgc(v) {
    | true => doBlk(b, None, stk)
    | false => transitionCnd(ctxFrame.ann, ebs, ob, stk)
    }
  | If1(((), _srcLoc), e_thn, e_els) =>
    switch asLgc(v) {
    | true => doEv(e_thn, stk)
    | false => doEv(e_els, stk)
    }
  | While1(((), _srcLoc), es_thn, itself) =>
    switch asLgc(v) {
    | true => transitionBgn(ctxFrame.ann, es_thn, itself, stk)
    | false => return(Con(Uni))(stk)
    }
  | Bgn1(((), _srcLoc), es, e) => transitionBgn(ctxFrame.ann, es, e, stk)
  | Yield1((), _srcLoc) =>
    // switch stk {}
    Continuing(
      Reducing(
        {
          it: Yielding(v),
          ann: ctxFrame.ann,
        },
        stk,
      ),
    )
  }
}
and doEv = (exp: expression<printAnn>, stk: stack) =>
  switch exp.it {
  | Con(c) => return(Con(c))(stk)
  | Ref(x) =>
    let val = doRef(current_env(stk), x)
    return(val)(stk)
  | Set(x, e) =>
    doEv(
      e,
      consCtx(
        {
          it: Set1(x, ((), e.ann.sourceLocation)),
          ann: exp.ann,
        },
        stk,
      ),
    )
  | Lam(xs, b) => {
      let v = makeFun(
        false,
        Js.List.toVector(xs),
        b,
        current_env(stk),
        {
          nodeKind: Expression,
          sourceLocation: exp.ann.sourceLocation,
        },
        exp.ann.print,
      )
      return(v)(stk)
    }
  | GLam(xs, b) => {
      let v = makeFun(
        true,
        Js.List.toVector(xs),
        b,
        current_env(stk),
        {
          nodeKind: Expression,
          sourceLocation: exp.ann.sourceLocation,
        },
        exp.ann.print,
      )
      return(v)(stk)
    }
  | Let(LetKind.Plain, xes, b) => transitionLet(exp.ann, list{}, xes, b, stk)
  | Let(LetKind.Nested, _xes, _b) => raiseRuntimeError(WIP("let*"))
  | Let(LetKind.Recursive, xes, b) => transitionLetrec(exp.ann, xes, b, stk)

  | Bgn(es, e) => transitionBgn(exp.ann, es, e, stk)

  | AppPrm(p, es) => transitionAppPrm(exp.ann, p, list{}, es, stk)
  | App(e, es) =>
    doEv(
      e,
      consCtx(
        {
          it: App1(((), e.ann.sourceLocation), es),
          ann: exp.ann,
        },
        stk,
      ),
    )
  | Cnd(ebs, ob) => transitionCnd(exp.ann, ebs, ob, stk)
  | If(e_cnd, e_thn, e_els) =>
    doEv(
      e_cnd,
      consCtx(
        {
          it: If1(((), e_cnd.ann.sourceLocation), e_thn, e_els),
          ann: exp.ann,
        },
        stk,
      ),
    )
  | Yield(e) =>
    doEv(
      e,
      consCtx(
        {
          it: Yield1((), e.ann.sourceLocation),
          ann: exp.ann,
        },
        stk,
      ),
    )
  | And(_es) => raiseRuntimeError(WIP("logical and"))
  | Or(_es) => raiseRuntimeError(WIP("logical or"))
  | While(e_cnd, es_thn) =>
    doEv(
      e_cnd,
      consCtx(
        {
          it: While1(((), e_cnd.ann.sourceLocation), es_thn, exp),
          ann: exp.ann,
        },
        stk,
      ),
    )
  }
and transitionLetrec = (_ann, _xes: list<bind<printAnn>>, _b: block<printAnn>, _stk: stack) => {
  raiseRuntimeError(AnyError("letrec is no longer supported"))
}
and transitionLet = (ann, xvs, xes: list<bind<printAnn>>, b, stk: stack) => {
  switch xes {
  | list{} => {
      let xvs = xvs->List.reverse->List.toArray
      let xs = xvs->Array.map(((x, _v)) => x)
      let env = extend(current_env(stk), Array.concat(xs, List.toArray(xsOfBlock(b))))
      xvs->Array.forEach(((x, v)) => {
        doSetVar(env, x, rmSrcLoc(v))
      })
      Continuing(entering(Let, b, env, stk))
    }

  | list{{it: (x, e)}, ...xes} =>
    doEv(
      e,
      consCtx(
        {
          ann,
          it: Let1(xvs, (x, ((), e.ann.sourceLocation)), xes, b),
        },
        stk,
      ),
    )
  }
}
and transitionBlock = ({it: b, ann}: block<printAnn>, isGen, env: environment, stk: stack) => {
  switch b {
  | BRet(exp) =>
    doEv(
      exp,
      add_pile(
        {ctx: new_pile({isGen, base: {it: BRet((), exp.ann.sourceLocation), ann}}), env},
        stk,
      ),
    )
  | BCons(t, b) =>
    switch t.it {
    | Exp(exp) =>
      // Js.Console.log(exp.ann.sourceLocation)
      doEv(
        exp,
        add_pile(
          {ctx: new_pile({isGen, base: {ann, it: BExp(((), exp.ann.sourceLocation), b)}}), env},
          stk,
        ),
      )
    | Def(d) =>
      switch d.it {
      | Var(x, exp) =>
        doEv(
          exp,
          add_pile(
            {
              ctx: new_pile({isGen, base: {ann, it: BDef(x, ((), exp.ann.sourceLocation), b)}}),
              env,
            },
            stk,
          ),
        )
      | Fun(f, xs, fb) => {
          let v = makeFun(
            false,
            xs->List.toArray,
            fb,
            env,
            {
              nodeKind: Definition,
              sourceLocation: ann.sourceLocation,
            },
            d.ann.print,
          )
          doSetVar(env, f, v)
          transitionBlock(b, isGen, env, stk)
        }
      | GFun(f, xs, fb) => {
          let v = makeFun(
            true,
            xs->List.toArray,
            fb,
            env,
            {
              nodeKind: Definition,
              sourceLocation: ann.sourceLocation,
            },
            d.ann.print,
          )
          doSetVar(env, f, v)
          transitionBlock(b, isGen, env, stk)
        }
      }
    }
  }
}
and transitionPrg = ({ann, it: p}, env: environment) => {
  switch p {
  | PNil => Terminated(Tm)
  | PCons(t, p) =>
    switch t.it {
    | Exp(exp) =>
      // Js.Console.log(exp.ann.sourceLocation)
      doEv(exp, new_pile({ctx: new_pile({ann, it: PExp(((), exp.ann.sourceLocation), p)}), env}))
    | Def(d) =>
      switch d.it {
      | Var(x, exp) =>
        doEv(
          exp,
          new_pile({
            ctx: new_pile({ann, it: PDef(x, ((), exp.ann.sourceLocation), p)}),
            env,
          }),
        )
      | Fun(f, xs, fb) => {
          let v = makeFun(
            false,
            xs->List.toArray,
            fb,
            env,
            {
              nodeKind: Definition,
              sourceLocation: d.ann.sourceLocation,
            },
            d.ann.print,
          )
          doSetVar(env, f, v)
          transitionPrg(p, env)
        }
      | GFun(f, xs, fb) => {
          let v = makeFun(
            true,
            xs->List.toArray,
            fb,
            env,
            {
              nodeKind: Definition,
              sourceLocation: d.ann.sourceLocation,
            },
            d.ann.print,
          )
          doSetVar(env, f, v)
          transitionPrg(p, env)
        }
      }
    }
  }
}
and transitionCnd = (ann, ebs, ob, stk: stack) => {
  switch ebs {
  | list{} =>
    switch ob {
    | None => return(Con(Uni))(stk)
    | Some(b) => doBlk(b, None, stk)
    }
  | list{(e, b), ...ebs} => {
      let exp = e
      doEv(
        exp,
        consCtx(
          {
            ann,
            it: Cnd1(((), e.ann.sourceLocation), b, ebs, ob),
          },
          stk,
        ),
      )
    }
  }
}
and transitionBgn = (ann, es, e, stk: stack) => {
  switch es {
  | list{} => doEv(e, stk)
  | list{e0, ...es} =>
    doEv(
      e0,
      consCtx(
        {
          ann,
          it: Bgn1(((), e0.ann.sourceLocation), es, e),
        },
        stk,
      ),
    )
  }
}
// and doLoop = (e, b, exp, stk: stack) => {
//   let e1: expression<printAnn> = e
//   let b1: block = extend_block(b, exp)
//   let b2: block = (list{}, annotate((Con(Uni): expression), exp.ann.begin, exp.ann.end))
//   let e = annotate(Cnd(list{(e1, b1)}, Some(b2)), exp.ann.begin, exp.ann.end)
//   doEv(e, stk)
// }
and doAppPrm = (ann, p, vs, stk): state => {
  delta(ann, p, vs)(stk)
}
and doApp = (v, vs, stk): state => {
  switch asFun(v) {
  | {isGen, xs, body: b, env} =>
    if Js.Array.length(xs) == Js.List.length(vs) {
      let env = extend(env, Array.concat(xs, xsOfBlock(b)->List.toArray))

      {
        Js.Array.forEachi((x, i) => {
          doSetAbs(false, env, x, Js.Option.getExn(Js.List.nth(vs, i)))
        }, xs)
      }

      if isGen {
        let id = newHavId()
        let v = VGen({id, useful: ref(true), status: ref(Fresh(b, env))})
        allHavs := list{v, ...allHavs.contents}
        Continuing(Returning(v, stk))
        // return(v)(stk)
      } else {
        Continuing(entering(App, b, env, stk))
      }
    } else {
      raise(RuntimeError(ArityMismatch(Exactly(Js.Array.length(xs)), Js.List.length(vs))))
    }
  }
}
and doPrint = (v, stk): state => {
  let v = outputletOfValue(v)
  printStdout(v)
  return(Con(Uni))(stk)
}
and entering = (entrance, b, env, stk) => {
  let stk = switch stk {
  // tail-call optimization
  | {
      topping: list{{ctx: {topping: list{}, base: {isGen: None, base: {it: BRet(_)}}}}, ...topping},
      base,
    } => {topping, base}
  | stk => stk
  }
  Entering(entrance, b, env, stk)
}
and doEntering = (b: block<printAnn>, env, stk): state => {
  transitionBlock(b, None, env, stk)
}
and transitionAppPrm = (
  ann,
  f: primitive,
  vs: list<(value, sourceLocation)>,
  es: list<expression<printAnn>>,
  stk: stack,
) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      doAppPrm(ann, f, List.map(vs, rmSrcLoc), stk)
    }

  | list{e, ...es} =>
    let exp = e
    doEv(
      exp,
      consCtx(
        {
          ann,
          it: AppPrm1(f, vs, ((), exp.ann.sourceLocation), es),
        },
        stk,
      ),
    )
  }
}
and transitionApp = (
  ann,
  f: (value, sourceLocation),
  vs: list<(value, sourceLocation)>,
  es: list<expression<printAnn>>,
  stk: stack,
) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      let f = rmSrcLoc(f)
      let vs = List.map(vs, rmSrcLoc)
      switch f {
      | _ =>
        Continuing(
          Reducing(
            {
              it: Applying(f, vs),
              ann,
            },
            stk,
          ),
        )
      }
    }

  | list{e, ...es} =>
    let exp = e
    doEv(
      exp,
      consCtx(
        {
          ann,
          it: App2(f, vs, ((), exp.ann.sourceLocation), es),
        },
        stk,
      ),
    )
  }
}
and doBlk = (b: block<printAnn>, isGen, stk: stack): state => {
  let xs = xsOfBlock(b)->List.toArray
  let env = current_env(stk)
  let env = if Array.length(xs) == 0 {
    env
  } else {
    extend(env, xs)
  }
  let stk = switch stk {
  // tail-call optimization
  | {
      topping: list{
        {ctx: {topping: list{}, base: {isGen: None, base: {it: BRet(_)}}}, env: _},
        ...topping,
      },
      base,
    } => {
      topping,
      base,
    }
  | stk => stk
  }
  transitionBlock(b, isGen, env, stk)
}

let load = (program: program<printAnn>, randomSeed: string, p: bool, recycleHeapBoxes) => {
  // initialize all global things
  firstState := true
  allEnvs := list{}
  allHavs := list{}
  stdout := list{}
  printTopLevel := p
  used_addresses := Set.make()
  gc := recycleHeapBoxes
  randomInt := makeRandomInt(randomSeed)

  // now let's get started
  let xs = List.toArray(SMoL.xsOfProgram(program))
  try {
    let env = makeTopLevel(initialEnv, xs)
    transitionPrg(program, env)
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}

let doNext = (generator, stk) => {
  let {status: r} = generator
  switch r.contents {
  | Fresh(b, env) => {
      r := Running
      transitionBlock(b, Some(generator), env, stk)
    }
  | Suspended({topping, base}, env) => {
      r := Running
      return(Con(Uni))(add_pile({ctx: {topping, base: {isGen: Some(generator), base}}, env}, stk))
    }
  | Running => raise(RuntimeError(AnyError("This generator is already running.")))

  | Done => raise(RuntimeError(AnyError("This generator is done.")))
  }
}

let reduction = (state: continuing_state): state => {
  try {
    firstState := false
    unmarkEnvs()
    unmarkHavs()
    switch state {
    | Returning(v, stk: stack) => return(v)(stk)
    | Entering(_, b, env, stk: stack) => doEntering(b, env, stk)
    | Reducing(redex, stk: stack) =>
      switch redex.it {
      | Setting(x, v) => setting(x, v, stk)
      | Nexting(gen) => doNext(gen, stk)
      | VecSetting(v, i, e) => doVecSet(v, i, (true, e), stk)
      | Applying(f, vs) => doApp(f, vs, stk)
      | Printing(v) => doPrint(v, stk)
      | Yielding(v) => yield(v, stk)
      }
    }
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}

let rec useful_value = (value: value) => {
  switch value {
  | Con(_) => ()
  | VFun(v) =>
    if !v.useful.contents {
      v.useful := true
      useful_environment(v.env)
    }
  | VGen(v) => useful_generator(v)
  | Vec(v) => useful_vector(v)
  }
}
and useful_generator = v => {
  if !v.useful.contents {
    v.useful := true
    switch v.status.contents {
    | Fresh(_, env) => useful_environment(env)
    | Suspended(_, env) => useful_environment(env)
    | _ => ()
    }
  }
}
and useful_vector = v => {
  if !v.useful.contents {
    v.useful := true
    v.contents->Array.forEach(((_, v)) => useful_value(v))
  }
}
and useful_environment = (environment: environment) => {
  switch environment {
  | list{} => ()
  | list{f, ...env} =>
    if !f.useful.contents {
      f.useful := true
      f.content->Array.forEach(((_, rhs)) => {
        let (_, v) = rhs.contents
        v->Option.forEach(useful_value)
      })
      useful_environment(env)
    }
  }
}

let useful_redex = (redex: redex) => {
  switch redex.it {
  | Applying(value, vs) => {
      useful_value(value)
      vs->List.forEach(useful_value)
    }
  | Setting(_, value) => useful_value(value)
  | VecSetting(vector, _, value) => {
      useful_vector(vector)
      useful_value(value)
    }
  | Printing(value) => useful_value(value)
  | Yielding(value) => useful_value(value)
  | Nexting(generator) => useful_generator(generator)
  }
}

let useful_body_frame = (frame: frame<bodyBase>) => useful_environment(frame.env)

let useful_program_frame = (frame: frame<programBase>) => useful_environment(frame.env)

let useful_stack = (stack: stack) => {
  stack.topping->List.forEach(useful_body_frame)
  useful_program_frame(stack.base)
}

let collect_garbage = (state: state) => {
  // Mark all environment frames as not useful
  allEnvs.contents->List.forEach(env => {
    env
    ->List.head
    ->Option.forEach(frame => {
      frame.useful := false
    })
  })
  // Mark all heap allocated value as not useful
  allHavs.contents->List.forEach(v => {
    switch v {
    | VFun(v) => v.useful := false
    | VGen(v) => v.useful := false
    | Vec(v) => v.useful := false
    | _ => ()
    }
  })
  switch state {
  | Terminated(Err(ExpectButGiven(_, value))) => useful_value(value)
  | Terminated(Err(_))
  | Terminated(Tm) => ()
  | Continuing(Returning(value, stack)) => {
      useful_value(value)
      useful_stack(stack)
    }
  | Continuing(Entering(_entrance, _block, environment, stack)) => {
      useful_environment(environment)
      useful_stack(stack)
    }
  | Continuing(Reducing(redex, stack)) => {
      useful_redex(redex)
      useful_stack(stack)
    }
  }
  // Filter out not useful environment frames
  allEnvs :=
    allEnvs.contents->List.filter(env => {
      env->List.head->Option.mapOr(true, frame => frame.useful.contents)
    })
  // Filter out not useful heap-allocated values
  allHavs :=
    allHavs.contents->List.filter(v => {
      switch v {
      | VFun(v) => v.useful.contents
      | VGen(v) => v.useful.contents
      | Vec(v) => v.useful.contents
      | _ => true
      }
    })
}

let transition = (state: continuing_state): state => {
  let state = reduction(state)
  if gc.contents {
    collect_garbage(state)
  }
  state
}
