/*

This file convert smol states to react elements.

*/

open List
open SMoL
open SExpression
open! Runtime

let stringOfKindedSourceLocation = ({nodeKind, sourceLocation}) => {
  `${NodeKind.toString(nodeKind)}-${SourceLocation.toString(sourceLocation)}`
}

let substituteById = (p: print<'id>, id: 'id, q: print<'id>): print<'id> => {
  // Js.Console.log3("Within", Print.toString(p), p)
  // Js.Console.log3("- replace", stringOfKindedSourceLocation(id), id)
  // Js.Console.log3("- with", Print.toString(q), q)
  let count = ref(0)
  let rec sub = ({ann, it}: print<'id>): print<'id> => {
    if ann == Some(id) {
      count := 1 + count.contents
      q
    } else {
      let it = switch it {
      | Plain(s) => Plain(s)
      | Group(es) => Group(List.map(es, sub))
      }
      {ann, it}
    }
  }
  let r = sub(p)
  // if count.contents != 1 {
  //   Js.Console.log("Weird thing happenned when trying to replace")
  //   Js.Console.log2("within", p)
  //   Js.Console.log2("the element", id)
  //   Js.Console.log2("with", q)
  // }
  r
}

module Syntax = {
  type t =
    | Lispy
    | Python
    | JavaScript
    | Pseudo
    | Scala
  let toString = t => {
    switch t {
    | Lispy => "Lispy"
    | Python => "Python"
    | JavaScript => "JavaScript"
    | Pseudo => "Pseudo"
    | Scala => "Scala"
    }
  }
  let fromString = s => {
    switch s {
    | "Lispy" => Some(Lispy)
    | "PY" => Some(Python)
    | "Python" => Some(Python)
    | "JS" => Some(JavaScript)
    | "JavaScript" => Some(JavaScript)
    | "PC" => Some(Pseudo)
    | "Pseudo" => Some(Pseudo)
    | "SC" => Some(Scala)
    | "Scala" => Some(Scala)
    | _ => None
    }
  }
  let all = [Lispy, Python, JavaScript, Pseudo, Scala]
}

let id = x => x

@module("./highlight") external highlight: string => unit = "highlight"
@module("./highlight") external lowlight: string => unit = "lowlight"

let array_interleave = (arr, sep) => {
  if Array.length(arr) <= 1 {
    arr
  } else {
    let prefix = [Option.getExn(arr[0])]
    let suffix = Array.flatMap(Array.slice(arr, ~start=1, ~end=Array.length(arr)), e => [sep, e])
    Array.concat(prefix, suffix)
  }
}

let re_split = (s, re) => {
  switch Js.String.match_(re, s) {
  | None => [s]
  | Some(addresses) => {
      let addresses = Array.map(addresses, a => Option.getExn(a))
      // Js.Console.log2("Found addresses", addresses)
      let result = [s]
      Array.reduce(addresses, result, (result, address) => {
        // Js.Console.log3("current result", result, address)
        result
        ->Array.mapWithIndex((resultPiece, i) => {
          if mod(i, 2) == 0 {
            // Js.Console.log3("resultPiece", resultPiece, Js.String.split(address, resultPiece))
            let resultPiece = Js.String.split(address, resultPiece)
            array_interleave(resultPiece, address)
          } else {
            [resultPiece]
          }
        })
        ->Array.flat
      })
    }
  }
}

let blankElem = e => {
  <code className="blank"> {e} </code>
}

// let reactOfPrint = (p: SMoL.print<kindedSourceLocation>): React.element => {
//   let rec reactOfAnnotatedPrint = ({it, ann}: SMoL.print<_>) => {
//     let ann = switch ann {
//     | None => it => it
//     | Some(ann) =>
//       it => {
//         let className = stringOfKindedSourceLocation(ann)
//         <span className> {it} </span>
//       }
//     }
//     switch it {
//     | Plain("") => <> </>
//     | Group(list{}) => <> </>
//     | Plain(s) => ann(React.string(s))
//     | Group(es) => ann(React.array(es->List.toArray->Array.map(reactOfAnnotatedPrint)))
//     }
//   }
//   // Js.Console.log(p)
//   blankElem(reactOfAnnotatedPrint(p))
// }

let blank = (~marked=false, s) => {
  let marked = !firstState.contents && marked
  // Js.Console.log2("before split", s)
  let s = re_split(s, %re("/@[-_0-9a-zA-Z]+/g"))
  // Js.Console.log2("after split", s)
  <code
    className={`blank ${if marked {
        "marked"
      } else {
        ""
      }}`}>
    // {React.string(String.concat("<address>", s->List.fromArray))}
    {React.array(
      s->Array.mapWithIndex((s, i) => {
        if mod(i, 2) == 0 {
          React.string(s)
        } else {
          // must be an address
          // let address = s
          let address = String.substring(s, ~start=1, ~end=String.length(s))
          <span
            className={`ref ref-${address}`}
            onMouseEnter={_ => {
              highlight(`#def-${address}`)
            }}
            onMouseLeave={_ => {
              lowlight(`#def-${address}`)
            }}>
            {React.string(s)}
          </span>
        }
      }),
    )}
  </code>
}

let defblank = s => {
  <code className="blank def"> {React.string(s)} </code>
}

let safe_f = (string_of, safe_string_of, src) => {
  switch string_of(src) {
  | dst => dst
  | exception SMoL.SMoLParseError(err) => {
      let parseFeedback = SMoL.ParseError.toString(err)
      `;; ${parseFeedback}\n${safe_string_of(src)}`
    }
  | exception SMoL.SMoLPrintError(err) => `;; ${err}\n${safe_string_of(src)}`
  }
}

exception Impossible(string)
let render = (sk, holeText, s, srcMap: kindedSourceLocation => option<sourceLocation>) => {
  let printName = x =>
    switch sk {
    | Syntax.Lispy => SMoLPrinter.printName(x)
    | JavaScript => JSPrinter.printName(x)
    | Python => PYPrinter.printName(x)
    | Pseudo => PCPrinter.printName(x)
    | Scala => SCPrinter.printName(x)
    }

  let printTerm = x =>
    switch sk {
    | Lispy => SMoLPrinter.printStandAloneTerm(x)
    | JavaScript => JSPrinter.printStandAloneTerm(x)
    | Python => PYPrinter.printStandAloneTerm(x)
    | Pseudo => PCPrinter.printStandAloneTerm(x)
    | Scala => SCPrinter.printStandAloneTerm(x)
    }

  let printOutput = x =>
    switch sk {
    | Lispy => SMoLPrinter.printOutput(~sep="\n", x)
    | JavaScript => JSPrinter.printOutput(~sep="\n", x)
    | Python => PYPrinter.printOutput(~sep="\n", x)
    | Pseudo => PCPrinter.printOutput(~sep="\n", x)
    | Scala => SCPrinter.printOutput(~sep="\n", x)
    }

  let dummyAnn = it => {
    {
      ann: {begin: {ln: 0, ch: 0}, end: {ln: 0, ch: 0}},
      it,
    }
  }

  let printTerm = (t: termNode<_>): string => {
    printTerm(dummyAnn(t))
  }

  let printExp = (e: expressionNode<sourceLocation>): string => {
    printTerm(Exp(dummyAnn(e)))
  }

  let expr_of_value = (v: value): expressionNode<_> => {
    switch v {
    // Constants
    | Con(constant) => Con(constant)
    // Functions
    | VFun(function) => Con(Sym(`@${Int.toString(function.id)}`))
    | VGen(generator) => Con(Sym(`@${Int.toString(generator.id)}`))
    // Vectors
    | Vec(vector) => Con(Sym(`@${Int.toString(vector.id)}`))
    }
  }

  let printVal = (v: value) => {
    printExp(v->expr_of_value)
  }

  let printOfValue = (v: value): Print.t<'id> => {
    Plain(printVal(v))->Print.dummy
  }

  let getBodyBasePrint = ({ann: {print}}: annotated<Runtime.bodyBaseNode, SMoL.printAnn>): print<
    kindedSourceLocation,
  > => {
    print
  }

  let getProgramBasePrint = ({ann: {print}}: annotated<Runtime.programBaseNode, SMoL.printAnn>) => {
    print
  }

  let exprLoc = sourceLocation => {nodeKind: Expression, sourceLocation}

  let renderBodyContext = (
    ~hole=Plain(holeText)->Print.dummy,
    ctx: pile<contextFrame, bodyBase>,
  ): React.element => {
    let {topping, base} = ctx
    let print = ref(getBodyBasePrint(base.base))
    // replace dead expressions
    let trueHole = topping->List.reduceReverse(holeOfBodyBase(base.base.it), (hole, ctxFrame) => {
      if SourceLocation.toString(hole) != SourceLocation.toString(ctxFrame.ann.sourceLocation) {
        Js.Console.log("skipped a dead expression")
        print := substituteById(print.contents, exprLoc(hole), ctxFrame.ann.print)
      }
      holeOfFrame(ctxFrame)
    })
    // plug values in
    topping->List.forEach(f => {
      valuesOfFrame(f)->List.forEach(((v, id)) => {
        print := substituteById(print.contents, exprLoc(id), printOfValue(v))
      })
    })
    // plug the hole in
    print := substituteById(print.contents, exprLoc(trueHole), hole)
    blank(print.contents->Print.toString)
    // reactOfPrint(print.contents)
  }

  let renderProgramContext = (
    ~hole=Plain(holeText)->Print.dummy,
    ctx: pile<contextFrame, programBase>,
  ): React.element => {
    let {topping, base} = ctx
    let print = ref(getProgramBasePrint(base))
    // replace dead expressions
    let trueHole = topping->List.reduceReverse(holeOfProgramBase(base.it), (hole, ctxFrame) => {
      if SourceLocation.toString(hole) != SourceLocation.toString(ctxFrame.ann.sourceLocation) {
        print := substituteById(print.contents, exprLoc(hole), ctxFrame.ann.print)
      }
      holeOfFrame(ctxFrame)
    })
    print := substituteById(print.contents, exprLoc(trueHole), hole)
    // plug values in
    topping->List.forEach(f => {
      valuesOfFrame(f)->List.forEach(((v, id)) => {
        print := substituteById(print.contents, exprLoc(id), printOfValue(v))
      })
    })
    // convert to React.element
    blank(print.contents->Print.toString)
    // reactOfPrint(print.contents)
  }

  let show_envFrm = (frm: environmentFrame) => {
    if Array.length(frm.content) == 0 {
      React.string("nothing")
    } else {
      <span className="binds">
        {React.array(
          Array.mapWithIndex(frm.content, (xv, key) => {
            let key = Int.toString(key)
            let (x, v) = xv
            <span key className="bind">
              {blank(Print.toString(x.ann.print))}
              <span ariaHidden={true}> {React.string(" ↦ ")} </span>
              <span className="sr-only"> {React.string("to")} </span>
              {
                let (recentlyChanged, v) = v.contents
                let v = v->Option.map(printVal)->Option.getOr("💣")
                blank(~marked=recentlyChanged, v)
              }
            </span>
          }),
        )}
      </span>
    }
  }

  let show_env = (env: environment) => {
    switch env {
    | list{} => raise(Impossible("An environment must have at least one frame."))
    | list{frm, ..._rest} => blank(`@${printName(EnvironmentID.toString(frm.id))}`)
    }
  }

  let show_one_env = (env: environment, key: int): React.element => {
    switch env {
    | list{} =>
      // blank("An environment must have at least one frame.")
      raise(Impossible("An environment must have at least one frame."))
    | list{frm, ...rest} => {
        let {id, content: _} = frm
        <li
          id={`def-${printName(EnvironmentID.toString(id))}`}
          key={Int.toString(key)}
          className="env-frame box">
          <span>
            {defblank(`@${printName(EnvironmentID.toString(id))}`)}
            <br />
            {React.string("binds ")}
            {show_envFrm(frm)}
            <br />
            {React.string("extending ")}
            {show_env(rest)}
          </span>
        </li>
      }
    }
  }

  let show_all_envs = () => {
    if allEnvs.contents === list{} {
      <p> {React.string("(No environments)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all environments">
        {React.array(allEnvs.contents->reverse->List.mapWithIndex(show_one_env)->List.toArray)}
      </ol>
    }
  }

  let show_one_hav = (val: value, key: int): React.element => {
    let key = Int.toString(key)
    switch val {
    | VGen({id, status: gen}) => {
        let id = id->Int.toString
        let status = switch gen.contents {
        | Fresh(_b, _env) => "fresh"
        | Suspended(_ctx, _env) => "suspended"
        | Running => "running"
        | Done => "done"
        }
        <li key id={`def-${id}`} className="fun box">
          {defblank(`@${id}`)}
          {React.string(", a ")}
          {blank(status)}
          {React.string(" generator")}
          {switch gen.contents {
          | Fresh(b, env) =>
            <>
              <br />
              {blank(Print.toString(b.ann.print))}
              <br />
              <span>
                {React.string("with environment ")}
                {show_env(env)}
              </span>
            </>
          | Suspended(ctx, env) =>
            <>
              <br />
              {React.string("with context ")}
              {{...ctx, base: {isGen: None, base: ctx.base}}->renderBodyContext}
              <br />
              <span>
                {React.string("with environment ")}
                {show_env(env)}
              </span>
            </>
          | Running | Done => <> </>
          }}
        </li>
      }
    | VFun({id, isGen, sourceLocation: ann, print, env}) => {
        let id = id->Int.toString
        let ann = if sk != Lispy {
          srcMap(ann)->Option.getOr(ann.sourceLocation)
        } else {
          ann.sourceLocation
        }
        <li key id={`def-${id}`} className="fun box">
          {defblank(`@${id}`)}
          {React.string(isGen ? ", a generator function" : ", a function")}
          <br />
          <details>
            <summary>
              {React.string(`at line ${(ann.begin.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.begin.ch + 1)->Int.toString}`)} </small>
              {React.string(` to ${(ann.end.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.end.ch + 1)->Int.toString}`)} </small>
            </summary>
            {blank(Print.toString(print))}
            <br />
          </details>
          <span>
            {React.string("with environment ")}
            {show_env(env)}
          </span>
        </li>
      }

    | Vec({id, contents: vs}) => {
        let id = id->Int.toString
        <li key id={`def-${id}`} className="vec box">
          {defblank(`@${id}`)}
          <br />
          {React.string("vec")}
          {React.array(
            vs
            ->Array.map(((recentlyChanged, v)) => {
              blank(~marked=recentlyChanged, printVal(v))
            })
            ->Array.mapWithIndex((e, i) =>
              <span key={Int.toString(i)}>
                {React.string(" ")}
                {e}
              </span>
            ),
          )}
        </li>
      }

    | Con(_) => raise(Impossible("Not supported heap-allocated value"))
    }
  }

  let show_all_havs = () => {
    if allHavs.contents === list{} {
      <p> {React.string("(No heap-allocated values)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all heap-allocated values">
        {React.array(allHavs.contents->reverse->List.mapWithIndex(show_one_hav)->List.toArray)}
      </ol>
    }
  }

  let string_of_error = err => {
    switch err {
    | UnboundIdentifier(symbol) => `The variable \`${symbol}\` is not defined.`
    | RedefinedIdentifier(symbol, env_id) =>
      `The variable \`${symbol}\` is defined more than once in the \`@${env_id}\` environment.`
    | UsedBeforeInitialization(symbol) => `The variable \`${symbol}\` hasn't been assigned a value.`
    | ExpectButGiven(string, value) => `Expecting a ${string}, given ${printVal(value)}.`
    | ArityMismatch(arity, int) =>
      `Expecting a function that accept ${Int.toString(
          int,
        )} arguments, given a function that takes ${RTArity.toString(arity)} arguments.`
    | OutOfBound(length, index) =>
      `Expecting an index less than the length of the vector (${Int.toString(
          length,
        )}), found ${Int.toString(index)}.`
    | DivisionByZero => `Division by zero`
    | AnyError(message) => message
    | WIP(string) => `Support for ${string} is WIP`
    }
  }

  let show_body_frame = (frm: frame<bodyBase>, key: int) => {
    let {ctx, env} = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {React.string("Waiting for a value")}
      <br />
      {React.string("in context ")}
      {ctx->renderBodyContext}
      <br />
      {React.string("in environment ")}
      {show_env(env)}
    </li>
  }

  let show_program_frame = (key: int, frm: frame<programBase>) => {
    let {ctx, env} = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {React.string("Waiting for a value")}
      <br />
      {React.string("in context ")}
      {ctx->renderProgramContext}
      <br />
      {React.string("in environment ")}
      {show_env(env)}
    </li>
  }

  let show_stack = (stk: stack) => {
    <ol className="box-list the-stack" ariaLabel="stack frames, with the oldest at the top">
      {show_program_frame(-1, stk.base)}
      {React.array(stk.topping->reverse->List.mapWithIndex(show_body_frame)->List.toArray)}
    </ol>
  }

  let show_state = (stack, now) => (envs, heap, stdout: output) => {
    <article id="stacker-configuration" ariaLabel="the current stacker configuration">
      <section id="stack-and-now">
        <h1> {React.string("Stack & Current Task")} </h1>
        {stack}
        <hr />
        {now}
      </section>
      <section id="environments">
        <h1> {React.string("Environments")} </h1>
        {envs}
      </section>
      <section id="heap">
        <h1> {React.string("Heap-allocated Values")} </h1>
        {heap}
      </section>
      <section id="stdout">
        <hr />
        {if stdout == list{} {
          <span> {React.string("(No output yet)")} </span>
        } else {
          <>
            <h1> {React.string("Output: ")} </h1>
            <pre className="stdout"> {React.string(printOutput(List.reverse(stdout)))} </pre>
          </>
        }}
      </section>
    </article>
  }
  let nowOfTerminatedState = s => {
    switch s {
    | Err(err) =>
      <div className="now box errored">
        <p> {React.string("Errored")} </p>
        {blank(string_of_error(err))}
      </div>

    | Tm =>
      <div className="now box terminated">
        <p> {React.string("Terminated")} </p>
      </div>
    }
  }
  let nowOfRedex = (redex: redex, ctx, env) => {
    switch redex.it {
    | Yielding(v) =>
      <p className="now box returning">
        {React.string("Yielding ")}
        {blank(printExp(v->expr_of_value))}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Printing(v) =>
      <p className="now box printing">
        {React.string("Printing ")}
        {blank(printExp(v->expr_of_value))}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Applying(f, vs) =>
      <p className="now box calling">
        {React.string("Calling ")}
        {blank(
          printExp(App(dummyAnn(expr_of_value(f)), vs->List.map(v => dummyAnn(expr_of_value(v))))),
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Nexting({id}) =>
      <p className="now box calling">
        {React.string("Resuming ")}
        {blank(`@${Int.toString(id)}`)}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Setting(x, v) =>
      let x = {
        let {ann: {sourceLocation}, it} = x
        {ann: sourceLocation, it}
      }
      <p className="now box replacing">
        {React.string("Rebinding a variable ")}
        <br />
        {blank(printExp(Set(x, expr_of_value(v)->dummyAnn)))}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | VecSetting(v_vec, i, v_val) =>
      <p className="now box replacing ">
        {React.string("Replacing a vector element ")}
        <br />
        {blank(
          AppPrm(
            VecSet,
            list{
              dummyAnn(expr_of_value(Vec(v_vec))),
              dummyAnn(expr_of_value(Con(Num(Int.toFloat(i))))),
              dummyAnn(expr_of_value(v_val)),
            },
          )->printExp,
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>
    }
  }
  let show_continuing_state = state => {
    switch state {
    | Returning(v, stk: stack) => {
        let stk = show_stack(stk)
        let now =
          <div className="now box returning">
            <p>
              {React.string("Returning ")}
              {blank(v->printVal)}
            </p>
          </div>
        show_state(stk, now)
      }

    | Entering(entrance, b, env, stk: stack) => {
        let stk = show_stack(stk)
        let now =
          <p className="now box called">
            {React.string(`Evaluating ${string_of_entrace(entrance)}`)}
            <br />
            {blank(b.ann.print->Print.toString)}
            <br />
            {React.string("in environment ")}
            {show_env(env)}
          </p>
        show_state(stk, now)
      }

    | Reducing(redex, stk) => {
        let (ctx, env, stk) = switch stk {
        | {topping: list{}, base: {ctx, env}} => (
            ctx->renderProgramContext,
            env->show_env,
            <p> {React.string("(No stack frames)")} </p>,
          )
        | {topping: list{{ctx, env}, ...topping}, base} => (
            ctx->renderBodyContext,
            env->show_env,
            show_stack({topping, base}),
          )
        }
        let now = nowOfRedex(redex, ctx, env)
        show_state(stk, now)
      }
    }
  }
  switch s {
  | Terminated(state) =>
    show_state(<p> {React.string("(No stack frames)")} </p>, nowOfTerminatedState(state))(
      show_all_envs(),
      show_all_havs(),
      stdout.contents,
    )
  | Continuing(state) =>
    show_continuing_state(state)(show_all_envs(), show_all_havs(), stdout.contents)
  }
}
