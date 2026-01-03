open SExpression
open SMoL

type edittimeError = YieldNotInDefgen(sourceLocation)
exception EdittimeError(edittimeError)

let checkYieldOnlyInGenerator = (program: program<_>, extractSourceLocation) => {
  let raiseError = sourceLocation => raise(EdittimeError(YieldNotInDefgen(sourceLocation)))
  let rec checkProgram = program => {
    switch program.it {
    | PNil => ()
    | PCons(t, p) => {
        checkTerm(false, t)
        checkProgram(p)
      }
    }
  }
  and checkTerm = (inGen, term) => {
    switch term.it {
    | Exp(e) => checkExpression(inGen, e)
    | Def(d) => checkDefinition(inGen, d)
    }
  }
  and checkExpression = (inGen, expression) => {
    switch expression.it {
    | Con(_) => ()
    | Ref(_) => ()
    | Set(_, e) => checkExpression(inGen, e)
    | Lam(_, b) => checkBlock(false, b)
    | Let(_, xes, b) => {
        xes->List.forEach(b => {
          let (_, e) = b.it
          checkExpression(inGen, e)
        })
        checkBlock(inGen, b)
      }
    | AppPrm(_, es) => es->List.forEach(e => checkExpression(inGen, e))
    | App(e, es) => {
        checkExpression(inGen, e)
        es->List.forEach(e => checkExpression(inGen, e))
      }
    | Bgn(es, e) => {
        es->List.forEach(e => checkExpression(inGen, e))
        checkExpression(inGen, e)
      }
    | If(e1, e2, e3) => {
        checkExpression(inGen, e1)
        checkExpression(inGen, e2)
        checkExpression(inGen, e3)
      }
    | And(es) => es->List.forEach(e => checkExpression(inGen, e))
    | Or(es) => es->List.forEach(e => checkExpression(inGen, e))
    | Cnd(ebs, ob) => {
        ebs->List.forEach(((e, b)) => {
          checkExpression(inGen, e)
          checkBlock(inGen, b)
        })
        ob->Option.forEach(b => {
          checkBlock(inGen, b)
        })
      }
    | Yield(e) => {
        checkExpression(inGen, e)
        if !inGen {
          raiseError(extractSourceLocation(expression.ann))
        }
      }
    | While(e, es) => {
        checkExpression(inGen, e)
        es->List.forEach(e => checkExpression(inGen, e))
      }
    }
  }
  and checkDefinition = (inGen, definition) => {
    switch definition.it {
    | Var(_, e) => checkExpression(inGen, e)
    | Fun(_x, _xs, b) => checkBlock(false, b)
    | GFun(_x, _xs, b) => checkBlock(true, b)
    }
  }
  and checkBlock = (inGen, block) => {
    switch block.it {
    | BRet(e) => checkExpression(inGen, e)
    | BCons(t, b) => {
        checkTerm(inGen, t)
        checkBlock(inGen, b)
      }
    }
  }
  switch checkProgram(program) {
  | () => None
  | exception EdittimeError(YieldNotInDefgen(sourceLocation)) =>
    let sourceLocation = SourceLocation.toString(sourceLocation)
    Some(`${sourceLocation}: yield may only be used inside a generator function.`)
  }
}
