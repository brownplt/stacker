module SMoLCodeMirror = {
  @react.component @module("./my-code-mirror_smol.js")
  external make: (~value: string, ~readOnly: bool, ~onChange: string => unit) => React.element =
    "default"
}
module JavaScriptCodeMirror = {
  @react.component @module("./my-code-mirror_javascript.js")
  external make: (~value: string, ~readOnly: bool, ~onChange: string => unit) => React.element =
    "default"
}
module PythonCodeMirror = {
  @react.component @module("./my-code-mirror_python.js")
  external make: (~value: string, ~readOnly: bool, ~onChange: string => unit) => React.element =
    "default"
}
module CommonCodeMirror = {
  @react.component @module("./my-code-mirror_common.js")
  external make: (~value: string, ~readOnly: bool, ~onChange: string => unit) => React.element =
    "default"
}
module ScalaCodeMirror = {
  @react.component @module("./my-code-mirror_scala.js")
  external make: (~value: string, ~readOnly: bool, ~onChange: string => unit) => React.element =
    "default"
}

@react.component
let make = (~syntax, ~program, ~readOnly, ~setProgram) => {
  // let output = switch S_expr.parse_many(program |> S_expr.stringToSource) {
  // | parsed => S_expr.stringOfManySexprs(parsed)
  // | exception S_expr.WantSExprFoundEOF => "Unexpected EOF while looking for an S-expression."
  // | exception S_expr.WantStringFoundEOF => "Unexpected EOF while processing a string."
  // | exception S_expr.WantEscapableCharFound(string) =>
  //   `This character (${string}) can't be escaped.`
  // | exception S_expr.WantSExprFoundRP(_source) => "Unexpected right parenthesis."
  // }
  let onChange = s => {
    setProgram(_ => s)
  }
  switch syntax {
  | Render.Syntax.Lispy => <SMoLCodeMirror value=program readOnly={readOnly} onChange={onChange} />
  | JavaScript => <JavaScriptCodeMirror value=program readOnly={readOnly} onChange={onChange} />
  | Python => <PythonCodeMirror value=program readOnly={readOnly} onChange={onChange} />
  | Pseudo => <CommonCodeMirror value=program readOnly={readOnly} onChange={onChange} />
  | Scala => <ScalaCodeMirror value=program readOnly={readOnly} onChange={onChange} />
  }
  // let onChange = evt => {
  //   let s = ReactEvent.Form.currentTarget(evt)["value"]
  //   setProgram(_ => s)
  // }
  // <div>
  // <textarea value=program onChange rows=17 cols=30 />
  // </div>
}
