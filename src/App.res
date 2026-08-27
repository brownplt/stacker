open SMoL
open Render
open Statics

@module("./url_parameters.js") external syntaxAtURL: string = "syntaxAtURL"
@module("./url_parameters.js") external inputSyntaxAtURL: string = "inputSyntaxAtURL"
@module("./url_parameters.js") external printTopLevelAtURL: bool = "printTopLevelAtURL"
@module("./url_parameters.js") external randomSeedAtURL: string = "randomSeedAtURL"
@module("./url_parameters.js") external holeAtURL: string = "holeAtURL"
@module("./url_parameters.js") external nNextAtURL: int = "nNextAtURL"
@module("./url_parameters.js") external programAtURL: string = "programAtURL"
@module("./url_parameters.js") external gcAtURL: bool = "gcAtURL"
@module("./url_parameters.js") external readOnlyMode: bool = "readOnlyMode"
@module("./url_parameters.js")
external make_url: (string, string, string, string, int, string, bool, bool, bool) => string =
  "make_url"
@module("./url_parameters.js")
external replace_url: (string, string, string, int, string, bool, bool, bool) => unit = "replace_url"
@scope("window") @val external openPopUp: string => unit = "openPopUp"

exception Impossible

let defaultHole = "•"

module FontSize = {
  type t =
    | XXS
    | XS
    | S
    | M
    | L
    | XL
    | XXL
  let default = M
  let toString = fontSize => {
    switch fontSize {
    | XXS => "xx-small"
    | XS => "x-small"
    | S => "small"
    | M => "medium"
    | XXL => "xx-large"
    | XL => "x-large"
    | L => "large"
    }
  }
  let fromString = s => {
    switch s {
    | "xx-small" => XXS
    | "x-small" => XS
    | "small" => S
    | "xx-large" => XXL
    | "x-large" => XL
    | "large" => L
    | _ => M
    }
  }
}

type running_state = {
  prevs: list<React.element>,
  now: React.element,
  nexts: list<React.element>,
  latestState: Runtime.state,
  srcMap: kindedSourceLocation => option<SExpression.sourceLocation>,
}

let pool_of_randomSeed = [
  Js.Math._E->Float.toString,
  "smol",
  "defvar",
  "deffun",
  "cond",
  "lambda",
  "2023",
]
let new_randomSeed = () => {
  let index = Js.Math.random_int(0, 1 + Array.length(pool_of_randomSeed))
  pool_of_randomSeed->Array.get(index)->Option.getOr(Js.Math.random()->Float.toString)
}

type state = {
  running: option<running_state>,
  previewSyntax: option<Render.Syntax.t>,
}

type randomSeedConfig = {isSet: bool, randomSeed: string}

let inputLanguage = (sk: Render.Syntax.t): SMoL.Language.t => {
  switch sk {
  | Rhombus => Rhombus
  | _ => SMoL
  }
}

let outputLanguage = (sk: Render.Syntax.t): SMoL.Language.t => {
  switch sk {
  | Lispy => SMoL
  | Rhombus => Rhombus
  | Python => Python
  | JavaScript => JavaScript
  | Pseudo => PseudoCode
  | Scala => Scala
  }
}

let translateProgram = (~input, sk, printTopLevel, p) => {
  SMoL.translateProgram(
    ~input=inputLanguage(input),
    ~output=outputLanguage(sk),
    printTopLevel,
    p,
  )
}

let translateProgramFull = (~input, sk, printTopLevel, p) => {
  SMoL.translateProgramFull(
    ~input=inputLanguage(input),
    ~output=outputLanguage(sk),
    printTopLevel,
    p,
  )
}

let make_preview = (~input, sk: Syntax.t, printTopLevel, program) => {
  switch translateProgram(~input, sk, printTopLevel, program) {
  | program =>
    <>
      <span>
        {React.string(`(Showing the `)}
        <u> {React.string(Syntax.toString(sk))} </u>
        {React.string(` translation)`)}
      </span>
      <CodeEditor syntax={sk} program={program} readOnly={true} setProgram={_ => ()} />
    </>
  | exception SMoLTranslateError(err) => {
      let parseFeedback = TranslateError.toString(err)
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
    }
  }
}

let parseSMoL = (program: string) => {
  Parser.parseProgram(program)
}

@react.component
let make = () => {
  let (program, rawSetProgram) = React.useState(_ => "")
  // let program = remove_lang_line(program)
  let (parseFeedback, setParseFeedback) = React.useState(_ => "")
  let setProgram = (setter: string => string) => {
    setParseFeedback(_ => "")
    rawSetProgram(setter)
  }
  let (syntax, setSyntax) = React.useState(_ => {
    Syntax.fromString(syntaxAtURL)->Option.getOr(Lispy)
  })
  let (inputSyntax, setInputSyntax) = React.useState(_ => {
    Syntax.fromString(inputSyntaxAtURL)->Option.filter(Syntax.isReadable)->Option.getOr(Lispy)
  })
  // Reading Rhombus runs a WebAssembly parser, and a browser will not compile a
  // module that size synchronously. Start it as the page loads so it is ready
  // before anyone can choose Rhombus, and report the wait rather than a parse
  // error if they get there first.
  let (readerReady, setReaderReady) = React.useState(_ => SMoL.RhombusReader.isReady())
  React.useEffect0(() => {
    if !SMoL.RhombusReader.isReady() {
      SMoL.RhombusReader.init()
      ->Promise.thenResolve(() => setReaderReady(_ => true))
      ->Promise.done
    }
    None
  })
  let (printTopLevel, setPrintTopLevel) = React.useState(_ => printTopLevelAtURL)
  let (recycleHeapBoxes, setRecycleHeapBoxes) = React.useState(_ => gcAtURL)
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let (hole, setHole) = React.useState(_ => {
    if holeAtURL == "" {
      defaultHole
    } else {
      holeAtURL
    }
  })
  let forward = s => {
    switch s {
    | None => raise(Impossible)
    | Some({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => raise(Impossible)
    | Some({prevs, now, nexts: list{}, latestState: Continuing(latestState), srcMap}) => {
        let latestState = Runtime.transition(latestState)
        Some({
          prevs: list{now, ...prevs},
          now: Render.render(syntax, hole, latestState, srcMap),
          nexts: list{},
          latestState,
          srcMap,
        })
      }

    | Some({prevs, now, nexts: list{e, ...nexts}, latestState, srcMap}) =>
      Some({
        prevs: list{now, ...prevs},
        now: e,
        nexts,
        latestState,
        srcMap,
      })
    }
  }
  let backward = state =>
    switch state {
    | None => raise(Impossible)
    | Some({prevs, now, nexts, latestState, srcMap}) =>
      switch prevs {
      | list{} => raise(Impossible)
      | list{e, ...prevs} => Some({prevs, now: e, nexts: list{now, ...nexts}, latestState, srcMap})
      }
    }
  let nextable = state =>
    switch state {
    | None => false
    | Some({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => false
    | Some({prevs: _, now: _, nexts: list{}, latestState: Continuing(_)}) => true
    | Some({prevs: _, now: _, nexts: list{_e, ..._nexts}, latestState: _}) => true
    }
  let loadProgram = program => {
    switch translateProgramFull(~input=inputSyntax, syntax, printTopLevel, program) {
    | exception SMoLTranslateError(err) => {
        setParseFeedback(_ => TranslateError.toString(err))
        None
      }

    | program =>
      switch checkYieldOnlyInGenerator(program, ann => ann.sourceLocation) {
      | Some(err) => {
          setParseFeedback(_ => err)
          None
        }
      | None => {
          open SExpression
          let s: Runtime.state = Runtime.load(
            program,
            randomSeed.randomSeed,
            printTopLevel,
            recycleHeapBoxes,
          )
          let srcMap: kindedSourceLocation => option<sourceLocation> = {
            let map = program.ann.print->Print.toSourceMap(stringOfKindedSourceLocation)
            srcLoc => {
              Map.get(map, stringOfKindedSourceLocation(srcLoc))
            }
          }
          Some({
            prevs: list{},
            nexts: list{},
            now: Render.render(syntax, hole, s, srcMap),
            latestState: s,
            srcMap,
          })
        }
      }
    }
  }
  let (editorFontSize, setEditorFontSize) = React.useState(_ => FontSize.default)
  let (state, setState) = React.useState(_ => {
    setProgram(_ => programAtURL)
    if nNextAtURL < 0 {
      None
    } else {
      let s = ref(loadProgram(programAtURL))
      for _ in 1 to nNextAtURL {
        s.contents = forward(s.contents)
      }
      s.contents
    }
  })
  let nNext = state->Option.mapOr(0, ({prevs}) => prevs->List.length)
  React.useEffect(
    () => {
      replace_url(
        syntax->Syntax.toString,
        randomSeed.randomSeed,
        hole,
        nNext,
        program,
        readOnlyMode,
        recycleHeapBoxes,
        printTopLevel,
      )
      None
    },
    (
      syntax->Syntax.toString,
      randomSeed.randomSeed,
      hole,
      nNext,
      program,
      readOnlyMode,
      recycleHeapBoxes,
      printTopLevel,
    ),
  )
  let onRunClick = _evt => {
    setState(_ => loadProgram(program))
  }
  let onStopClick = _evt => {
    setState(_ => None)
  }
  let prevable = switch state {
  | None => false
  | Some({prevs, now: _, nexts: _, latestState: _}) =>
    switch prevs {
    | list{} => false
    | list{_e, ..._prevs} => true
    }
  }
  let onPrevClick = _evt => {
    setState(backward)
  }
  let onNextClick = _evt => {
    setState(forward)
  }
  let is_running = state != None
  let onRunAndNextX99 = _ => {
    setState(state => {
      let rec forwardN = (n, s) => {
        if n === 0 {
          s
        } else if nextable(s) {
          forwardN(n - 1, forward(s))
        } else {
          s
        }
      }
      forwardN(
        99,
        if is_running {
          state
        } else {
          loadProgram(program)
        },
      )
    })
  }
  let nextable = nextable(state)
  // The examples are written as s-expressions. Put one in the editor in
  // whatever syntax the editor is reading, transliterated rather than
  // instrumented — the top-level printing setting applies when it is run, not
  // when it is loaded.
  let loadExample = source =>
    setProgram(_ =>
      if inputSyntax == Lispy {
        source
      } else {
        switch translateProgram(~input=Lispy, inputSyntax, false, source) {
        | translated => translated
        | exception SMoLTranslateError(_) => source
        }
      }
    )

  let onShare = readOnlyMode => _ => {
    openPopUp(
      make_url(
        syntax->Syntax.toString,
        inputSyntax->Syntax.toString,
        randomSeed.randomSeed,
        hole,
        nNext,
        program,
        readOnlyMode,
        recycleHeapBoxes,
        printTopLevel,
      ),
    )
  }
  let onKeyDown = evt => {
    let key = ReactEvent.Keyboard.key(evt)

    // Js.log(`Key pressed (${key})`)
    if key == "j" && prevable {
      onPrevClick(evt)
    } else if key == "k" && nextable {
      onNextClick(evt)
    }
  }
  let runButton =
    <button onClick=onRunClick disabled={is_running}>
      <span ariaHidden={true}> {React.string("▶ ")} </span>
      {React.string("Run")}
    </button>
  let stopButton =
    <button onClick=onStopClick disabled={!is_running}>
      <span ariaHidden={true}> {React.string("⏹ ")} </span>
      {React.string("Stop")}
    </button>
  let prevButton =
    <button onClick=onPrevClick disabled={!prevable}>
      <span ariaHidden={true}> {React.string("⏮ ")} </span>
      {React.string("Previous")}
      // <kbd> {React.string("j")} </kbd>
    </button>
  let nextButton =
    <button onClick=onNextClick disabled={!nextable}>
      <span ariaHidden={true}> {React.string("⏭ ")} </span>
      {React.string("Next")}
      // <kbd> {React.string("k")} </kbd>
    </button>
  let runAndNextX99 =
    <button onClick=onRunAndNextX99 disabled={is_running && !nextable}>
      <span ariaHidden={true}>
        {React.string(
          if is_running {
            "⏭ "
          } else {
            "▶ "
          },
        )}
      </span>
      {React.string(
        if is_running {
          "Next×99"
        } else {
          "Run and Next×99"
        },
      )}
    </button>
  let editorConfig = if readOnlyMode {
    <> </>
  } else {
    <span>
      <label> {React.string("Font size = ")} </label>
      <select
        onChange={evt => {
          let fs: string = ReactEvent.Form.currentTarget(evt)["value"]
          let fs = FontSize.fromString(fs)
          setEditorFontSize(_ => fs)
        }}>
        <option selected={FontSize.XXS == editorFontSize} value={FontSize.toString(XXS)}>
          {React.string(FontSize.toString(XXS))}
        </option>
        <option selected={FontSize.XS == editorFontSize} value={FontSize.toString(XS)}>
          {React.string(FontSize.toString(XS))}
        </option>
        <option selected={FontSize.S == editorFontSize} value={FontSize.toString(S)}>
          {React.string(FontSize.toString(S))}
        </option>
        <option selected={FontSize.M == editorFontSize} value={FontSize.toString(M)}>
          {React.string(FontSize.toString(M))}
        </option>
        <option selected={FontSize.L == editorFontSize} value={FontSize.toString(L)}>
          {React.string(FontSize.toString(L))}
        </option>
        <option selected={FontSize.XL == editorFontSize} value={FontSize.toString(XL)}>
          {React.string(FontSize.toString(XL))}
        </option>
        <option selected={FontSize.XXL == editorFontSize} value={FontSize.toString(XXL)}>
          {React.string(FontSize.toString(XXL))}
        </option>
      </select>
    </span>
  }
  let exampleProgramsAndStopButtonShortcut = if readOnlyMode {
    <> </>
  } else {
    <>
      <details>
        <summary>
          {React.string("The program must be ")}
          <em> {React.string("edited")} </em>
          {React.string(" in the ")}
          <a
            href="https://docs.google.com/document/d/e/2PACX-1vTMVCrUYliicrunyxftDwv6HVmBeKaRW9-VF9Xh1GUFoHMmomOczz_RRIZXPJoH8WB66x-d4GlRvwuy/pub">
            {React.string("Lispy")}
          </a>
          {React.string(" syntax.")}
        </summary>
        {React.string("Example programs:")}
        <menu ariaLabel="a list of example programs">
          <li>
            <button
              disabled={is_running}
              value="Fibonacci"
              onClick={_evt => loadExample(Programs.program_fib)}>
              {React.string("Fibonacci")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Scope"
              onClick={_evt => loadExample(Programs.program_dynscope)}>
              {React.string("Scope")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Counter"
              onClick={_evt => loadExample(Programs.program_ctr1)}>
              {React.string("Counter")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Aliasing"
              onClick={_evt => loadExample(Programs.program_aliasing)}>
              {React.string("Aliasing")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Object"
              onClick={_evt => loadExample(Programs.program_object)}>
              {React.string("Object")}
            </button>
          </li>
        </menu>
      </details>
      <span>
        {React.string("Stacker will ")}
        <em> {React.string("read")} </em>
        {React.string(" the ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            let chosen = Syntax.fromString(newValue)->Option.getOr(Lispy)
            setInputSyntax(_ => chosen)
            // Presenting in a syntax a program cannot be read in is the point,
            // but presenting Lispy while reading Rhombus hides what was typed,
            // so follow the input unless the two were already apart.
            setSyntax(prev =>
              if prev == inputSyntax {
                chosen
              } else {
                prev
              }
            )
          }
          <select onChange disabled={is_running}>
            {React.array(
              Syntax.readable->Array.map(s => {
                <option selected={s == inputSyntax} value={Syntax.toString(s)}>
                  {React.string({Syntax.toString(s)})}
                </option>
              }),
            )}
          </select>
        }
        {React.string(" syntax and ")}
        <em> {React.string("present")} </em>
        {React.string(" in the ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setSyntax(_ => Syntax.fromString(newValue)->Option.getOr(Lispy))
          }
          <select onChange disabled={is_running}>
            {React.array(
              Syntax.all->Array.map(s => {
                <option selected={s == syntax} value={Syntax.toString(s)}>
                  {React.string({Syntax.toString(s)})}
                </option>
              }),
            )}
          </select>
        }
        {React.string(" syntax.")}
        {if inputSyntax == Rhombus && !readerReady {
          <span className="parse-feedback">
            {React.string(" (Loading the Rhombus reader...)")}
          </span>
        } else {
          React.null
        }}
      </span>
      {if is_running {
        <p>
          <mark>
            <button onClick=onStopClick disabled={!is_running}>
              <span ariaHidden={true}> {React.string("⏹ ")} </span>
              {React.string("Stop")}
            </button>
            {React.string(" before making any change!")}
          </mark>
        </p>
      } else {
        React.array([])
      }}
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
    </>
  }
  let advancedConfiguration = if readOnlyMode {
    <> </>
  } else {
    <details>
      <summary>
        <span ariaHidden={true}> {React.string("⚙️ ")} </span>
        {React.string("Advanced configuration:")}
      </summary>
      <label>
        {React.string("Random seed = ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setRandomSeed(_ => {isSet: true, randomSeed: newValue})
          }
          if randomSeed.isSet {
            <input disabled={is_running} type_="text" value={randomSeed.randomSeed} onChange />
          } else {
            <input
              disabled={is_running} type_="text" placeholder={randomSeed.randomSeed} onChange
            />
          }
        }
      </label>
      <br />
      <label>
        {React.string("Hole = ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setHole(_ => newValue)
          }
          <input disabled={is_running} type_="text" value={hole} onChange />
        }
      </label>
      <br />
      <label>
        <input
          type_="checkbox"
          disabled={is_running}
          checked={printTopLevel}
          onChange={_ => setPrintTopLevel(v => !v)}
        />
        {React.string("Print the values of top-level expressions")}
      </label>
      <br />
      <label>
        <input
          type_="checkbox"
          disabled={is_running}
          checked={recycleHeapBoxes}
          onChange={_ => setRecycleHeapBoxes(v => !v)}
        />
        {React.string("Garbage Collection (GC)")}
      </label>
    </details>
  }

  let (dragging, setDragging) = React.useState(() => false)
  let (editorWidth, setEditorWidth) = React.useState(() => None)
  <main
    onKeyDown
    onMouseMove={event => {
      if dragging {
        let x = ReactEvent.Mouse.clientX(event)
        setEditorWidth(_ => Some(x))
      }
    }}
    onMouseUp={_ => {
      setDragging(_ => false)
    }}>
    <section
      id="program-source"
      style={switch editorWidth {
      | None => {}
      | Some(editorWidth) => {width: `calc(${Belt.Int.toString(editorWidth)}px - 0.5ex)`}
      }}>
      {exampleProgramsAndStopButtonShortcut}
      {editorConfig}
      <div
        ariaLabel="the code editor, press Esc then Tab to escape!"
        style={{
          fontSize: FontSize.toString(editorFontSize),
        }}>
        <CodeEditor
          syntax={if is_running {
            syntax
          } else {
            inputSyntax
          }}
          program={if is_running && syntax != inputSyntax {
            translateProgram(~input=inputSyntax, syntax, printTopLevel, program)
          } else {
            program
          }}
          readOnly={is_running}
          setProgram
        />
      </div>
    </section>
    <div
      id="split"
      onMouseDown={_ => {
        setDragging(_ => true)
      }}
    />
    <section id="stacker">
      {advancedConfiguration}
      <menu id="nav-trace" ariaLabel="toolbar">
        {if readOnlyMode {
          <> </>
        } else {
          <>
            <li> {runButton} </li>
            <li> {stopButton} </li>
          </>
        }}
        <li> {prevButton} </li>
        <li> {nextButton} </li>
        <li> {runAndNextX99} </li>
        <li>
          <button onClick={onShare(readOnlyMode)} disabled={!is_running}>
            <span ariaHidden={true}> {React.string("🔗 ")} </span>
            {React.string("Share")}
          </button>
        </li>
        {if readOnlyMode {
          <li>
            <a
              href={make_url(
                syntax->Syntax.toString,
                inputSyntax->Syntax.toString,
                "",
                hole,
                -1,
                program,
                false,
                recycleHeapBoxes,
                printTopLevel,
              )}>
              {React.string("✎ edit")}
            </a>
          </li>
        } else {
          <li>
            <button onClick={onShare(true)} disabled={!is_running}>
              <span ariaHidden={true}> {React.string("🔗 ")} </span>
              {React.string("Share (read-only)")}
            </button>
          </li>
        }}
      </menu>
      {switch state {
      | None =>
        <>
          <p>
            {React.string("To start tracing, click ")}
            <button onClick=onRunClick disabled={is_running}>
              <span ariaHidden={true}> {React.string("▶ ")} </span>
              {React.string("Run")}
            </button>
            {React.string(".")}
          </p>
          // A preview is worth showing whenever the presentation differs from
          // what was typed, which is no longer the same as "not Lispy".
          {if syntax == inputSyntax {
            <> </>
          } else {
            make_preview(~input=inputSyntax, syntax, printTopLevel, program)
          }}
        </>
      | Some(s) => s.now
      }}
    </section>
  </main>
}
