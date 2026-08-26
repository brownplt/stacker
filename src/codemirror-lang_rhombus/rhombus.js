import { parser } from "./parser.js";
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Identifier: t.variableName,
      // #true / #false / #void, which Rhombus writes with a leading #.
      Literal: t.atom,
      // ~else, and keyword arguments generally.
      Keyword: t.labelName,
      Number: t.number,
      String: t.string,
      LineComment: t.lineComment,
      Operator: t.operator,
      Bar: t.controlOperator,
      Colon: t.punctuation,
      "Comma Semicolon": t.separator,
      "( )": t.paren,
      "[ ]": t.squareBracket,
      "{ }": t.brace,

      "def": t.definitionKeyword,
      "fun": t.definitionKeyword,
      "let": t.definitionKeyword,
      "mutable": t.modifier,
      "if": t.controlKeyword,
      "cond": t.controlKeyword,
      "while": t.controlKeyword,
      "block": t.controlKeyword,
    }),
    indentNodeProp.add({
      Application: context => context.column(context.node.from) + context.unit
    }),
    foldNodeProp.add({
      Application: foldInside
    })
  ]
});

import { LRLanguage } from "@codemirror/language";

export const rhombusLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: "//" }
  }
});

import { completeFromList } from "@codemirror/autocomplete";

export const rhombusCompletion = rhombusLanguage.data.of({
  autocomplete: completeFromList([
    { label: "def", type: "keyword" },
    { label: "fun", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "mutable", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "cond", type: "keyword" },
    { label: "while", type: "keyword" },
    { label: "block", type: "keyword" },
    { label: "~else", type: "keyword" },
    { label: "#true", type: "constant" },
    { label: "#false", type: "constant" },
    { label: "#void", type: "constant" },
    { label: "Array", type: "function" },
    { label: "Pair", type: "function" },
    { label: "List", type: "function" },
    { label: "println", type: "function" },
    { label: "error", type: "function" },
    { label: ":=", type: "function" },
    { label: "+", type: "function" },
    { label: "-", type: "function" },
    { label: "*", type: "function" },
    { label: "/", type: "function" },
    { label: "++", type: "function" },
    { label: "<", type: "function" },
    { label: ">", type: "function" },
    { label: "<=", type: "function" },
    { label: ">=", type: "function" },
    { label: "==", type: "function" },
    { label: "!=", type: "function" },
    { label: "===", type: "function" },
    { label: "&&", type: "function" },
    { label: "||", type: "function" },
    { label: "!", type: "function" }
  ])
});

import { LanguageSupport } from "@codemirror/language";

export function rhombus() {
  return new LanguageSupport(rhombusLanguage, [rhombusCompletion]);
}
