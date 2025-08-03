import { parser } from "./parser.js";
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Identifier: t.variableName,
      Boolean: t.bool,
      String: t.string,
      LineComment: t.lineComment,
      "( )": t.paren,
      "[ ]": t.paren,
      "#t": t.keyword,
      "#f": t.keyword,
      "defgen": t.keyword,
      "deffun": t.keyword,
      "defvar": t.keyword,
      "yield": t.keyword,
      "while": t.keyword,
      "set!": t.keyword,
      "if": t.keyword,
      "let": t.keyword,
      "cond": t.keyword,
      "else": t.keyword,
      "begin": t.keyword,
      "lambda": t.keyword,
      "λ": t.keyword,
      "generator": t.keyword,
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

export const smolLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: ";" }
  }
});

import { completeFromList } from "@codemirror/autocomplete";

export const smolCompletion = smolLanguage.data.of({
  autocomplete: completeFromList([
    { label: "#t", type: "keyword" },
    { label: "#f", type: "keyword" },
    { label: "defgen", type: "keyword" },
    { label: "deffun", type: "keyword" },
    { label: "defvar", type: "keyword" },
    { label: "yield", type: "keyword" },
    { label: "while", type: "keyword" },
    { label: "set!", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "cond", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "begin", type: "keyword" },
    { label: "lambda", type: "keyword" },
    { label: "generator", type: "keyword" },
    { label: "λ", type: "keyword" },
    { label: "+", type: "function" },
    { label: "-", type: "function" },
    { label: "*", type: "function" },
    { label: "/", type: "function" },
    { label: "<", type: "function" },
    { label: "=", type: "function" },
    { label: ">", type: "function" },
    { label: "<=", type: "function" },
    { label: ">=", type: "function" },
    { label: "!=", type: "function" },
    { label: "eq?", type: "function" },
    { label: "mvec", type: "function" },
    { label: "vec-ref", type: "function" },
    { label: "vec-set!", type: "function" },
    { label: "vec-len", type: "function" },
    { label: "error", type: "function" },
    { label: "next", type: "function" },
    { label: "print", type: "function" }
  ])
});

import { LanguageSupport } from "@codemirror/language";

export function smol() {
  return new LanguageSupport(smolLanguage, [smolCompletion]);
}
