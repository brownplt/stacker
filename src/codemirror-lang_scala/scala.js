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
      "true": t.keyword,
      "false": t.keyword,
      "def": t.keyword,
      "var": t.keyword,
      "val": t.keyword,
      "if": t.keyword,
      "else": t.keyword,
      "throw": t.keyword,
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

export const scalaLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: "//" }
  }
});

import { completeFromList } from "@codemirror/autocomplete";

export const scalaCompletion = scalaLanguage.data.of({
  autocomplete: completeFromList([
    { label: "true", type: "keyword" },
    { label: "false", type: "keyword" },
    { label: "function", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "+", type: "function" },
    { label: "-", type: "function" },
    { label: "*", type: "function" },
    { label: "/", type: "function" },
    { label: "<", type: "function" },
    { label: "===", type: "function" },
    { label: ">", type: "function" },
    { label: "<=", type: "function" },
    { label: ">=", type: "function" },
    { label: "!=", type: "function" },
    { label: "throw", type: "function" }
  ])
});

import { LanguageSupport } from "@codemirror/language";

export function scala() {
  return new LanguageSupport(scalaLanguage, [scalaCompletion]);
}
