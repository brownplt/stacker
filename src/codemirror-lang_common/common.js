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
      "let": t.keyword,
      "fun": t.keyword,
      "if": t.keyword,
      "else": t.keyword,
      "return": t.keyword,
      "while": t.keyword,
      "raise": t.keyword,
      "lam": t.keyword,
      "end": t.keyword,
      "vec": t.keyword,
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

export const commonLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: "//" }
  }
});

import { completeFromList } from "@codemirror/autocomplete";

export const commonCompletion = commonLanguage.data.of({
  autocomplete: completeFromList([

    { label: "true", type: "keyword" },
    { label: "false", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "fun", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "return", type: "keyword" },
    { label: "while", type: "keyword" },
    { label: "raise", type: "keyword" },
    { label: "lam", type: "keyword" },
    { label: "end", type: "keyword" },
    { label: "vec", type: "keyword" },
    { label: "+", type: "function" },
    { label: "-", type: "function" },
    { label: "*", type: "function" },
    { label: "/", type: "function" },
    { label: "<", type: "function" },
    { label: "==", type: "function" },
    { label: ">", type: "function" },
    { label: "<=", type: "function" },
    { label: ">=", type: "function" },
    { label: "!=", type: "function" }
  ])
});

import { LanguageSupport } from "@codemirror/language";

export function common() {
  return new LanguageSupport(commonLanguage, [commonCompletion]);
}
