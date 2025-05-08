import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { scala } from './codemirror-lang_scala/scala';
import { noActiveLine } from './codemirror-no-active-line';

function ScalaCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "100%",
    extensions: [scala(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default ScalaCodeMirror;
