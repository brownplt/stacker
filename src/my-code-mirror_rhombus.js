import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { rhombus } from './codemirror-lang_rhombus/rhombus';
import { noActiveLine } from './codemirror-no-active-line';

function rhombusCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "100%",
    extensions: [rhombus(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default rhombusCodeMirror;
