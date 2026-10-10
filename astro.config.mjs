import { defineConfig } from 'astro/config';
import datalog from './src/syntax/datalog.js';
import gdb from './src/syntax/gdb.js';
import llvm from './src/syntax/llvm.js';
import cabal from './src/syntax/cabal.js';

export default defineConfig({
  site: 'https://luctielen.com',
  markdown: {
    syntaxHighlight: 'shiki',
    shikiConfig: {
      theme: 'css-variables',
      langs: ['zig', datalog, gdb, llvm, cabal],
    },
  },
});
