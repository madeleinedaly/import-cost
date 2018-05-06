'use strict';
const _ = require('lodash');    // this is a comment
const capitalize = require('lodash/capitalize');
const {cleanup} = require('import-cost');

console.log(capitalize('hello world'));

/*
  (ov-clear)
  (setq import-cost--decorations-list nil)
  (import-cost--deactivate!)
  (import-cost--activate!)
  (import-cost--process-active-buffer!)
*/
