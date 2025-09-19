# js_of_ocaml Package Fix

This directory contains updated patches for js_of_ocaml-compiler.6.0.1+ox that resolve
the patch application failures.

## Issue
The original patches in the oxcaml opam repository were failing to apply:
- js_of_ocaml-iarray-primitives.patch
- js_of_ocaml-floatarray_create_local.patch

## Solution
Updated patches that are compatible with the current js_of_ocaml source structure.

## Usage
These patches can be applied manually or used to replace the failing ones in a local
opam repository setup.