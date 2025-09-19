# Solution for js_of_ocaml-compiler.6.0.1+ox Installation Issues

This repository contains a fix for the patch application failures when installing `js_of_ocaml-compiler.6.0.1+ox`.

## Problem

When running:
```bash
opam install js_of_ocaml.6.0.1+ox js_of_ocaml-compiler.6.0.1+ox
```

The installation fails with patch errors:
```
These patches didn't apply:
- js_of_ocaml-iarray-primitives.patch: exited with code 1
- js_of_ocaml-floatarray_create_local.patch: exited with code 1
```

## Solution

### Option 1: Automatic Fix Script (Recommended)

Use the provided fix script after the opam install fails:

1. Try the normal install (it will fail):
```bash
opam install js_of_ocaml.6.0.1+ox js_of_ocaml-compiler.6.0.1+ox
```

2. Find the build directory where the failure occurred (usually in `~/.opam/{switch}/.opam-switch/build/js_of_ocaml-compiler.6.0.1+ox/`)

3. Run the fix script:
```bash
./fix-js-of-ocaml-patches.sh ~/.opam/5.2.0+ox/.opam-switch/build/js_of_ocaml-compiler.6.0.1+ox
```

4. Continue the install:
```bash
opam install js_of_ocaml.6.0.1+ox js_of_ocaml-compiler.6.0.1+ox
```

### Option 2: Manual Fix

If you prefer to apply the fixes manually:

1. Navigate to the failed build directory
2. Add the missing functions to `runtime/js/array.js`:
```javascript
// Provides: caml_iarray_of_array const
function caml_iarray_of_array(a) {
  return a;
}

// Provides: caml_array_of_iarray const
function caml_array_of_iarray(a) {
  return a;
}

//Provides: caml_floatarray_create_local const (const)
//Requires: caml_floatarray_create
function caml_floatarray_create_local(x) {
  return caml_floatarray_create(x);
}
```

3. Add the missing exports to `runtime/wasm/array.wat` (add before the final `)`):
```wasm
   (func (export "caml_iarray_of_array")
      (param $a (ref eq)) (result (ref eq))
      (local.get $a))

   (func (export "caml_array_of_iarray")
      (param $a (ref eq)) (result (ref eq))
      (local.get $a))
```

4. Add the export to the `$caml_floatarray_create` function:
```wasm
   (func $caml_floatarray_create
      (export "caml_floatarray_create_local")
      (export "caml_make_float_vect") (export "caml_floatarray_create")
      ...
```

### Files Included

- `fix-js-of-ocaml-patches.sh` - Automated fix script
- `patches/` - Directory containing corrected patch files for reference
- `patches/README.md` - Additional documentation about the patches

### What These Patches Do

1. **iarray-primitives**: Adds identity conversion functions between regular arrays and immutable arrays
2. **floatarray_create_local**: Adds a local allocation version of floatarray creation for OCaml 5.x compatibility

These functions are required for the OxCaml compiler extensions to work properly with js_of_ocaml.