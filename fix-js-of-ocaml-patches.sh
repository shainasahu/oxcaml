#!/bin/bash

# Fix for js_of_ocaml-compiler.6.0.1+ox patch failures
# This script applies the missing patches after a failed opam install

set -e

echo "Applying fixes for js_of_ocaml-compiler patch failures..."

# Get the build directory (this needs to be determined at runtime)
BUILD_DIR="$1"
if [ -z "$BUILD_DIR" ]; then
    echo "Usage: $0 <js_of_ocaml_build_directory>"
    exit 1
fi

if [ ! -d "$BUILD_DIR" ]; then
    echo "Build directory $BUILD_DIR does not exist"
    exit 1
fi

echo "Working in build directory: $BUILD_DIR"

# Apply the missing iarray functions to runtime/js/array.js
if ! grep -q "caml_iarray_of_array" "$BUILD_DIR/runtime/js/array.js"; then
    echo "Adding iarray functions to runtime/js/array.js"
    cat >> "$BUILD_DIR/runtime/js/array.js" << 'EOF'

// Provides: caml_iarray_of_array const
function caml_iarray_of_array(a) {
  return a;
}

// Provides: caml_array_of_iarray const
function caml_array_of_iarray(a) {
  return a;
}
EOF
fi

# Apply the missing iarray functions to runtime/wasm/array.wat
if ! grep -q "caml_iarray_of_array" "$BUILD_DIR/runtime/wasm/array.wat"; then
    echo "Adding iarray functions to runtime/wasm/array.wat"
    # Remove the last closing parenthesis
    sed -i '$ d' "$BUILD_DIR/runtime/wasm/array.wat"
    
    # Add the new functions and closing parenthesis
    cat >> "$BUILD_DIR/runtime/wasm/array.wat" << 'EOF'

   (func (export "caml_iarray_of_array")
      (param $a (ref eq)) (result (ref eq))
      (local.get $a))

   (func (export "caml_array_of_iarray")
      (param $a (ref eq)) (result (ref eq))
      (local.get $a))
)
EOF
fi

# Apply the missing floatarray_create_local function to runtime/js/array.js
if ! grep -q "caml_floatarray_create_local" "$BUILD_DIR/runtime/js/array.js"; then
    echo "Adding floatarray_create_local function to runtime/js/array.js"
    
    # Find the line with caml_floatarray_create and add the local version after it
    sed -i '/^function caml_floatarray_create(/a\
\
//Provides: caml_floatarray_create_local const (const)\
//Requires: caml_floatarray_create\
function caml_floatarray_create_local(x) {\
  return caml_floatarray_create(x);\
}' "$BUILD_DIR/runtime/js/array.js"
fi

# Apply the missing floatarray_create_local export to runtime/wasm/array.wat  
if ! grep -q "caml_floatarray_create_local" "$BUILD_DIR/runtime/wasm/array.wat"; then
    echo "Adding floatarray_create_local export to runtime/wasm/array.wat"
    
    # Add the export to the caml_floatarray_create function
    sed -i '/func \$caml_floatarray_create/a\
      (export "caml_floatarray_create_local")' "$BUILD_DIR/runtime/wasm/array.wat"
fi

echo "Patch fixes applied successfully!"