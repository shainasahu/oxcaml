#!/bin/bash

# Test script to demonstrate the js_of_ocaml fix
# This simulates the failure scenario and shows how to resolve it

set -e

echo "=== js_of_ocaml-compiler Fix Demonstration ==="
echo

# Create a test scenario
TEST_DIR="/tmp/js_of_ocaml_fix_test"
echo "1. Setting up test environment in $TEST_DIR"
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Download and extract js_of_ocaml source (simulating opam download)
echo "2. Downloading js_of_ocaml source..."
curl -sL "https://github.com/ocsigen/js_of_ocaml/archive/a8e8d2c1696a5fb3ddb4fe15495b1a8625a29b4b.tar.gz" | tar xz
JS_SRC_DIR="$TEST_DIR/js_of_ocaml-a8e8d2c1696a5fb3ddb4fe15495b1a8625a29b4b"

echo "3. Simulating patch failure scenario..."
# Verify that the required functions are missing (which would cause patch failure)
if grep -q "caml_iarray_of_array" "$JS_SRC_DIR/runtime/js/array.js"; then
    echo "   WARNING: iarray functions already present (test may not be accurate)"
fi

if grep -q "caml_floatarray_create_local" "$JS_SRC_DIR/runtime/js/array.js"; then
    echo "   WARNING: floatarray_create_local already present (test may not be accurate)"  
fi

echo "4. Applying fix script..."
/home/runner/work/oxcaml/oxcaml/fix-js-of-ocaml-patches.sh "$JS_SRC_DIR"

echo "5. Verifying fixes were applied..."
echo "   Checking for iarray functions in JS runtime:"
if grep -q "caml_iarray_of_array" "$JS_SRC_DIR/runtime/js/array.js"; then
    echo "   ✓ caml_iarray_of_array found"
else
    echo "   ✗ caml_iarray_of_array NOT found"
    exit 1
fi

if grep -q "caml_array_of_iarray" "$JS_SRC_DIR/runtime/js/array.js"; then
    echo "   ✓ caml_array_of_iarray found"
else
    echo "   ✗ caml_array_of_iarray NOT found" 
    exit 1
fi

echo "   Checking for floatarray_create_local in JS runtime:"
if grep -q "caml_floatarray_create_local" "$JS_SRC_DIR/runtime/js/array.js"; then
    echo "   ✓ caml_floatarray_create_local found in JS"
else
    echo "   ✗ caml_floatarray_create_local NOT found in JS"
    exit 1
fi

echo "   Checking for iarray functions in WASM runtime:"
if grep -q "caml_iarray_of_array" "$JS_SRC_DIR/runtime/wasm/array.wat"; then
    echo "   ✓ caml_iarray_of_array export found in WASM"
else
    echo "   ✗ caml_iarray_of_array export NOT found in WASM"
    exit 1
fi

echo "   Checking for floatarray_create_local in WASM runtime:"
if grep -q "caml_floatarray_create_local" "$JS_SRC_DIR/runtime/wasm/array.wat"; then
    echo "   ✓ caml_floatarray_create_local export found in WASM"
else
    echo "   ✗ caml_floatarray_create_local export NOT found in WASM"
    exit 1
fi

echo
echo "🎉 All fixes verified successfully!"
echo
echo "The fix script successfully added:"
echo "- caml_iarray_of_array and caml_array_of_iarray functions"
echo "- caml_floatarray_create_local function" 
echo "- All necessary WASM exports"
echo
echo "This resolves the patch application failures for js_of_ocaml-compiler.6.0.1+ox"
echo

# Clean up
rm -rf "$TEST_DIR"
echo "Test completed and cleaned up."