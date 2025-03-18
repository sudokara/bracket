find_package(Python REQUIRED COMPONENTS Interpreter)

# Define the path to your test script, llracket binary, and runtime.c
set(TEST_SCRIPT "${CMAKE_SOURCE_DIR}/tests/test_script.py")
set(LLRACKET_BINARY "${CMAKE_BINARY_DIR}/bin/llracket")
set(RUNTIME_FILE "${CMAKE_SOURCE_DIR}/tools/runtime/runtime.c")

if(NOT DEFINED LLVM_BIN_DIR)
    set(LLVM_BIN_DIR "/usr/local/bin/" CACHE PATH "Path to LLVM binary directory")
endif()

# Add a custom target for running tests
add_custom_target(
  check
  COMMAND ${Python_EXECUTABLE} ${TEST_SCRIPT} --llracket ${LLRACKET_BINARY}
          --runtime ${RUNTIME_FILE} --build-dir ${CMAKE_BINARY_DIR}
          --llvm-bin-dir ${LLVM_BIN_DIR}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests
  COMMENT "Running tests...")
