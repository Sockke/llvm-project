add_mlir_dialect_library(MLIRAsyncTransforms
  AsyncParallelFor.cpp
  AsyncRuntimeRefCounting.cpp
  AsyncRuntimeRefCountingOpt.cpp
  AsyncToAsyncRuntime.cpp
  PassDetail.cpp

  ADDITIONAL_HEADER_DIRS
  ${MLIR_MAIN_INCLUDE_DIR}/mlir/Dialect/Async

  DEPENDS
  MLIRAsyncPassIncGen

  LINK_LIBS PUBLIC
  MLIRArithmetic
  MLIRAsync
  MLIRIR
  MLIRPass
  MLIRSCF
  MLIRSCFToControlFlow
  MLIRStandard
  MLIRTransforms
  MLIRTransformUtils
)
