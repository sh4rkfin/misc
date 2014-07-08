Go package: util

export GOPATH={path to idea projects}/misc/src/go

To verify compilation
  go build

To run tests
  go test

To compile the tests for debugging with gdb
  go test -c -gcflags "-N -l"