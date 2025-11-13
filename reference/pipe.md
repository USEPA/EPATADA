# Pipe Operator

The pipe operator (`%>%`) is a powerful tool for chaining operations in
a readable and concise manner. For detailed information, refer to
magrittr:pipe()

## Arguments

- lhs:

  A value or the magrittr placeholder, representing the initial input to
  the pipeline.

- rhs:

  A function call or expression that operates on `lhs`, using magrittr
  semantics.

## Value

The result of evaluating `rhs(lhs)`, enabling seamless chaining of
operations.
