# sort

In-place sorting on arrays.

## API

- `sort_int(arr)` — sort an integer array in ascending order
- `sort_by(arr, len, cmp)` — sort by a plain comparison function
- `sort_env(arr, len, env, cmp)` — sort by a plain function given `env`, what it needs (in place of a closure, which would be allocated and never freed)

Uses quicksort with insertion sort for small partitions.

## Dependencies

- array
- arith
